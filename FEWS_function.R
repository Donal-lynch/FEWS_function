#' @param times: vector of the time (month) at which price observations were made
#' @param logprice: vector of logarithm of prices at the given time
#' @param id: vector of distinct identification number of consumer goods
#' @param weight: vector of expenditure weights used in the regressions
#' @param window_length: single number for length of windows of the data that regressions are fit on
#' @param splice_pos: The positon on which to splice the windows together.
#' This can be a number from 1 to window_length or any of c("window", "half","movement", "mean").
#' @param time_unit: Format of data ie. day, week, month, quarter, year. Can be left blank and the code can determine it
#' @param num_cores: Number of cores to use for parrallell computaion. Convention is parallel::detectCores()-1 on local machines
#' @param return_fe_list: Option to return the indexes from all FE windows


library(dplyr)
library(tidyr)
library(stringr)
library(MatrixModels)
library(doSNOW)
library(data.table)

FEWS <-  function(times, logprice, id, window_length, weight = NULL,
                  splice_pos = "mean", return_fe_list = FALSE,
                  time_unit = NULL, num_cores=NULL) {
  # Function to calculte the Fixed effects window splice price index from
  # a time series of prices
  # Arguments:
  #   See top of script for detials of each argument
  #
  # Returns:
  #   output - a list of 1 or 2 items, depeding on return_fe_list argument.
  #     fews_df - a dataframe of the FEWS index
  #   OPTIONAL RETURN:
  #     fe_list - a list of dataframes. Each dataframe relates to one window in
  #       the series. The dataframes contain the output coefficeints from the FE
  #       model, before the splicing takes place.

  timer <- Sys.time() # Get the current time

  # check arguments are all legit
  c(times, logprice, id, weight, window_length, splice_pos, time_unit) %=%
    check_inputs (times = times,
                  logprice = logprice,
                  id = id,
                  weight = weight,
                  window_length = window_length,
                  splice_pos = splice_pos,
                  time_unit = time_unit)


  # make a data frame from all the in inputs
  prices.df <- data.frame(times = times, logprice = logprice, weight = weight,
                          id = id)

  # It is essential that the data frame is sorted by date
  # use an if because sorting is slow, but testing is fast
  if (is.unsorted(prices.df$times)){
    prices.df <- prices.df [order(prices.df$times), ]
  }

  # Get the time_unit if use did not supply it. Otherwise, check that the
  # time unit is sensible
  time_unit <- get_time_unit (prices.df$times, time_unit = time_unit)

  if (is.null(time_unit)){
    cat("Returning Null as there as a problem with the time data")
    return (NULL)
  }

  # Impute missing times
  prices.df <- impute_time (prices.df, time_unit, verbose = TRUE)

  # Get the indexs of the start of each window
  window_st_days <- get_window_st_days (df = prices.df,
                                   time_unit = time_unit,
                                   window_length = window_length)

  num_windows <- length(window_st_days)
  cat("Number of windows:", num_windows, "\n")


  if (!is.null(num_cores)) {
    # Starting Parallel =======================================================
    cores <- num_cores
    cl <- makeSOCKcluster(cores)
    registerDoSNOW(cl)

    cat("Running regression on each window...\n")

    num_ops <- num_windows
    rqd_packs <- c("dplyr", "MatrixModels", "stringr")
    rqd_data <- c("FE_model", "lmfun", "get_win_dates")

    pb <- txtProgressBar(min = 1, max = num_ops, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress) # Start the PB
    fe_indexes <-
      foreach(i = 1:num_ops,
              .packages = rqd_packs,
              .options.snow = opts,
              .combine = "cbind",
              .export = rqd_data) %dopar% {
                FE_model (st_date = window_st_days[i],
                          df = prices.df,
                          time_unit = time_unit,
                          window_length = window_length)
              }
    close(pb) # End The PB
    stopCluster(cl)
    # Ending Parallel ========================================================
  } else {
    # Non parallell code
    fe_indexes <- lapply(X = window_st_days,
                    FUN = FE_model,
                    df = prices.df,
                    time_unit = time_unit,
                    window_length = window_length)

    # this list needs to be coerced into a df to match the foreach output
    # First convert each individual item to a df
    fe_indexes <- lapply(fe_indexes, FUN = as.data.frame)
    # Now convert the whole thing:
    fe_indexes <- as.data.frame(fe_indexes)
  }


  # Convert back fom log price
  fe_indexes <- exp (fe_indexes)
  # Add in a row of 1's for the baseline month
  fe_indexes <- rbind (rep(1, each = ncol(fe_indexes)), fe_indexes)

  # fe_list is a list of each window's fixed effects index
  fe_list <- get_fe_list (fe_indexes = fe_indexes,
                    window_st_days = window_st_days,
                    time_unit = time_unit,
                    window_length = window_length)

  # Make the FEWS with from the fe_list
  fews_df <- get_fews_df (fe_list = fe_list,
                      window_length = window_length,
                      splice_pos = splice_pos)


  cat("\nFinished. It took", round(Sys.time() - timer, 2), "seconds\n\n\n")

  if (return_fe_list) {
  # Wrap the output in a list
  output <- list(fe_list = fe_list, fews_df = fews_df)
  return(output)
  } else {
    # Wrap the output in a list of one item. This is so that the return type is
    # same either way
    output <- list(fews_df = fews_df)
  }
}



#### HELPER FUNCTIONS ####

"%!in%" <- function(x, y) !("%in%" (x, y))


"%=%" <- function(lhs, rhs) {
  # Special equals to assign multiple enteries at once - like python
  # stolen from here:
  # https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
  # %=% is used opposed to := because := is used by data.table package
  # Example usage:
  # c(a, b) := functionReturningTwoValues()

  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir = frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, "formula"))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir = frame)
  return(invisible(NULL))
}



check_inputs <- function (times = times, logprice = logprice, id = id,
                          weight = weight, window_length = window_length,
                          splice_pos = splice_pos, time_unit = time_unit) {
  # Function to confirm that all inputs are correct shape and class
  # Returns all of the arguments, though some may have had the class modified

  if (missing(weight) | is.null(weight)){
    weight <- rep(1, length(times))
    cat("No weighting assigned...All weights set to 1\n")
  }

  # Returns all inputs, but some may be modified to the correct data type

  if (length(times) != length(logprice)){
    stop("times and logprice should be vectors of the same length")
  }else if (length(times) != length(id)){
    stop("times and id should be vectors of the same length")
  }else if (length(times) != length(weight)){
    stop("times and weight should be vectors of the same length")
  }else if (length(window_length) != 1 | class(window_length) != "numeric"){
    stop("window_length should be a single number")
  } else if (!(class(times) %in% c("zoo", "Date", "numeric", "integer"))){
    stop (paste("The price times class is: ",
                class(times),
                "\n The class must be a date or numeric"))
  }


  time_units <- c("day", "week", "month", "quarter", "year")

  if (!(missing(time_unit) | is.null(time_unit))) {
    time_unit <- tolower(time_unit)
    if (!(time_unit %in% time_units)) {
      stop("time_unit of ", time_unit,
           " is not a valid option. \n You must input one of: ",
           paste(time_units, collapse = ", "))
    }
  }


  splice_pos_all <- c("window", "half", "movement", "mean")
  if (!( is.numeric(splice_pos) | is.integer(splice_pos))) {
    # splice_pos not a number
    # convert to lower case, to help user error
    splice_pos <- tolower(splice_pos)
    if (!(splice_pos %in% splice_pos_all)) {
      stop("splice_pos of ", time_unit,
           " is not a valid option. \n You must input a number or one of: ",
           paste(splice_pos_all, collapse = ", "))
    }
    # Theses two options all easy to convert to a number:
    if (splice_pos ==  "window")  splice_pos <- 2
    if (splice_pos == "half") splice_pos <- ceiling(window_length / 2)

  } else{
    # splice_pos is a numeric or integer
    if (splice_pos %% 1 != 0){
      stop ("Splice position must be a whole number")
    } else if (splice_pos > window_length){
      stop ("Splice position must be less than window length")
    }
  }


  if (class(times) == "numeric" | class(times) == "integer") {
    times <- as.Date(times, origin = "1970-01-01")
  }

  return (list(times = times, logprice = logprice, id = id, weight = weight,
               window_length = window_length, splice_pos = splice_pos,
               time_unit = time_unit))
}



get_time_unit <- function (price_times, time_unit = NULL) {
  # Calculates the frequency of the price observations.
  # Confirm that the frequencey matches the user input frequency
  # Args:
  #   price_times: The times data for the price observations
  #     NOTE: ***These should already be sorted***
  #   time_unit: The Users expected time frequencey
  #
  # Returns:
  #   time_unit - as determined by this func

  # Get the time steps between every time entry
  freq <- as.numeric(unique(price_times)) %>%
    diff() %>%
    mean()

  test_freq <- function(freq, num)
    # Tests if a a number is within 20% of a test number
    if (freq > num * 0.8 & freq < num * 2){
      return (TRUE)
    } else (
      return (FALSE)
    )

  if (test_freq(freq, 1)) {
    time_unit.determined <- "day"
  } else if (test_freq(freq, 7)) {
    time_unit.determined <- "week"
  } else if (test_freq(freq, 30.5)) {
    time_unit.determined <- "month"
  } else if (test_freq(freq, 91.25)) {
    time_unit.determined <- "quarter"
  } else if (test_freq(freq, 365)) {
    time_unit.determined <- "year"
  } else {
    return(NULL)
  }

  # Only check if they match if user has entered a time unit
  if (!(missing(time_unit) | is.null(time_unit))) {
    if (time_unit.determined != time_unit) {
      stop (paste("User expects dates to be", time_unit,
                  ",but they look like", time_unit.determined))
    }
  }

  return (time_unit.determined)

}

impute_time <- function(dframe, time_unit, verbose = TRUE) {
  # Imputes missing time enteries by copying the previous available data
  #
  # Args:
  #   dframe: A dataframe with the columns:"price_date" "rid" "price"
  #   time_unit: a string of any of:
  #             "day", "week", "month", "quarter", "year"
  #   verbose: If TRUE, prints sample number of imputed days. Default is TRUE.
  #
  # Returns:
  #   dframe: a dataframe as passed, but with missing times imputed


  # Error handling:
  if (!(c("times", "logprice", "weight", "id") %in%  colnames(dframe) %>%
        all())) {
    stop (paste("The data frame column names are: ",
                colnames(dframe) %>% paste(collapse = ", "),
                "\n The correct names should be: times,logprice, weight, id"))
  }

  # Make a sequence of all dates and compare it to the unique vals of the
  # Date column

  unique_dates <- (unique(dframe$times))
  # order the dates
  unique_dates <- unique_dates[order(unique_dates)]

  all_dates <- seq.Date(from = unique_dates[1],
                       to = unique_dates[length(unique_dates)],
                       by = time_unit)

  if (length(all_dates) > length(unique_dates)){
    cat("There are", length(all_dates) - length(unique_dates),
        "missing time values. This represents",
        (length(all_dates) - length(unique_dates)) / length(all_dates) * 100,
        "% of all dates. These will be imputed.\n")
  }else if (length(all_dates) < length(unique_dates))  {
    # There are some strange dates in the time series
    stop("There are more dates than unique dates. Is your time unit correct?")
  }else if (all(all_dates == unique_dates)) {
    cat ("All dates accounted for. No need to impute.\n")
    return (dframe) # No need to perform further computation
  }

  # Get the dates of all the missing days
  missing_times <- all_dates[all_dates %!in% dframe$times]

  # loop through all of the missing days and use the previous day's values
  for (i in 1:length(missing_times)){
    # copy the the time prior to the missing time
    prev_time <- dframe[dframe$times == missing_times[i] - 1, ]
    # Update the new time to have the same
    prev_time$times <- missing_times[i]
    dframe <- rbind(dframe, prev_time)
  }
  # Return the sorted data frame
  dframe[order(dframe$times), ]

}



get_window_st_days <- function (dframe, time_unit,  window_length) {
  # Calculate a sequence of dates corrosponding to the starts of each window
  # Args:
  #     dframe - data frame with times colum
  #     time_unit - one of "day", "week", "month", "quarter" or "year"
  #     window_length - the number of time_units per window
  # Returns:
  #     A date sequence corrosponding to the start date of each window

  num_windows <- length(unique(dframe$times)) - window_length + 1

  if (num_windows < 0) {
    stop ("window lenght of ", window_length,
          " is longer then the number time periods: ",
          length(unique(dframe$times)))
  }

  seq.Date(from = min(dframe$times),
           by = time_unit,
           length.out = num_windows)
}


get_win_dates <- function(st_date, time_unit, window_length){
  # Calculate a sequence of dates corrosponding to the dates in a window which
  # starts at st_date
  # Args:
  #     st_date - a date, corrospondin to the first date of the window
  #     time_unit - one of "day", "week", "month", "quarter" or "year"
  #     window_length - the number of time_units per window
  # Returns:
  #     A date sequence corrosponding to each date in the window

  seq(st_date, by = time_unit, length.out = window_length)
}



FE_model <- function(st_date, dframe, time_unit, window_length ) {
  # Run linear regression on every window and extract the coefficients
  # for the time values only
  # Arguments:
  #     st_date - a date indicating the start day of the window
  #     dframe - a data frame
  # weight
  # time_unit
  # window_length



  # Get the dates of each day in this window
  win_dates <- get_win_dates(st_date, time_unit, window_length)
  # Subset dframe by the dates in this window
  dframe_win <- filter(dframe, times %in% win_dates)


  # Run the linear model and extract the coefficients from the regression
  lm_coefs_all <- lmfun(dframe_win) %>%
    coef()


  # Choose only the coefficients for month (disregard the intercept, and the
  # coefficients for the product id). This is done based on the name of the
  # columns
  fe_coefs <- lm_coefs_all[!is.na(str_extract(names(lm_coefs_all),
                                              "timefact\\d+"))]

  # fe_coefs should equal the length of window minus the 1. The reason it is -1
  # is due to the fact that 1 coefficient is sacraficed to intercept
  if ( (length(fe_coefs) + 1) != window_length){
    # TODO Should this be an error?
    cat("The number of time coefficients does not match the window lenght?\n")
    cat("Is there a day missing?\n")
  }

  return (fe_coefs)
}


lmfun <- function(dframe){
  # Regress the dates and id's as factors against logprice
  # Arguments
  #   dframe - a dframe frame with logprices, weights and id's
  # Returns
  #   modelOutput - the output of the linear model


  if (all(dframe$weight == 1)){
    weight <- NULL
  } else {
    weight <- dframe$weight
  }


  # Refactor the dates here. Otherwise columns are created in the regression
  # matrix with all zeros, corrosponding to dates not in the current window
  dframe$timefact <- factor(dframe$times)
  dframe$IDfact <- as.factor(dframe$id)

  # glm uses the alpabetically first id as the reference. However, if this
  # value doesn't appear in the then all other values are being compared
  # to a numer that is essentially zero. Hence you can get crazily high numbers
  # like indexes of 10^50 from normal looking data
  dframe <- within(dframe, IDfact <- relevel(IDfact,
                                         ref = as.character(dframe$IDfact[1])))


  # Regression doesn't work if there is only 1 item in the time window.
  if (nlevels(dframe$IDfact) == 1) {
    glm_formula <- dframe$logprice ~ dframe$timefact
  } else {
    glm_formula <- dframe$logprice ~ dframe$timefact + dframe$IDfact
  }

  # use the try catch as glm seems more robust, but glm4 is more
  # memory efficient with large dataframes (due to sparse = T)

  model_output <- tryCatch({
    glm4(glm_formula, weights = weight,
         sparse = TRUE)
  }, warning = function(w) {
    # cat("glm4 threw a warning, using glm\n")
    glm(glm_formula, weights = weight)
  }, error = function(e) {
    # cat("glm4 threw an error, using glm\n")
    glm(glm_formula, weights = weight)
  }, finally = {
    # some finally code here
  })

  return(model_output)
}


get_fe_list <- function (fe_indexes, window_st_days, time_unit, window_length) {
  # Takes the lm model outputs and makes a list with of dataframes containing
  # Regression coefficients (i.e. indexes) and correct dates
  #
  # Arguments:
  #     FEindxes - is a matrix of dim(window_length, num_windows) containing all
  #         of the output coefficients from the lm. all values converted back
  #         from log and 1 append as the first month in each case
  #     window_st_days - a sequence of the start date of each window
  #     time_unit -  one of "day", "week", "month", "quarter" or "year"
  # Returns:
  #     fe_list - a list of dataframes. Lenght is number of windows.
  #         nrow of each data frame is window length.
  #         The data frames contain:
  #             FE indexes for each window
  #             the window id (numeric)
  #             the dates

  fe_list <- list()

  # Loop over each window and construct the DF
  for (i in 1:ncol(fe_indexes)){
    fe_list[[i]] <- data.frame(
      price_date = get_win_dates(window_st_days[i],
                               time_unit = time_unit,
                               window_length = window_length),
      fe_indexes = fe_indexes[, i],
      window_id = i)

    # row names are junk due to times as factors and rbing rows earlier
    rownames(fe_list[[i]]) <- c()
  }
  return (fe_list)
}



get_fews_df <- function (fe_list, window_length, splice_pos) {
  # Splice the windows together to produce a continous timeseries of indexes
  # Arguments:
  #   fe_list - output from get_fe_list. See that  function for details
  #   window_length -  window_length - the number of time_units per window
  #   splice_pos - the index on whcih to splice the windows. Can also be
  #       'mean' whcih is the geo mean of all possible values of splice_pos


  # Initialise the df. As the first window is unspliced, copy it exactly
  fews <- fe_list[[1]]

  # Loop over the windows, starting at the second window.
  # Cannot splice on the first
  for (i in 2: length(fe_list)){
    old_window <- fe_list[[i - 1]]$fe_indexes
    new_window <- fe_list[[i]]$fe_indexes

    # Get the previous FEWS index value
    old_fews <- fews$fe_indexes[nrow(fews)]

    update_factor <- splice_update (old_window,
                                   new_window,
                                   splice_pos = splice_pos)

    # Get the "new" FEWS index value
    new_fews <- old_fews * update_factor

    # buid up the new row
    # price_date is the last date in the current window
    last_date <- fe_list[[i]]$price_date[nrow(fe_list[[i]])]

    new_row <- data.frame(price_date = last_date,
                         fe_indexes = new_fews,
                         window_id = i)

    fews <- rbind(fews, new_row)
  }

  return (fews)
}



gm_mean <- function(x, na.rm=TRUE){
  # Calculate the geometric mean of a vector of numbers
  # Stolen from here:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

  # This is a safer implementation than using PRODUCT () as floating point
  # errors are vrey likely when using PRODUCT () for many large or small numbers
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}


splice_update <- function (win_old, win_new, splice_pos){
  # Calculate the update factor for splicing two windows
  # Arguments
  #   win_old - the indexes for the previous window
  #   win_new - the indexes for the current window
  #   splice_pos - an integer for the time period on which to splice
  #     the windows. Can also be 'mean' or 'window' for differnt splice types
  #
  # Returns
  #   update_factor - the splice update factor

  stopifnot(length(win_old) == length(win_new))
  w <- length(win_old)

  # As the old window starts 1 entery earlier in time than the new window,
  # adding a NaN to the start of the new window makes the indexes of the 2
  # windows align. This value is never used in the calculations
  win_new <- c(NaN, win_new)

  # Variable names chosen to follow notation in IndexNumR Package
  # https://cran.r-project.org/web/packages/IndexNumR/vignettes/indexnumr.html
  Pw_new <- win_new[w]
  Pw1_new <- win_new[w + 1]
  Pw_old  <- win_old[w]

  if (splice_pos == "mean") {
    t_accum <- c() # Accumulator for the t loop
    for (t in seq(1, w - 1)) {
      Pt1_new <- win_new[t + 1]
      Pt1_old <- win_old[t + 1]
      t_accum <- c(t_accum, (Pw1_new / Pt1_new) / (Pw_old / Pt1_old))
    }
    update_factor <- gm_mean(t_accum)
  }else if (splice_pos == "movement") {
    update_factor <- (Pw1_new/Pw_new)
  } else {
    Pn_new <- win_new[splice_pos]
    Pn_old <- win_old[splice_pos]
    update_factor <- (Pw1_new / Pn_new) / (Pw_old / Pn_old )
  }

  return (update_factor)
}
