#' @param times: vector of the time (month) at which price observations were made
#' @param logprice: vector of logarithm of prices at the given time
#' @param ID: vector of distinct identification number of consumer goods
#' @param weight: vector of expenditure weights used in the regressions
#' @param windowLength: single number for length of windows of the data thatregressions are fit on
#' @param splicePos: The positon on which to splice the windows together. This can be a numnber 1 - window length or 'mean' for a mean splice
#' @param timeUnit: Format of data ie. day, week, month, quarter, year. Can be left blank and the code can determine it
#' @param run_summaries: section of code to run a summary, recommend off while developing
#' @param numCores: Number of cores to use for parrallell computaion. Convention is parallel::detectCores()-1 on local machines

# library(PriceStatsNZ)
library(dplyr)
library(tidyr)
library(stringr)
library(MatrixModels)
library(doSNOW)
library(data.table)


#### HELPER FUNCTIONS ####

'%!in%' <- function(x,y)!('%in%'(x,y))


'%=%' <- function(lhs, rhs) {
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
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
}



check_inputs <- function (times = times, logprice = logprice, ID = ID,
                          weight = weight, windowLength = windowLength,
                          splicePos = splicePos, timeUnit = timeUnit) {
  # Function to confirm that all inputs are correct shape and class
  # Returns all of the arguments, though some may have had the class modified

  if(missing(weight)){
    weight <- rep(1,length(times))
    cat('No weighting assigned...All weights set to 1\n')
  }

  # Returns all inputs, but some may be modified to the correct data type

  if(length(times) != length(logprice)){
    stop("times and logprice should be vectors of the same length")
  }else if(length(times) != length(ID)){
    stop("times and ID should be vectors of the same length")
  }else if(length(times) != length(weight)){
    stop("times and weight should be vectors of the same length")
  }else if(length(windowLength) != 1 | class(windowLength) != "numeric"){
    stop("windowLength should be a single number")
  } else if(!(class(times) %in% c("zoo", "Date", "numeric", "integer"))){
    stop (paste("The price times class is: ",
                class(times),
                "\n The class must be a date or numeric"))
  }


  timeUnits <- c("day", "week", "month", "quarter", "year")

  if (!(missing(timeUnit) | is.null(timeUnit))) {
    timeUnit <- tolower(timeUnit)
    if (!(timeUnit %in% timeUnits)) {
      stop("timeUnit of ", timeUnit,
           " is not a valid option. \n You must input one of: ",
           paste(timeUnits, collapse = ", "))
    }
  }


  splicePosS = c("window", "half","end", "mean", "movement")
  if (!( is.numeric(splicePos) | is.integer(splicePos))) {
    # SplicePos not a number
    # convert to lower case, to help user error
    splicePos <- tolower(splicePos)
    if (!(splicePos %in% splicePosS)) {
      stop("splicePos of ", timeUnit,
           " is not a valid option. \n You must input a number or one of: ",
           paste(splicePosS, collapse = ", "))
    }
    # Theses two options all easy to convert to a number:
    if (splicePos ==  "window")  splicePos <- 2
    if (splicePos == "half") splicePos <- floor(mean(c(1,windowLength)))
    if (splicePos == "end") splicePos <- windowLength

  } else{
    # splicePos is a numeric or integer
    if(splicePos%%1 !=0){
      stop ("Splice position must be a whole number")
    }
  }


  if (class(times) == 'numeric' | class(times) == "integer") {
    times <- as.Date(times, origin = "1970-01-01")
  }

  return (list(times = times, logprice = logprice, ID = ID, weight = weight,
               windowLength = windowLength, splicePos = splicePos,
               timeUnit = timeUnit))
}



GetTimeUnit <- function (priceTimes, timeUnit = NULL) {
  # Calculates the frequency of the price observations.
  # Confirm that the frequencey matches the expected frequency based on user input
  # Args:
  #   priceTimes: The times data for the price observations
  #     NOTE: ***These should already be sorted***
  #   timeUnit: The Users expected time frequencey
  #
  # Returns:
  #   timeUnit - as determined by this func

  # Get the time steps between every time entry
  freq <- as.numeric(unique(priceTimes)) %>%
    diff() %>%
    mean()

  freqTest <- function(freq, num)
    # Tests if a a number is within 20% of a test number
    if (freq>num*0.8 & freq<num*2){
      return (TRUE)
    } else (
      return (FALSE)
    )

  if (freqTest(freq, 1)) {
    timeUnit.determined <- "day"
  } else if (freqTest(freq, 7)) {
    timeUnit.determined <- "week"
  } else if (freqTest(freq, 30.5)) {
    timeUnit.determined <- "month"
  } else if (freqTest(freq, 91.25)) {
    timeUnit.determined <- "quarter"
  } else if (freqTest(freq, 365)) {
    timeUnit.determined <- "year"
  } else {
    return(NULL)
  }

  # Only check if they match if user has entered a time unit
  if (!(missing(timeUnit) | is.null(timeUnit))) {
    if (timeUnit.determined != timeUnit) {
      stop (paste("User expects dates to be", timeUnit, ',but they look like',
                  timeUnit.determined))
    }
  }

  return (timeUnit.determined)

}

TimeImpute <- function(dframe, timeUnit, verbose = TRUE) {
  # Imputes missing time enteries by copying the previous available data
  #
  # Args:
  #   dframe: A dataframe with the columns:"price_date" "rid" "price"
  #   timeUnit: a string of any of:
  #             "day", "week", "month", "quarter", "year"
  #   verbose: If TRUE, prints sample number of imputed days. Default is TRUE.
  #
  # Returns:
  #   dframe: a dataframe as passed, but with missing times imputed


  # Error handling:
  if (!(c("times","logprice", "weight", "ID") %in%  colnames(dframe) %>%
        all())) {
    stop (paste("The data frame column names are: ",
                colnames(dframe)%>%paste(collapse = ", "),
                "\n The correct names should be: times,logprice, weight, ID"))
  }

  # Make a sequence of all dates and compare it to the unique vals of the
  # Date column

  uniqueDates <- (unique(dframe$times))
  uniqueDates <- uniqueDates[order(uniqueDates)]

  allDates <- seq.Date(from = uniqueDates[1],
                       to = uniqueDates[length(uniqueDates)],
                       by = timeUnit)

  if (length(allDates) > length(uniqueDates)){
    cat("There are", length(allDates) - length(uniqueDates),
        "missing time values. This represents",
        (length(allDates) - length(uniqueDates))/length(allDates) * 100,
        "% of all dates. These will be imputed.\n")
  }else if (length(allDates) < length(uniqueDates))  {
    # There are some strange dates in the time series
    stop("There are more dates than unique dates. Is your time unit correct?")
  }else if (all(allDates == uniqueDates)) {
    cat ("All dates accounted for. No need to impute.\n")
    return (dframe) # No need to perform further computation
  }

  # Get the dates of all the missing days
  missing_times <- allDates[allDates %!in% dframe$times]

  # loop through all of the missing days and use the previous day's values
  for (i in 1:length(missing_times)){
    # copy the the time prior to the missing time
    prevTime <- dframe[dframe$times==missing_times[i]-1,]
    # Update the new time to have the same
    prevTime$times <- missing_times[i]
    dframe <- rbind(dframe,prevTime)
  }
  # Return the sorted data frame
  dframe[order(dframe$times),]

}



GetWindowStarts <- function (dframe, timeUnit,  windowLength) {
  # Calculate a sequence of dates corrosponding to the starts of each window
  # Args:
  #     dframe - data frame with times colum
  #     timeUnit - one of "day", "week", "month", "quarter" or "year"
  #     windowLength - the number of timeunits per window
  # Returns:
  #     A date sequence corrosponding to the start date of each window

  numWins <- length(unique(dframe$times)) - windowLength + 1

  seq.Date(from = min(dframe$times),
           by = timeUnit,
           length.out = numWins)
}


getWinDates <- function(stDate, timeUnit, windowLength){
  # Calculate a sequence of dates corrosponding to the dates in a window which
  # starts at stDate
  # Args:
  #     stDate - a date, corrospondin to the first date of the window
  #     timeUnit - one of "day", "week", "month", "quarter" or "year"
  #     windowLength - the number of timeunits per window
  # Returns:
  #     A date sequence corrosponding to each date in the window

  seq(stDate, by = timeUnit, length.out = windowLength)
}



FE_model <- function(stDate, dframe, timeUnit, windowLength ) {
  # Run linear regression on every window and extract the coefficients for the time
  # values only
  # Arguments:
  #     stDate - a sequence of dates indicating the start days of the windows
  #     dframe - a data frame
  # weight
  # timeUnit
  # windowLength


  WinDates <- getWinDates(stDate, timeUnit, windowLength)

  dframeWin <- filter(dframe, times %in% WinDates)


  # Run the linear model and extract the coefficients from the regression
  coefs <- lmfun(dframeWin) %>%
    coef()

  # Choose only the coefficients for month (disregard the intercept, and the
  # coefficients for the product id). This is done based on the name of the
  # columns
  coef_id <- coefs[!is.na(str_extract(names(coefs),'timefact\\d+'))]

  # coef_id should equal the length of window minus the 1. The reason it is -1
  # is due to the fact that 1 coefficient is sacraficed to intercept
  if((length(coef_id)+1)!=windowLength){
    # TODO Should this be an error?
    cat('The number of timefact coefficients does not match the window lenght???\n')
    cat('Is there a day missing?\n')
  }

  return (coef_id)
}


lmfun <- function(dframe){
  # Regress the dates and ID's as factors against logprice
  # Arguments
  #   dframe - a dframe frame with logprices, weights and ID's
  # Returns
  #   modelOutput - the output of the linear model

  if (all(dframe$weight==1)){
    weight <- NULL
  }

  # Refactor the dates here. Otherwise columns are created in the regression
  # matrix with all zeros, corrosponding to dates not in the current window
  dframe$timefact <- factor(dframe$times)
  dframe$IDfact <- as.factor(dframe$ID)

  # glm uses the alpabetically first ID as the reference. However, if this
  # value doesn't appear in the then all other values are being compared
  # to a numer that is essentially zero. Hence you can get crazily high numbers
  # like indexes of 10^50 from normal looking data
  dframe <- within(dframe, IDfact <- relevel(IDfact,
                                         ref = as.character(dframe$IDfact[1])))


  # Regression doesn't work if there is only 1 item in the time window.
  if (nlevels(dframe$IDfact) == 1) {
    glmFormula <- dframe$logprice ~ dframe$timefact
  } else {
    glmFormula <- dframe$logprice ~ dframe$timefact + dframe$IDfact
  }

  # use the try catch as glm seems more robust, but glm4 is more memory efficient
  # with large dataframes (due to sparse = T)

  modelOutput <- tryCatch({
    glm4(glmFormula,
         weights = dframe$weight,
         sparse=TRUE)
  }, warning = function(w) {
    # warning-handler-code
  }, error = function(e) {
    modelOutput <- glm(glmFormula,
                 weights = dframe$weight)
  }, finally = {
    # cleanup-code
  })

  return(modelOutput)
}


GetFE <- function (FEindexes, windowStarts, timeUnit, windowLength) {
  # Takes the lm model outputs and makes a list with of dataframes containing
  # Regression coefficients (i.e. indexes) and correct dates
  #
  # Arguments:
  #     FEindxes - is a matrix of dim(windowLength, numWindows) containing all
  #         of the output coefficients from the lm. all values converted back
  #         from log and 1 append as the first month in each case
  #     windowStarts - a sequence of the start date of each window
  #     timeUnit -  one of "day", "week", "month", "quarter" or "year"
  # Returns:
  #     FE.list - a list of dataframes. Lenght is number of windows.
  #         nrow of each data frame is window length.
  #         The data frames contain:
  #             FE indexes for each window
  #             the window id (numeric)
  #             the dates

  FE.list <- list()

  # Loop over each window and construct the DF
  for (i in 1:ncol(FEindexes)){
    FE.list[[i]] <- data.frame(
      price_date = getWinDates(windowStarts[i],
                               timeUnit = timeUnit,
                               windowLength = windowLength),
      FEindexes = FEindexes[,i],
      window_id = i)

    # row names are junk due to times as factors and rbing rows earlier
    rownames(FE.list[[i]]) <- c()
  }
  return (FE.list)
}



GetFEWS <- function (FE.list, windowLength, splicePos) {
  # Splice the windows together to produce a continous timeseries of indexes
  # Arguments:
  #   FE.list - output from GetFE. See that  function for details
  #   windowLength -  windowLength - the number of timeunits per window
  #   splicePos - the index on whcih to splice the windows. Can also be
  #       'mean' whcih is the geometric mean of all possible values of splicePos


  # Initialise the df. As the first window is unspliced, copy it exactly
  FEWS <- FE.list[[1]]

  # Loop over the windows, starting at the second window.
  # Cannot splice on the first
  for(i in 2: length(FE.list)){
    old_window <-FE.list[[i-1]]$FEindexes
    new_window <- FE.list[[i]]$FEindexes

    # Get the previous FEWS index value
    oldFEWS <- FEWS$FEindexes[nrow(FEWS)]

    updateFactor <- splice_update (old_window,
                                   new_window,
                                   splicePos = splicePos)

    # Get the "new" FEWS index value
    newFEWS <- oldFEWS * updateFactor

    # buid up the new row
    # price_date is the last date in the current window
    newRow <- data.frame(price_date = FE.list[[i]]$price_date[nrow(FE.list[[i]])],
                         FEindexes = newFEWS,
                         window_id = i)

    FEWS <- rbind(FEWS, newRow)
  }

  return (FEWS)
}



gm_mean <- function(x, na.rm=TRUE){
  # Calculate the geometric mean of a vector of numbers
  # Stolen from here:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

  # This is a safer implementation than using PRODUCT () as floating point
  # errors are vrey likely when using PRODUCT () for many large or small numbers
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


splice_update <- function (win_old, win_new, splicePos){
  # Calculate the update factor for splicing two windows
  # Arguments
  #   win_old - the indexes for the previous window
  #   win_new - the indexes for the current window
  #   splicePos - the index on whcih to splice the windows. Can also be
  #       'mean' whcih is the geometric mean of all possible values of splicePos
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
  Pw1_new <- win_new[w+1]
  Pw_old  <- win_old[w]

  if (splicePos == "mean") {
    t_accum <- c() # Accumulator for the t loop
    for (t in seq(1,w-1)) {
      Pt1_new <- win_new[t+1]
      Pt1_old <- win_old[t+1]
      t_accum <- c(t_accum, (Pw1_new/Pt1_new)/(Pw_old/Pt1_old))
    }
    update_factor <- gm_mean(t_accum)
  } else if(splicePos == "movement") {
    update_factor <- (Pw1_new/Pw_new)
  }else {
    Pn_new <- win_new[splicePos]
    Pn_old <- win_old[splicePos]
    # update_factor <- (Pw1_new/Pw_new) # Movement splice - this works
    update_factor <- (Pw1_new/Pn_new) / (Pw_old/Pn_old ) # Window
  }

  return (update_factor)
}



TextSender <- function () {
  library("RPushbullet")
  library("httr")
  pbPost("note", paste0("FEWS run"),
         content(GET("http://numbersapi.com/random/trivia")))
}


#### Main Function ####

FEWS <-  function(times, logprice, ID, weight, windowLength, splicePos = 'mean',
                  timeUnit = NULL, numCores=NULL) {

  timer <- Sys.time() # Get the current time


  # check arguments are all legit
  c(times, logprice, ID, weight, windowLength, splicePos, timeUnit) %=%
    check_inputs (times = times,
                  logprice = logprice,
                  ID = ID,
                  weight = weight,
                  windowLength = windowLength,
                  splicePos = splicePos,
                  timeUnit = timeUnit)


  # make a data frame from all the in inputs
  prices.df <- data.frame(times = times, logprice = logprice, weight = weight,
                          ID = ID)

  # It is essential that the data frame is sorted by date
  # use an if because sorting is slow, but testing is fast
  if (is.unsorted(prices.df$times)){
    prices.df <- prices.df [order(prices.df$times), ]
  }

  # Get the timeUnit if use did not supply it. Otherwise, check that the
  # time unit is sensible
  timeUnit <- GetTimeUnit (prices.df$times, timeUnit = timeUnit)

  if (is.null(timeUnit)){
    cat("Returning Null as there as a problem with the time data")
    return (NULL)
  }

  # Impute missing times
  prices.df <- TimeImpute (prices.df, timeUnit, verbose = TRUE)

  # Get the indexs of the start of each window
  windowStarts <- GetWindowStarts (df = prices.df,
                                   timeUnit = timeUnit,
                                   windowLength = windowLength)

  numWindows <- length(windowStarts)
  cat('Number of windows:',numWindows,'\n')


  if (!is.null(numCores)) {
    # Starting Parallel =======================================================
    cores <- numCores
    cl <- makeSOCKcluster(cores)
    registerDoSNOW(cl)

    cat('Running regression on each window...\n')

    numOps <- numWindows
    rqdPacks <- c("dplyr", "MatrixModels", "stringr")
    rqdData <- c("FE_model", "lmfun", "getWinDates")

    pb <- txtProgressBar(min=1, max=numOps, style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress) # Start the PB
    coefs <-
      foreach(i=1:numOps, .packages = rqdPacks, .options.snow = opts,
              .combine = "cbind", .export = rqdData) %dopar% {
                FE_model (stDate = windowStarts[i],
                          df = prices.df,
                          timeUnit = timeUnit,
                          windowLength = windowLength)
              }
    close(pb) # End The PB
    stopCluster(cl)
    # Ending Parallel ========================================================
  } else {
    # Non parallell code
    coefs <- lapply(X = windowStarts,
                    FUN = FE_model,
                    df = prices.df,
                    timeUnit = timeUnit,
                    windowLength = windowLength)

    # this list needs to be coerced into a df to match the foreach output
    # First convert each individual item to a df
    coefs <- lapply(coefs, FUN = as.data.frame)
    # Now convert the whole thing:
    coefs <- as.data.frame(coefs)
  }


  # Convert back fom log price
  coefs <- exp (coefs)
  # Add in a row of 1's for the baseline month. This is the FE indexes
  FEindexes <- rbind (rep(1, each = ncol(coefs)), coefs)

  # FE.list is a list of each window's fixed effects index
  FE.list <- GetFE (FEindexes = FEindexes,
                    windowStarts = windowStarts,
                    timeUnit = timeUnit,
                    windowLength = windowLength)

  # Make the FEWS with from the FE.list
  FEWS.df <- GetFEWS (FE.list = FE.list,
                      windowLength = windowLength,
                      splicePos = splicePos)


  # Wrap the output in a list
  output <- list(FE.list=FE.list, FEWS.df=FEWS.df)
  cat('\nFinished. It took',round(Sys.time() - timer,2),'seconds\n\n\n')
  return(output)
}


