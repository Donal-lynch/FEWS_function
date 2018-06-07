
# load the fews function
source("FEWS_function.R")
# read the csv into memory
synt_data <- read.csv("SYNTHETIC.csv")

# Build the list to be passed to the FEWS function
arg_list <- list(times = synt_data$month_num,
                 logprice = log(synt_data$value / synt_data$quantity),
                 id = synt_data$prodid_num,
                 weight = synt_data$value,
                 window_length = 5,
                 splice_pos = "mean",
                 return_fe_list = FALSE,
                 num_cores = NULL)


fews_output <- FEWS(arg_list)

# FEWS retuns a list, but as 'return_fe_list = FALSE' it is a list of 1 item
# extract the dataframe
fews_output <- fews_output$fews_df

# plot the fews index against the month number
plot(fews_output$price_date,
     fews_output$fe_indexes,
     type = "l")



