library(dplyr)

# load the fews function
source("FEWS_function.R")
# read the csv into memory
synt_data <- read.csv("Syntethic data.csv")

# A little bit of tidying up of the data. In many cases the same item is
# priced multiple times on the same day. These are just all averaged together
# `gm_mean ()` is geometric mean
synt_data_tidy <- synt_data %>%
  mutate(price = value / quantity) %>%
  select(month_num, prodid_num, value, price) %>%
  group_by(month_num, prodid_num) %>%
  summarise(price = gm_mean(price),
            value = gm_mean(value))



# Call the FEWS function
fews_output <- FEWS(times = synt_data_tidy$month_num,
                    logprice = log(synt_data_tidy$price),
                    id = synt_data_tidy$prodid_num,
                    weight = synt_data_tidy$value,
                    window_length = 5,
                    splice_pos = "mean",
                    return_fe_list = TRUE,
                    num_cores = NULL)


# FEWS retuns a list of data frames
# (or a list of 1 dataframe if return_fe_list is FALSE)
# extract the results dataframe
fews_output <- fews_output$fews_df

# plot the fews index against the month number
plot(fews_output$price_date,
     fews_output$fe_indexes,
     type = "l")

