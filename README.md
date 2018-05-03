# FEWS_function
This function aims to offer a versatile implementation of the FEWS index for public use.

Licence: GNU GPLv3

Authored by:
Donal Lynch - Stats New Zealand  
Matthew Stansfield - Stats New Zealand  
Sam Olivecrona - Stats New Zealand  

This code is still in a development stage, and any comments, issues, feature requests are very welcome:  
donal.lynch@stats.govt.nz

## Arguments
The function takes the following arguments:             
                  
| Argument      | Default | Details  |
|:------------- |:-----|:------|
| times         |      | a vector of dates, or numbers which can be coerced into dates (origin set to "1970-01-01") at which price observations were made |
| logprice      |      | vector of logarithm of prices at the given time |
| id            |      | vector of distinct identification number of consumer goods |
| window_length |      | single number for the length of windows on which the Fixed effects model is run on|
| weight        | NULL | vector of expenditure weights used in the regressions|
| splice_pos    | mean | The positon on which to splice the windows together. This can be a number from 1 to window_length or any of "window", "half", "mean", "movement". See below for further details |
| time_unit     | NULL | Format of times data i.e. day, week, month, quarter, year. This argument can be left blank and the function aims to determine the time_unit. If it is supplied, but the function determines that the user input does not match the function's own estimate an error is thrown |
|num_cores      | NULL | Number of cores to use for parallel computation. NULL implies no parallel computation |

## Running the Function
The intention is that this entire script is sourced and then the `FEWS()` function is run in a separate script. All of the helper functions are only designed to be called by the `FEWS()` function and not intended to be particularly useful for any other purpose.

## Splice Position
| Argument  | Details       |
|:----------|:--------------|
| window    | Equivalent to setting `splice_pos = 2` |        
| half      | Equivalent to setting `splice_pos = window_length/2` for even window_length, or `splice_pos = (window_length + 1)/2` for odd window_length |
| movement  | Splicing based on the last day of the current window relative to the to last day of the previous window |
| mean      | The Geometric mean of all possible splice positions. This is suggested as the "best" method (Ivancic, Diewert and Fox 2011;33) |

An excellent description of these different splice position options, with formulae, can be found in the vignette for the IndexNumR package. See the 'Extending the GEKS index' section of the vignette which is available here: [IndexNumR Vignette](https://cran.r-project.org/web/packages/IndexNumR/vignettes/indexnumr.html). The variable naming convention used in the IndexNumR vignette is followed in this code in the `splice_update()` function.

## Imputation
The time_unit is required to impute the data. If there are missing time entries, or time entries with no prices, the last time which does contain data is copied until the next time with data.

## Parallel Computing
The doSnow package is used to run on multiple cores. The only part of the code which uses parallelisation is in performing the linear regressions. Good practise for running on a local machine is to choose the number of cores as the number of cores available minus one (`parallel::detectCores()-1`). If you are running on shared server, even better practise is to check what resources are available before you use parallelisation.
