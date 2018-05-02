.# FEWS_function
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
|times          |      | a vector of dates or numbers which can be coerced into dates (origin set to "1970-01-01") at which price observations were made |
| logprice      |      | vector of logarithm of prices at the given time |
| ID            |      | vector of distinct identification number of consumer goods |
|windowLength   |      | single number for the length of windows on which the Fixed effects model is run on|
|weight         | NULL | vector of expenditure weights used in the regressions|
|splicePos      | mean | The positon on which to splice the windows together. This can be a number from 1 to windowLength or any of "window", "half", "end", "mean", "movement". 'mean' gives the geoMean of all possible window splices|
| timeUnit      | NULL | Format of times data i.e. day, week, month, quarter, year. This argument can be left blank and the function aims to determine the timeUnit. If it is supplied, but the function determines that the user impute does not match the function's own estimate an error is thrown |
|numCores       | NULL | Number of cores to use for parallel computation. NULL implies no parallel computation |

## Running the Function
The intention is that this entire script is sourced and then the `FEWS()` function is run in a separate script. All of the helper functions are only designed to be called by the `FEWS()` function and not intended to be particularly useful for any other purpose.

## Splice Position
| Argument  | Details       |
|:----------|:--------------|
| window    | Equivalent to setting splicePos = 2 |        
| half      | Equivalent to setting splicePos = windowLenght/2 for even windowLenght, or (windowLenght + 1)/2 for odd windowLength 
| movement  | Equivalent to setting splicePos = windowLength
| mean      | The Geometric mean of all possible splice positions. This is suggested as the "best" method (Ivancic, Diewert and Fox 2011;33) |

## Imputation
The time unit is required to impute the data. If there are missing time entries, or time entireties with no prices, the last time which does contain data is copied until the next time with data.

## Parallel Computing
The doSnow pacakge is used to run on mutliple cores. The only part of the code which uses parrallelisation is in performing the linear regressions. Good practise, for running on a local machine, for choosing the number of cores is to use the number of cores available minus one (`parallel::detectCores()-1`). If you running on shared server, even better practise is to check what resources are available before you use parallelisation
