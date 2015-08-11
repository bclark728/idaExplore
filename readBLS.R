##
# readBLS.R
#
#  reader to pull data series from the BLS's api to a list of data.frames
#
#  Copyright 2015
#  Bryan Clark
#  bclark@postregister.com
#
##

library(blsAPI)
library(rjson)

readBLS <- function(codes, startyear = 2005, endyear = 2015) {
  # build request payload
  payload <- list('seriesid'  = as.list(codes),
                  'startyear' = startyear,
                  'endyear'   = endyear)
  
  # read in json data from BLS api
  response <- blsAPI(payload)
  json     <- fromJSON(response)
  
  # build data.frames
  ret = list()
  for(series in json$Results$series) {
    # split into dataset id and data
    d  <- series$data
    id <- series$seriesID
  
    # organize data into columns
    years        <- sapply(d, function(x) {x$year})
    periods      <- sapply(d, function(x) {x$period})
    period.names <- sapply(d, function(x) {x$periodName})
    values       <- sapply(d, function(x) {x$value})
    footnotes    <- sapply(d, function(x) {as.character(x$footnotes)})
  
    # build data.frame
    df <- data.frame(years, periods, period.names, values, footnotes)
    ret[[id]] <- df
  }
  
  # return list of data.frames
  ret
}
