#' Try to figure out the date format
#'
#' Try to figure out the date format and coerce to something useful
#'
#' @param x is input character vector of dates
#' @return a POSIXct vector of dates

testDates <- function(x){
  # first try lubridate
  dt <- suppressWarnings(try(lubridate::as_datetime(x, tz='UTC'), TRUE))

  if(any(class(dt) == 'try-error') | any(is.na(dt))){
    # then try flipTime
    dt <- suppressWarnings(try(flipTime::AsDateTime(x, time.zone = 'UTC'), TRUE))

    if(any(class(dt) == 'try-error') | any(is.na(dt))){
      # attempt to switch date time to time date
      dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS ymd', tz='UTC'), TRUE))

      if(any(class(dt) == 'try-error') | any(is.na(dt))){
        # attempt to switch date time to time date
        dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS dbY', tz='UTC'), TRUE))

        if(any(class(dt) == 'try-error') | any(is.na(dt))){
          # attempt to switch date time to time date
          dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='mdY HMS', tz='UTC'), TRUE))

          if(any(class(dt) == 'try-error') | any(is.na(dt))){
            # attempt to switch date time to time date
            dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='mdY HM', tz='UTC'), TRUE))

            if(any(class(dt) == 'try-error') | any(is.na(dt))){
              stop('Tried lubridate, flipTime and HMS ymd orders but unable to figure out datetime format.')
            }
          }
        }
      }
    }
  }
  return(dt)
}
