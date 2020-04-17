# function that tries to figure out the date format and coerce to something useful

testDates <- function(x){
  # first try lubridate
  dt <- suppressWarnings(try(lubridate::as_datetime(x), TRUE))
  
  if(any(class(dt) == 'try-error') | any(is.na(dt))){
    # then try flipTime
    dt <- suppressWarnings(try(flipTime::AsDateTime(x), TRUE))
    
    if(any(class(dt) == 'try-error') | any(is.na(dt))){
      # attempt to switch date time to time date
      dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS ymd'), TRUE))
      
      if(any(class(dt) == 'try-error') | any(is.na(dt))){
        # attempt to switch date time to time date
        dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS dbY'), TRUE))
        
        if(any(class(dt) == 'try-error') | any(is.na(dt))){
          stop('Tried lubridate, flipTime and HMS ymd orders but unable to figure out datetime format.')
        }
      }
    }
  }
  return(dt)
}