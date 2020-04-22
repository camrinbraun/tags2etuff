
interp_pdt <- function(pdt, span_x = 5, span_y = 150){

  if (class(pdt) == 'etuff'){
    pdt <- get_pdt(pdt)
  }

  ## prep the dates
  pdt$date <- as.Date(pdt$datetime)
  ddates <- pdt$datetime
  year <- as.numeric(format(ddates, '%Y')) #extracts year
  pdt$doy <- lubridate::yday(pdt$datetime)

  ## here we use mid-point
  pdt$PdtTempMid <- (pdt$PdtTempMax - pdt$PdtTempMin) / 2 + pdt$PdtTempMin

  # run the LOESS interp
  results <- grid2dloess(pdt$PdtTempMid, xgrid = pdt$doy, ygrid = pdt$PdtDepth,
                         span_x = span_x, span_y = span_y)

  return(results)

}
