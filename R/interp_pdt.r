#' Interpolate depth-temperature profile data to gap-fill
#'
#' Interpolate depth-temperature profile data to gap-fill
#'
#' @param pdt is dataframe of depth-temperature profile data
#' @param span_x is interpolation input param
#' @param span_y is interpolation input param
#' @return a list containing a data frame of interpolated depth-temperature data and a matching data frame with flags to highlihgt potential interpolation issues
#' @export
#'
interp_pdt <- function(pdt, span_x = 5, span_y = 150){

  if (class(pdt) == 'etuff'){
    pdt <- get_pdt(pdt)
  }

  ## prep the dates
  pdt$date <- as.Date(pdt$DateTime)
  ddates <- pdt$DateTime
  year <- as.numeric(format(ddates, '%Y')) #extracts year
  pdt$doy <- lubridate::yday(pdt$DateTime)

  ## here we use mid-point
  pdt$PdtTempMid <- (pdt$PdtTempMax - pdt$PdtTempMin) / 2 + pdt$PdtTempMin

  pdt <- data.frame(pdt %>% filter(!is.na(PdtDepth)))

  # run the LOESS interp
  results <- grid2dloess(pdt$PdtTempMid, xgrid = pdt$doy, ygrid = pdt$PdtDepth,
                         span_x = span_x, span_y = span_y)

  return(results)

}
