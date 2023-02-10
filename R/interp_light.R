#' Interpolate depth-temperature profile data to gap-fill
#'
#' Interpolate depth-temperature profile data to gap-fill
#'
#' @param df is dataframe of depth-light profile data. requires cols depth, light, DateTime. usually this has been filtered to daytime only using add_daynight() then dplyr::filter()
#' @param span_x is interpolation input param
#' @param span_y is interpolation input param
#' @param mask is logical. default is FALSE. if TRUE, the interpolated output is masked with NAs below each daily max depth value in input data, df
#' @return a list containing a data frame of interpolated depth-light data and a matching data frame with flags to highlihgt potential interpolation issues
#' @export
#' @examples
#' ## data probably sourced from series in most cases
#' series <- get_series(etuff)
#' ## usually filtered for daytime only
#' series$dn <- add_daynight(series, etuff)
#' series_dayOnly <- series %>% filter(dn == 'd' & !is.na(depth) & !is.na(light))
#' 
#' results <- interp_light(series_dayOnly, mask = TRUE)
#' 
#' ## day_vec is just daily integer that the loess uses so we calc that here too but could replace with unique dates
#' series$day_vec <- as.integer(df$date) - min(as.integer(df$date)) + 1
#' day_vec <- results$xgrid
#' depth_vec <- results$ygrid
#' image(day_vec, depth_vec, t(results$sm_data), axes=F, ylim=c(max(depth_vec),0))
#' contour(day_vec, depth_vec, t(results$sm_data), add=T)
#' 
interp_light <- function(df, span_x = 5, span_y = 150, mask = FALSE){
  
  ## prep the dates
  df$date <- as.Date(df$DateTime)
  ddates <- df$DateTime
  year <- as.numeric(format(ddates, '%Y')) #extracts year
  
  ## doy breaks if wrapped around annual cycle (i.e. deployments > 1 year)
  #df$doy <- lubridate::yday(df$DateTime)
  df$day_vec <- as.integer(df$date) - min(as.integer(df$date)) + 1
  
  df <- data.frame(df %>% filter(!is.na(depth)))
  
  # run the LOESS interp
  t1 <- Sys.time()
  results <- grid2dloess(df$light, xgrid = df$day_vec, ygrid = df$depth,
                         span_x = span_x, span_y = span_y)
  t2 <- Sys.time()
  
  ## mask interp based on max daily depth
  if (mask){
    maxZ <- data.frame(df %>% group_by(day_vec) %>% summarise(round(max(depth))))
    for (i in 1:nrow(maxZ)){
      results$sm_data[c((maxZ[i,2]+1):nrow(results$sm_data)), i] <- NA
    }
  }
  
  return(results)
  
}

