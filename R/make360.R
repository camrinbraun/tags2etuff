#' convert -180, 180 --> 0, 360
#' @param lon is numeric or integer longitude to be converted
#' @return converted lon

make360 <- function(lon) {
  isnot360 <- min(lon) < 0
  if (isnot360) {
    ind <- which(lon < 0)
    lon[ind] <- lon[ind] + 360
  }
  return(lon)
}