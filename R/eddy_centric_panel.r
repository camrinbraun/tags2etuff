
#' @param pred is a subset of pred data.frame contained in class 'move_eddies'
#' @param colour is character indicating desired colour of output plot points
#' @param tbins ...
#' @param xl vector of length 2 indicating x limits, otherwise determine automatically
#' @param yl vector of length 2 indicating y limits, otherwise determine automatically

eddy_centric_panel <- function(pred, colour = NULL, tbins = NULL, xl = NULL, yl = NULL){

  u_etype <- unique(pred$etype)
  u_region <- unique(pred$region)
  u_meander <- unique(pred$ismeander)
  if (length(u_etype) > 1) stop('Input data contains >1 eddy type. These data need to be subset prior to generating individual panels.')
  if (length(u_region) > 1) stop('Input data contains >1 region. These data need to be subset prior to generating individual panels.')
  if (length(u_meander) > 1) warning('Input data contains eddies flagged as meanders. These should probably be removed but constructing the plot anyway.')

  if (is.null(colour)){
    if (pred$etype[1] == 1) colour <- 'red'
    if (pred$etype[1] == -1) colour <- 'blue'
    if (is.na(pred$etype[1])) stop('colour not specified and cannot determine eddy type automatically.')
  }

  if (is.null(tbins)){
    warning('tbins unspecified as input. Using default values of seq(0, 2.5, by = 0.25).')
    tbins <- seq(0, 2.5, by = 0.25)
  }

  if (is.null(xl)) xl <- c(min(tbins * -1), max(tbins))
  if (is.null(yl)) yl <- c(min(tbins * -1), max(tbins))

  # eddy normalized circle plot panel
  plot(0, 0, xlim = xl, ylim = yl, type = 'n', xlab = '', ylab = 'eddy-centric distance', asp = 1)
  points(pred$dx, pred$dy, pch=16, col = colour)
  plotrix::draw.circle(0, 0, radius=.5, lty=2)
  plotrix::draw.circle(0, 0, radius=1, lty=2)
  plotrix::draw.circle(0, 0, radius=2)
  abline(h=0); abline(v=0)
  #title(paste('CRAWL data', sep=''))
  #text(0,2.2,paste('Anticyclonic - ', regions[i], sep=''),cex=2)
  #title(paste('Anticyclonic - ', regions[i], sep=''))
  n <- nrow(pred)
  text(-1.8,-2, paste('N = ', n, sep=''), font = 2)
  #text(2, 2, 'A', font = 2, cex = 1.2)

}


