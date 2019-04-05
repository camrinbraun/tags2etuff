
#' @param dhist is a subset of hist_xxx data.frame contained in class 'move_eddies'. Can be either hist_pred or hist_sim.
#' @param max.norm is value by which to normalize the frequency values by. Default is by max(upper_confidence_interval).
#' @param colour is character indicating desired colour of output plot points
#' @param tbins ...
#' @param lty
#' @param lwd
#' @param return_panel logical; do you want to return a plot panel using plot() or just add to existing panel using lines()

eddy_centric_hist <- function(dhist, max.norm = NULL, tbins = NULL, colour = NULL, lty = 1, lwd = 1, return_panel = TRUE){

  u_etype <- unique(dhist$etype)
  u_region <- unique(dhist$region)
  u_meander <- unique(dhist$ismeander)
  if (length(u_etype) > 1) stop('Input data contains >1 eddy type. These data need to be subset prior to generating individual panels.')
  if (length(u_region) > 1) stop('Input data contains >1 region. These data need to be subset prior to generating individual panels.')
  if (length(u_meander) > 1) warning('Input data contains eddies flagged as meanders. These should probably be removed but constructing the plot anyway.')

  if (is.null(tbins)){
    warning('tbins unspecified as input. Using default values of seq(0, 2.5, by = 0.25).')
    tbins <- seq(0, 2.5, by = 0.25)
  }

  if (is.null(colour)){
    if (dhist$etype[1] == 1) colour <- 'red'
    if (dhist$etype[1] == -1) colour <- 'blue'
    if (is.na(dhist$etype[1])) stop('colour not specified and cannot determine eddy type automatically.')
  }

  dt <- diff(tbins)
  area <- pi * tbins ^ 2
  narea <- area[2:length(area)] - area[1:(length(area)-1)]

  # fill missing tbins
  tbins.missing <- tbins[-which(tbins %in% dhist$tbin)]
  if (length(tbins.missing) > 0){
    dhist[c((nrow(dhist) + 1):(nrow(dhist) + length(tbins.missing))),] <- NA
    dhist$tbin[which(is.na(dhist$tbin))] <- tbins.missing
    dhist$region <- dhist$region[1]
    dhist$etype <- dhist$etype[1]
    dhist$ismeander <- dhist$ismeander[1]
    dhist$n[which(is.na(dhist$n))] <- 0
    dhist <- dhist[order(dhist$tbin),]
  }

  # calculate eddy-normalized histogram
  na.an <- dhist
  na.an$narea <- narea[which(tbins %in% na.an$tbin)]
  na.an$apdf.an <- na.an$n / na.an$narea

  # get conf intervals
  na.an$ci_an_u <- (sum(na.an$n) * ((1 + ((sum(na.an$n) - na.an$n + 1) / (na.an$n * qf(1 - (0.05 / 2), 2 * na.an$n, 2 * (sum(na.an$n) - na.an$n + 1))))) ^ -1)) / na.an$narea;
  na.an$ci_an_l <- (sum(na.an$n) * ((1 + ((sum(na.an$n) - na.an$n) / ((na.an$n + 1) * qf(0.1 / 2, 2 * (na.an$n + 1), 2 * (sum(na.an$n) - na.an$n))))) ^ -1)) / na.an$narea;

  # normalize
  if (is.null(max.norm)) max.norm <- max(na.an$ci_an_u, na.rm=T)
  na.an$norm <- na.an$apdf.an / max.norm

  if(return_panel){
    par(mar=c(4.2,4.2,2,2))
    maxy <- 1.01
    plot(0, 0, type='n', xlim=c(0,2), ylim=c(0, maxy + .05 * maxy),
         xlab='distance from centroid (Ls)', ylab='normalized number per unit area')
  }

  lines(na.an$tbin, na.an$norm, type='s', col=colour, lwd=lwd, lty=lty)
  for (m in 1:nrow(na.an)) segments(na.an$tbin[m] + dt[1] / 2, na.an$ci_an_l[m]/max.norm, na.an$tbin[m] + dt[1] / 2, na.an$ci_an_u[m]/max.norm, col=colour, lwd=lwd)
  #text(1.95, 1, 'C', font=2, cex=1.2)
  print(na.an)

}
