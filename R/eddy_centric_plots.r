
#' @param move_eddies is object of class move_eddies output from
#'   \code{collocate_eddies}
#' @param region is character used to match and subset input move data
#' @param region_shp is SpatialPolygons used as input to
#'   \code{collocate_eddies}. Default is NULL and will not plot the region
#'   boundaries.

eddy_centric_plots <- function(move_eddies, region, region_shp = NULL){

  # subset the input move_eddies data
  pred <- move_eddies$pred[which(move_eddies$pred$region == region),]
  hist_eddies <- move_eddies$hist_pred[which(move_eddies$hist_pred$region == region),]

  sim_hist <- move_eddies$hist_sim[which(move_eddies$hist_sim$region == region),]

  tbins=seq(0, 2.5, by=0.25)
  dt <- diff(tbins)
  area <- pi * tbins ^ 2
  narea <- area[2:length(area)] - area[1:(length(area)-1)]

  # plot layout
  #nf <- layout(matrix(c(1,2,
  #                      3,4,
  #                      5,6), nrow=3, ncol=2, byrow=T), widths=c(4.5,4.5), heights=c(4,4,2))
  nf <- layout(matrix(c(1,1,
                        2,3,
                        4,5), nrow=3, ncol=2, byrow=T), widths=c(4.5,4.5), heights=c(4,4,4))

  plot(pred$lon, pred$lat, type = 'n', xlim = xl, ylim = yl, xlab = 'Longitude', ylab = 'Latitude')
  world(add = T, fill = TRUE, col = 'grey', border = 'grey')
  if (!is.null(region_shp)) plot(region_shp, add = T)
  lines(pred$lon, pred$lat)
  with(subset(pred, is.na(etype)), points(lon, lat, pch = 16, col='grey'))
  with(subset(pred, etype == 1), points(lon, lat, pch = 16, col='red'))
  with(subset(pred, etype == -1), points(lon, lat, pch = 16, col='blue'))
  points(object$meta$geospatial_lon_start, object$meta$geospatial_lat_start, col = c('green'), bg = c('green'), pch = 24)
  points(object$meta$geospatial_lon_end, object$meta$geospatial_lat_end, col = c('red'), bg = c('red'), pch = 24)

  par(mar=c(2,4.2,2,2))

  # ANTICYCLONES - eddy normalized circle plots
  plot(0, 0, xlim = c(-2.1, 2.1), ylim = c(-2.1, 2.1), type = 'n', xlab = '', ylab = 'eddy-centric distance', asp = 1)
  points(pred$dx[which(pred$etype == 1)], pred$dy[which(pred$etype == 1)], pch=16, col='red')
  plotrix::draw.circle(0, 0, radius=.5, lty=2)
  plotrix::draw.circle(0, 0, radius=1, lty=2)
  plotrix::draw.circle(0, 0, radius=2)
  abline(h=0); abline(v=0)
  #title(paste('CRAWL data', sep=''))
  #text(0,2.2,paste('Anticyclonic - ', regions[i], sep=''),cex=2)
  #title(paste('Anticyclonic - ', regions[i], sep=''))
  n <- nrow(pred[which(pred$etype == 1),])
  text(-1.8,-2, paste('N = ', n, sep=''), font = 2)
  text(2, 2, 'A', font = 2, cex = 1.2)

  # CYCLONES - eddy normalized circle plots
  plot(0, 0, xlim=c(-2.1, 2.1), ylim=c(-2.1, 2.1), type='n', xlab='', ylab='eddy-centric distance', asp=1)
  points(pred$dx[which(pred$etype == -1)], pred$dy[which(pred$etype == -1)], pch=16, col='blue')
  plotrix::draw.circle(0, 0, radius=.5, lty=2)
  plotrix::draw.circle(0, 0, radius=1, lty=2)
  plotrix::draw.circle(0, 0, radius=2)
  abline(h=0); abline(v=0)
  #title(paste('CRAWL data', sep=''))
  #text(0,2.2,paste('Anticyclonic - ', regions[i], sep=''),cex=2)
  #title(paste('Anticyclonic - ', regions[i], sep=''))
  n <- nrow(pred[which(pred$etype == -1),])
  text(-1.8,-2, paste('N = ', n, sep=''), font = 2)
  text(2, 2, 'B', font = 2, cex = 1.2)


  warning('Removing meanders from eddy-centric histogram.')
  dhist <- hist_eddies[which(hist_eddies$ismeander == 0),]
  sim_hist <- sim_hist[which(sim_hist$ismeander == 0),]

  #--------------
  ## PRED DATA
  #--------------

  # calculate eddy-normalized ACE
  na.an <- dhist[which(dhist$etype == 1),]
  na.an$narea <- narea[which(tbins %in% na.an$tbin)]
  na.an$apdf.an <- na.an$n / na.an$narea
  # get conf intervals
  na.an$ci_an_u <- (sum(na.an$n) * ((1 + ((sum(na.an$n) - na.an$n + 1) / (na.an$n * qf(1 - (0.05 / 2), 2 * na.an$n, 2 * (sum(na.an$n) - na.an$n + 1))))) ^ -1)) / na.an$narea;
  na.an$ci_an_l <- (sum(na.an$n) * ((1 + ((sum(na.an$n) - na.an$n) / ((na.an$n + 1) * qf(0.1 / 2, 2 * (na.an$n + 1), 2 * (sum(na.an$n) - na.an$n))))) ^ -1)) / na.an$narea;

  # calculate eddy-normalized CE
  na.cy <- dhist[which(dhist$etype == -1),]
  na.cy$narea <- narea[which(tbins %in% na.cy$tbin)]
  na.cy$apdf.cy <- na.cy$n / na.cy$narea
  # get conf intervals
  na.cy$ci_cy_u <- (sum(na.cy$n) * ((1 + ((sum(na.cy$n) - na.cy$n + 1) / (na.cy$n * qf(1 - (0.05 / 2), 2 * na.cy$n, 2 * (sum(na.cy$n) - na.cy$n + 1))))) ^ -1)) / na.cy$narea;
  na.cy$ci_cy_l <- (sum(na.cy$n) * ((1 + ((sum(na.cy$n) - na.cy$n) / ((na.cy$n + 1) * qf(0.1 / 2, 2 * (na.cy$n + 1), 2 * (sum(na.cy$n) - na.cy$n))))) ^ -1)) / na.cy$narea;

  ## ** need to match na.an and na.cy to all tbins in case one doesn't have all the tbins in the dataset **
  #ratio.pred <- na.an$apdf.an / na.cy$apdf.cy # ratio of one to other

  # normalize among etypes
  max.norm <- max(c(na.an$ci_an_u, na.cy$ci_cy_u))
  na.an$norm <- na.an$apdf.an / max.norm
  na.cy$norm <- na.cy$apdf.cy / max.norm

  #--------------
  ## SIM DATA
  #--------------
  # calculate eddy-normalized ACE
  sim.an <- sim_hist[which(sim_hist$etype == 1),]
  sim.an$narea <- narea[which(tbins %in% sim.an$tbin)]
  sim.an$apdf.an <- sim.an$n / sim.an$narea
  # get conf intervals
  sim.an$ci_an_u <- (sum(sim.an$n) * ((1 + ((sum(sim.an$n) - sim.an$n + 1) / (sim.an$n * qf(1 - (0.05 / 2), 2 * sim.an$n, 2 * (sum(sim.an$n) - sim.an$n + 1))))) ^ -1)) / sim.an$narea;
  sim.an$ci_an_l <- (sum(sim.an$n) * ((1 + ((sum(sim.an$n) - sim.an$n) / ((sim.an$n + 1) * qf(0.1 / 2, 2 * (sim.an$n + 1), 2 * (sum(sim.an$n) - sim.an$n))))) ^ -1)) / sim.an$narea;

  # calculate eddy-normalized CE
  sim.cy <- sim_hist[which(sim_hist$etype == -1),]
  sim.cy$narea <- narea[which(tbins %in% sim.cy$tbin)]
  sim.cy$apdf.cy <- sim.cy$n / sim.cy$narea
  # get conf intervals
  sim.cy$ci_cy_u <- (sum(sim.cy$n) * ((1 + ((sum(sim.cy$n) - sim.cy$n + 1) / (sim.cy$n * qf(1 - (0.05 / 2), 2 * sim.cy$n, 2 * (sum(sim.cy$n) - sim.cy$n + 1))))) ^ -1)) / sim.cy$narea;
  sim.cy$ci_cy_l <- (sum(sim.cy$n) * ((1 + ((sum(sim.cy$n) - sim.cy$n) / ((sim.cy$n + 1) * qf(0.1 / 2, 2 * (sim.cy$n + 1), 2 * (sum(sim.cy$n) - sim.cy$n))))) ^ -1)) / sim.cy$narea;

  #ratio.sim <- sim.an$apdf.an / sim.cy$apdf.cy # ratio of one to other

  # normalize among etypes
  max.norm.sim <- max(c(sim.an$ci_an_u, sim.cy$ci_cy_u))
  sim.an$norm <- sim.an$apdf.an / max.norm.sim
  sim.cy$norm <- sim.cy$apdf.cy / max.norm.sim

  #---------------
  ## PLOT ACE HIST
  #---------------
  par(mar=c(4.2,4.2,2,2))
  maxy <- 1.01
  plot(0, 0, type='n', xlim=c(0,2), ylim=c(0, maxy + .05 * maxy),
       xlab='distance from centroid (Ls)', ylab='normalized number per unit area')

  lines(sim.an$tbin, sim.an$norm, type = 's', col = 'red', lwd = 1, lty = 2)
  for (m in 1:nrow(sim.an)) segments(sim.an$tbin[m] + dt[1] / 2, sim.an$ci_an_l[m]/max.norm.sim, sim.an$tbin[m] + dt[1] / 2, sim.an$ci_an_u[m]/max.norm.sim, col='red', lwd=1, lty = 2)

  lines(na.an$tbin, na.an$norm, type='s', col='red', lwd=2, lty=1)
  for (m in 1:nrow(na.an)) segments(na.an$tbin[m] + dt[1] / 2, na.an$ci_an_l[m]/max.norm, na.an$tbin[m] + dt[1] / 2, na.an$ci_an_u[m]/max.norm, col='red', lwd=2)
  text(1.95, 1, 'C', font=2, cex=1.2)

  #---------------
  ## PLOT CE HIST
  #---------------
  plot(0, 0, type='n', xlim=c(0,2), ylim=c(0, maxy + .05 * maxy),
       xlab='distance from centroid (Ls)', ylab='normalized number per unit area')

  lines(sim.cy$tbin, sim.cy$norm, type = 's', col = 'blue', lwd = 1, lty = 2)
  for (m in 1:nrow(sim.cy)) segments(sim.cy$tbin[m] + dt[1] / 2, sim.cy$ci_cy_l[m]/max.norm.sim, sim.cy$tbin[m] + dt[1] / 2, sim.cy$ci_cy_u[m]/max.norm.sim, col='blue', lwd=1, lty = 2)

  lines(na.cy$tbin, na.cy$norm, type='s', col='blue', lwd=2, lty=1)
  for (m in 1:nrow(na.cy)) segments(na.cy$tbin[m] + dt[1] / 2, na.cy$ci_cy_l[m]/max.norm, na.cy$tbin[m] + dt[1] / 2, na.cy$ci_cy_u[m]/max.norm, col='blue', lwd=2)
  text(1.95, 1, 'D', font=2, cex=1.2)

  warning('Check how these are normalized...')


  #--------------
  ## RATIO PANELS
  #--------------

  #par(mar=c(4.2,4.2,2,2))
  #plot(na.an$tbin, ratio.pred, xlim=c(0,2), ylim=c(0,4.5), ylab='ratio',
  #     xlab='distance from centroid (Ls)', type='l', lty=2)
  #lines(na.an$tbin, crawl.ratio.b, lty=1)
  #text(.35,0.15,'anticyclonic / cyclonic')
  #abline(h=1, lty=3, col='grey')
  #text(1.95, 4, 'E', font=2, cex=1.2)




}
