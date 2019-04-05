plot.move <- function(object){
  ## TO ADD:
  # - PSAT functionality, additional plot panel


  # get pred data and limits
  pred <- object$pred
  xl <- c(min(pred$lon), max(pred$lon))
  yl <- c(min(pred$lat), max(pred$lat))

  # modify limits if necessary
  if (min(as.numeric(c(object$meta$geospatial_lon_start, object$meta$geospatial_lon_end))) < xl[1]) xl[1] <- min(as.numeric(c(object$meta$geospatial_lon_start, object$meta$geospatial_lon_end)))
  if (max(as.numeric(c(object$meta$geospatial_lon_start, object$meta$geospatial_lon_end))) > xl[2]) xl[2] <- max(as.numeric(c(object$meta$geospatial_lon_start, object$meta$geospatial_lon_end)))
  if (min(as.numeric(c(object$meta$geospatial_lat_start, object$meta$geospatial_lat_end))) < yl[1]) yl[1] <- min(as.numeric(c(object$meta$geospatial_lat_start, object$meta$geospatial_lat_end)))
  if (max(as.numeric(c(object$meta$geospatial_lat_start, object$meta$geospatial_lat_end))) > yl[2]) yl[2] <- max(as.numeric(c(object$meta$geospatial_lat_start, object$meta$geospatial_lat_end)))

  ## get world map data
  world <- map_data('world')

  ## simple map of move data
  p1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
    coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') +
    geom_path(data = pred, aes(x = lon, y = lat)) +
    geom_point(data = pred, aes(x = lon, y = lat, colour = date)) +
    geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_start), y = as.numeric(geospatial_lat_start)), colour = c('green'), fill = c('green'), shape = 24) +
    geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_end), y = as.numeric(geospatial_lat_end)), colour = c('red'), fill = c('red'), shape = 24) +
    ggtitle(paste(object$meta$instrument_name))

  p1

}


#xl <- extendrange(mpm_fit$data$lon, f = 0.1)
#yl <- extendrange(mpm_fit$data$lat, f = 0.1)
#mpm_fit$data$g <- mpm_fit$fitted$g
#p <- ggplot() + geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"), fill = grey(0.3)) +
#  coord_cartesian(xlim = xl, ylim = yl) + xlab("Longitude") + ylab("Latitude")
#p <- p + ggtitle(paste(unique(as.character(mpm_fit$fitted$id)), sep = ""))
#p <- p + geom_point(data = fls$ssm[[1]]$data, aes_string(x = "lon", y = "lat", group = NULL),
#                    colour = grey(0.7), pch = "+", size = 4)
#p <- p + geom_point(data = mpm_fit$data, aes_string(x = "lon", y = "lat", group = NULL, colour = "g"),
#                    size = 1.25) +
#  scale_colour_gradientn(colours = viridis::viridis(256, alpha=1, begin=0, end=1, option='D'))
#p

