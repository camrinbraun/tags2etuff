#' @param object is object of class move_eddies
#' @param type indicates the type of plot to generate for input object
plot.move_eddies <- function(object, type = 'map'){
  ## TO ADD:
  # - PSAT functionality, additional plot panel


  if (class(object)[1] != 'move_eddies') stop('Object must be of class move_eddies.')

  if (type == 'map'){
    ## then make a map
    print('need to add region shp to the ggplot code')

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
      geom_point(data = subset(pred, is.na(etype)), aes(x = lon, y = lat), colour='grey') +
      geom_point(data = pred, aes(x = lon, y = lat, colour = as.factor(etype))) +
      scale_color_manual(values=c("blue", "red")) +
      geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_start), y = as.numeric(geospatial_lat_start)), colour = c('green'), fill = c('green'), shape = 24) +
      geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_end), y = as.numeric(geospatial_lat_end)), colour = c('red'), fill = c('red'), shape = 24) +
      ggtitle(paste(object$meta$instrument_name))

    print(p1)

  }

  if (type == 'eddy-centric'){

    warning('currently using a specific region in eddy-centric plot call. need to pass args from plot fun')

    # then make eddy-centric plots
    print(eddy_centric_plots(move_eddies, region = 'Sargasso', region_shp = NULL))


  }

  # push the output plot(s)
  #p1

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

plot(pred$lon, pred$lat, type = 'n', xlim = xl, ylim = yl, xlab = 'Longitude', ylab = 'Latitude')
world(add = T, fill = TRUE, col = 'grey', border = 'grey')
plot(region_shp, add = T)
lines(pred$lon, pred$lat)
with(subset(pred, is.na(etype)), points(lon, lat, pch = 16, col='grey'))
with(subset(pred, etype == 1), points(lon, lat, pch = 16, col='red'))
with(subset(pred, etype == -1), points(lon, lat, pch = 16, col='blue'))
points(object$meta$geospatial_lon_start, object$meta$geospatial_lat_start, col = c('green'), bg = c('green'), pch = 24)
points(object$meta$geospatial_lon_end, object$meta$geospatial_lat_end, col = c('red'), bg = c('red'), pch = 24)
