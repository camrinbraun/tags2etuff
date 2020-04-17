#' This function is to do very simple QC on SPOT tag conversion to eTUFF and the
#' associated metadata. It does NOT do any filtering or further processing of
#' the positions. Instead, it is meant to check that the coordinates, time
#' stamps, location classes and associated metadata all make sense.
#'
#' @param etuff is etuff file output from tag_to_etuff()
#'


qc_spot_etuff <- function(etuff, meta_row, writePDF = FALSE, cutdates = FALSE){

  ## any where datetime and variablevalue are identical?

  ## spread etuff back to tidy format
  df <- etuff %>% dplyr::select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')
  xl <- c(min(df$longitude), max(df$longitude))
  yl <- c(min(df$latitude), max(df$latitude))

  ## check appropriate sign on start/end and track coords
  ## just send warning as possible to have diff
  if (any(sign(df$latitude) != sign(meta_row$geospatial_lat_start) |
          sign(df$latitude) != sign(meta_row$geospatial_lat_end) |
          sign(meta_row$geospatial_lat_end) != sign(meta_row$geospatial_lat_start))){
    warning('Check latitudes.')
    print(paste('Lat start', meta_row$geospatial_lat_start, '.'))
    print(paste('Lat end', meta_row$geospatial_lat_end, '.'))
    print(paste('Sign of track lat is', unique(sign(df$latitude)), '.'))
  }

  if (any(sign(df$longitude) != sign(meta_row$geospatial_lon_start) |
          sign(df$longitude) != sign(meta_row$geospatial_lon_end) |
          sign(meta_row$geospatial_lon_end) != sign(meta_row$geospatial_lon_start))){
    warning('Check longitudes.')
    print(paste('Lon start', meta_row$geospatial_lon_start, '.'))
    print(paste('Lon end', meta_row$geospatial_lon_end, '.'))
    print(paste('Sign of track lon is', unique(sign(df$longitude)), '.'))
  }

  #==========
  ## BUILD PLOT
  #==========

  ## get world map data
  world <- map_data('world')

  #if (writePDF) pdf(paste(meta_row$instrument_name, '-ggmap.pdf', sep=''), width=8, height=12)
  p1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
    coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') +
    geom_path(data = df, aes(x = longitude, y = latitude)) +
    geom_point(data = df, aes(x = longitude, y = latitude, colour = DateTime)) +
    geom_point(data = meta_row, aes(x = geospatial_lon_start, y = geospatial_lat_start), colour = c('green'), fill = c('green'), shape = 24) +
    geom_point(data = meta_row, aes(x = geospatial_lon_end, y = geospatial_lat_end), colour = c('red'), fill = c('red'), shape = 24) +
    ggtitle(paste(meta_row$instrument_name))

  p2 <- ggplot(df, aes(y = latitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = meta_row, aes(x = time_coverage_start, y = geospatial_lat_start), colour = c('green'), fill = c('green'), shape = 24) +
    geom_point(data = meta_row, aes(x = time_coverage_end, y = geospatial_lat_end), colour = c('red'), fill = c('red'), shape = 24)

  p3 <- ggplot(df, aes(y = longitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = meta_row, aes(x = time_coverage_start, y = geospatial_lon_start), colour = c('green'), fill = c('green'), shape = 24) +
    geom_point(data = meta_row, aes(x = time_coverage_end, y = geospatial_lon_end), colour = c('red'), fill = c('red'), shape = 24)


  #layout_matrix = rbind(c(1), c(2), c(3)))
  if (writePDF){
    g <- gridExtra::arrangeGrob(grobs = list(p1, p2, p3), ncol=1, heights = c(6,2,2))
    ggsave(file = paste(meta_row$instrument_name, '-ggmap.pdf', sep=''), width=8, height=8, units = 'in', g)
    #dev.off()
    print(paste('Maps written to ', meta_row$instrument_name, '-ggmap.pdf.', sep=''))
  } else{
    gridExtra::grid.arrange(grobs = list(p1, p2, p3), ncol=1, heights = c(6,2,2))
    print('Should have output plot to graphics device.')
  }

}
