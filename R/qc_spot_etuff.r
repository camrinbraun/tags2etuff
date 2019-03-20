#' This function is to do very simple QC on SPOT tag conversion to eTUFF and the
#' associated metadata. It does NOT do any filtering or further processing of
#' the positions. Instead, it is meant to check that the coordinates, time
#' stamps, location classes and associated metadata all make sense.
#'
#' @param etuff is etuff file output from tag_to_etuff()
#'


qc_spot_etuff <- function(etuff, meta_row, writePDF = FALSE){

  ## check appropriate sign on start/end coords

  ## any where datetime and variablevalue are identical?

  ## spread etuff back to tidy format
  df <- etuff %>% select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  xl <- c(min(df$longitude), max(df$longitude))
  yl <- c(min(df$latitude), max(df$latitude))

  ## get world map data
  world <- map_data('world')

  if (writePDF) pdf(paste(meta_row$instrument_name, '-ggmap.pdf', sep=''), width=8, height=12)
  p1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
    coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') +
    geom_path(data = df, aes(x = longitude, y = latitude)) +
    geom_point(data = df, aes(x = longitude, y = latitude, colour = DateTime)) +
    geom_point(data = meta_row, aes(x = geospatial_lon_start, y = geospatial_lat_start), colour = c('green'), fill = c('green'), shape = 24) +
    geom_point(data = meta_row, aes(x = geospatial_lon_end, y = geospatial_lat_end), colour = c('red'), fill = c('red'), shape = 24) +
    ggtitle(paste(meta_row$instrument_name))

  p2 <- ggplot(df, aes(y = latitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lat), colour = c('green','red'), fill = c('green','red'), shape = 24)

  p3 <- ggplot(df, aes(y = longitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lon), colour = c('green','red'), fill = c('green','red'), shape = 24)

  grid.arrange(grobs = list(p1, p2, p3), ncol=1, heights = c(6,2,2))
  #layout_matrix = rbind(c(1), c(2), c(3)))
  if (writePDF) dev.off()

}
