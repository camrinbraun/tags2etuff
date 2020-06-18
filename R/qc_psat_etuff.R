#' Simple quality control plots for archival eTUFF file
#'
#' This function is to do very simple QC on PSAT tag conversion to eTUFF and the
#' associated metadata. It is meant to check that the converted data make sense.
#'
#' @param etuff is etuff file
#' @param meta_row is the associated metadata
#' @param writePNG is logical indicating whether or not to write the output to file as PNG
#' @param map is logical indicating whether to try to add a map (assuming track data is present in eTUFF)
#'
#' @return a QC plot
#' @export
#'
#' @importFrom lattice levelplot

qc_psat_etuff <- function(etuff, meta_row, writePNG = FALSE, map = TRUE){

  if (class(etuff) != 'etuff') stop('Input etuff object must be of class etuff.')

  bins <- etuff$bins
  meta <- etuff$meta
  #etuff <- etuff$etuff

  etuff <- reshape2::melt(etuff$etuff, id.vars=c('DateTime'))
  names(etuff) <- c('DateTime','VariableName','VariableValue')

  if (class(etuff$DateTime)[1] != 'POSIXct'){
    etuff <- etuff[which(etuff$DateTime != ''),]
    etuff$DateTime <- as.POSIXct(etuff$DateTime, tz='UTC')
  }

  if (!is.null(bins)){
    bins <- reshape2::melt(bins, measure.vars = c(1:ncol(bins)))#, id.vars=c('DateTime'))
    names(bins) <- c('VariableName','VariableValue')
  }


  #==========
  ## BUILD PLOT
  #==========

  ## simple depth light and sst plots

  # tad/tat plots
  if (!is.null(bins)){
    tad_bins <- bins[grep('HistDepthBin', bins$VariableName),]

    if (nrow(tad_bins) == 0){
      tad.plot <- ggplot() + geom_blank(aes(1,1)) +
        cowplot::theme_nothing()
      tat.plot <- ggplot() + geom_blank(aes(1,1)) +
        cowplot::theme_nothing()
    } else{

      tad <- etuff[grep('TimeAtDepth', etuff$VariableName),]
      tad <- tad[which(!is.na(tad$VariableValue)),]
      tad$binMin <- NA; tad$binMax <- NA
      binNames <- unique(tad$VariableName)
      binNames <- substr(binNames, 15, 17)
      for (ii in 1:length(binNames)){
        tad$binMin[grep(paste(binNames[ii]), tad$VariableName)] <- unique(tad_bins$VariableValue[grep(paste('Min', binNames[ii], sep=''), tad_bins$VariableName)])
        tad$binMax[grep(paste(binNames[ii]), tad$VariableName)] <- unique(tad_bins$VariableValue[grep(paste('Max', binNames[ii], sep=''), tad_bins$VariableName)])
      }

      tad$VariableValue <- as.numeric(tad$VariableValue)
      tad$binMax <- as.numeric(tad$binMax)
      tad.plot <- levelplot(tad$VariableValue ~ tad$DateTime * tad$binMax, col.regions = jet.colors, ylim=c(1005,0),
                            xlab='', ylab='Depth (m)')#, main='Time-at-depth (%)')

      tat_bins <- bins[grep('HistTempBin', bins$VariableName),]
      tat <- etuff[grep('TimeAtTemp', etuff$VariableName),]
      tat <- tat[which(!is.na(tat$VariableValue)),]
      tat$binMin <- NA; tat$binMax <- NA
      binNames <- unique(tat$VariableName)
      binNames <- substr(binNames, 14, 16)
      for (ii in 1:length(binNames)){
        tat$binMin[grep(paste(binNames[ii]), tat$VariableName)] <- unique(tat_bins$VariableValue[grep(paste('Min', binNames[ii], sep=''), tat_bins$VariableName)])
        tat$binMax[grep(paste(binNames[ii]), tat$VariableName)] <- unique(tat_bins$VariableValue[grep(paste('Max', binNames[ii], sep=''), tat_bins$VariableName)])
      }

      tat$VariableValue <- as.numeric(tat$VariableValue)
      tat$binMax <- as.numeric(tat$binMax)
      tat.plot <- levelplot(tat$VariableValue ~ tat$DateTime * tat$binMax, col.regions = jet.colors, #ylim=c(1005,0),
                            xlab='', ylab='Temperature (C)')#, main='Time-at-temperature (%)')

    }
  } else{
    tad.plot <- ggplot() + geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
    tat.plot <- ggplot() + geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
  }

  ## get SST
  sst <- etuff[which(etuff$VariableName == 'sst'),]
  sst <- sst[which(!is.na(sst$VariableValue)),]

  ## Build plots
  p1 <- ggplot(etuff[which(etuff$VariableName == 'depth'),], aes(x=DateTime, y=as.numeric(VariableValue) * -1)) +
    geom_point(colour = 'black') + ylab('Depth (m)') +
    geom_path(data = etuff[which(etuff$VariableName == 'depthMax'),], aes(x=DateTime, y=as.numeric(VariableValue) * -1), colour='red') +
    geom_path(data = etuff[which(etuff$VariableName == 'depthMin'),], aes(x=DateTime, y=as.numeric(VariableValue) * -1), colour='blue')
  p2 <- ggplot(data=sst, aes(x=DateTime, y = as.numeric(VariableValue))) + geom_path(colour = 'black') + ylab('SST (C)') + xlab('')
  p3 <- ggplot(data=etuff[which(etuff$VariableName == 'light'),], aes(x=DateTime, y = as.numeric(VariableValue))) + geom_point(colour = 'black') + ylab('Light') + xlab('')

  if (map){
    ## get world map data
    world <- map_data('world')

    if (!any(etuff$VariableName == 'latitude')) stop('map = TRUE but not lat/lon data in etuff.')
    df <- etuff %>% spread(VariableName, VariableValue)
    df <- df[which(!is.na(df$latitude)),]

    ## format date time
    #names(df)[1] <- 'DateTime'
    #df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')

    ## get limits
    df$longitude <- as.numeric(df$longitude)
    df$latitude <- as.numeric(df$latitude)
    df$longitudeError <- as.numeric(df$longitudeError)
    df$latitudeError <- as.numeric(df$latitudeError)
    xl <- c(min(df$longitude) - 2, max(df$longitude) + 2)
    yl <- c(min(df$latitude) - 2, max(df$latitude) + 2)

    ## simple map of move data
    m1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
      coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') #+
    #geom_path(data = pred, aes(x = lon, y = lat)) +

    ## add confidence intervals
    m1 <- m1 + geom_ellipse(data = df, aes(x0 = longitude, y0 = latitude, a = longitudeError, b = latitudeError, angle = 0),
                            alpha = 0.1, fill = 'grey', colour = 'grey')

    ## add points on top
    m1 <- m1 + geom_point(data = df, aes(x = longitude, y = latitude, colour = DateTime))

      #geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_start), y = as.numeric(geospatial_lat_start)), colour = c('green'), fill = c('green'), shape = 24) +
      #geom_point(data = object$meta, aes(x = as.numeric(geospatial_lon_end), y = as.numeric(geospatial_lat_end)), colour = c('red'), fill = c('red'), shape = 24) +
      #ggtitle(paste(object$meta$instrument_name))

    #ggplot(res.all, aes(x=date, y=qlogis(g), ymax = qlogis(g) + g.se, ymin = qlogis(g) - g.se, colour=as.factor(step), fill = as.factor(step))) +
    #  xlab('') + geom_ribbon(alpha=0.15, colour=NA)

    #ggplot(df, aes(x=longitude, y=latitude,
    #               ymax = latitude + latitudeError,
    #               ymin =  latitude - latitudeError,
    #               xmax = longitude + longitudeError,
    #               xmin = longitude + longitudeError,
    #               colour=date)) +
    #  xlab('') + geom_ribbon(alpha=0.15, colour=NA)

  }

  if (map){
    lay <- rbind(c(1,4),
                 c(2,4),
                 c(3,5),
                 c(6,6))
    g <- gridExtra::arrangeGrob(grobs = list(p3, p2, p1, tad.plot, tat.plot, m1), heights = c(2,2,4,6),
                                width = c(5,5), layout_matrix = lay)
  } else{
    lay <- rbind(c(1,4),
                 c(2,4),
                 c(3,5))

    g <- gridExtra::arrangeGrob(grobs = list(p3, p2, p1, tad.plot, tat.plot), heights = c(2,2,4),
                                width = c(5,5), layout_matrix = lay)
  }


  if (writePNG){
    ggsave(file = paste(meta_row$instrument_name, '-psat_qc.png', sep=''), width=12, height=10, units = 'in', g)
    print(paste('Maps written to ', meta_row$instrument_name, '-psat_qc.pdf.', sep=''))
  } else{
    print('Capture output and use gridExtra::grid.arrange(g) to show plot in device.')
  }

  return(g)

}
