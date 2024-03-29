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
#' @import ggplot2
#' @import ggforce

qc_archival_etuff <- function(etuff, meta_row, writePNG = FALSE, map = TRUE, output_dir = NULL){

  if (class(etuff) != 'etuff') stop('Input etuff object must be of class etuff.')

  bins <- etuff$bins
  meta <- etuff$meta
  if (map) df <- get_track(etuff)

  etuff <- data.frame(etuff$etuff)

  if (!is.null(bins)){
    bins <- reshape2::melt(bins)#, id.vars=c('DateTime'))
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

      tad <- etuff[,c('DateTime', names(etuff)[grep('TimeAtDepth', names(etuff))])]
      #tad <- etuff[grep('TimeAtDepth', etuff$VariableName),]

      tad <- reshape2::melt(tad, id.vars=c('DateTime'))
      names(tad) <- c('DateTime','VariableName','VariableValue')

      if (class(tad$DateTime)[1] != 'POSIXct'){
        tad <- tad[which(tad$DateTime != ''),]
        tad$DateTime <- as.POSIXct(tad$DateTime, tz='UTC')
      }

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

      tat <- etuff[,c('DateTime', names(etuff)[grep('TimeAtTemp', names(etuff))])]
      #tat <- etuff[grep('TimeAtDepth', etuff$VariableName),]

      tat <- reshape2::melt(tat, id.vars=c('DateTime'))
      names(tat) <- c('DateTime','VariableName','VariableValue')

      if (class(tat$DateTime)[1] != 'POSIXct'){
        tat <- tat[which(tat$DateTime != ''),]
        tat$DateTime <- as.POSIXct(tat$DateTime, tz='UTC')
      }

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
  } else if('internalTemperature' %in% names(etuff)){
    intT <- etuff[,c('DateTime', 'internalTemperature')]
    intT <- intT[seq(1, nrow(intT), by=50),]

    extT <- etuff[,c('DateTime', 'temperature')]
    extT <- extT[seq(1, nrow(extT), by=50),]

    tad.plot <- ggplot(intT, aes(x=DateTime, y=internalTemperature, colour=internalTemperature)) +
      geom_point() + ylab('Internal Temperature') + theme(legend.position = "none")
    tat.plot <- ggplot(extT, aes(x=DateTime, y=temperature, colour=temperature)) +
      geom_point() + ylab('External Temperature') + theme(legend.position = "none")

  } else{
    tad.plot <- ggplot() + geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
    tat.plot <- ggplot() + geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
  }

  ## get SST
  #sst <- etuff[which(etuff$VariableName == 'sst'),]

  sst <- etuff[,c('DateTime', names(etuff)[grep('sst', names(etuff), fixed=TRUE)])]
  if ('sst' %in% names(sst)){
    sst <- sst[,c('DateTime','sst')]
  } else if ('sstMedian' %in% names(sst)){
    sst <- sst[,c('DateTime','sstMedian')]
    names(sst)[2] <- 'sst'
  } else if ('sstMean' %in% names(sst)){
    sst <- sst[,c('DateTime','sstMean')]
    names(sst)[2] <- 'sst'
  } else{
    stop('No SST data found.')
  }
  sst <- reshape2::melt(sst, id.vars=c('DateTime'))
  names(sst) <- c('DateTime','VariableName','VariableValue')

  if (class(sst$DateTime)[1] != 'POSIXct'){
    sst <- sst[which(sst$DateTime != ''),]
    sst$DateTime <- as.POSIXct(sst$DateTime, tz='UTC')
  }
  sst <- sst[which(!is.na(sst$VariableValue)),]

  depth <- etuff[,c('DateTime', 'depth', 'temperature')]
  depth <- depth[seq(1, nrow(depth), by=50),]

  light <- etuff[,c('DateTime', 'light')]
  light <- light[seq(1, nrow(light), by=50),]

  ## Build plots
  p1 <- ggplot(depth, aes(x=DateTime, y=depth * -1, colour=temperature)) +
    geom_point() + ylab('Depth (m)') + theme(legend.position = "none")
    #geom_path(data = etuff[which(etuff$VariableName == 'depthMax'),], aes(x=DateTime, y=as.numeric(VariableValue) * -1), colour='red') +
    #geom_path(data = etuff[which(etuff$VariableName == 'depthMin'),], aes(x=DateTime, y=as.numeric(VariableValue) * -1), colour='blue')
  p2 <- ggplot(data=sst, aes(x=DateTime, y = as.numeric(VariableValue))) + geom_path(colour = 'black') + ylab('SST (C)') + xlab('')
  p3 <- ggplot(data=light, aes(x=DateTime, y = light)) + geom_point(colour = 'black') + ylab('Light') + xlab('')

  if (map){
    ## get world map data
    world <- map_data('world')

    #if (!any(etuff$VariableName == 'latitude')) stop('map = TRUE but not lat/lon data in etuff.')
    #df <- etuff %>% spread(VariableName, VariableValue)
    #df <- df[which(!is.na(df$latitude)),]

    ## format date time
    #names(df)[1] <- 'DateTime'
    #df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')

    ## get limits
    #df$longitude <- as.numeric(df$longitude)
    #df$latitude <- as.numeric(df$latitude)
    #df$longitudeError <- as.numeric(df$longitudeError)
    #df$latitudeError <- as.numeric(df$latitudeError)
    xl <- c(min(df$longitude) - 2, max(df$longitude) + 2)
    yl <- c(min(df$latitude) - 2, max(df$latitude) + 2)

    ## simple map of move data
    m1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
      coord_fixed(xlim=xl, ylim=yl) + xlab('') + ylab('') #+
    #geom_path(data = pred, aes(x = lon, y = lat)) +

    ## add confidence intervals
    if (all(c('longitudeError', 'latitudeError') %in% names(df))){

      m1 <- m1 + geom_ellipse(data = df, aes(x0 = longitude, y0 = latitude, a = longitudeError, b = latitudeError, angle = 0),
                              alpha = 0.1, fill = 'grey', colour = 'grey')
    }

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
    if (is.null(output_dir)) output_dir <- getwd()
    ggsave(file = paste(output_dir, '/', meta_row$instrument_name, '-psat_qc.png', sep=''), width=12, height=10, units = 'in', g)
    print(paste('Maps written to ', output_dir, '/', meta_row$instrument_name, '-psat_qc.png.', sep=''))
  } else{
    print('Capture output and use gridExtra::grid.arrange(g) to show plot in device.')
  }

  return(g)

}
