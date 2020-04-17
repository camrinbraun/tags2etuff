#' This function is to do very simple QC on PSAT tag conversion to eTUFF and the
#' associated metadata. It is meant to check that the converted data make sense.
#'
#' @param etuff is etuff file output from tag_to_etuff()
#'


qc_psat_etuff <- function(etuff, meta_row, writePNG = FALSE){

  ## any where datetime and variablevalue are identical?

  ## spread etuff back to tidy format
  #tad <- etuff[grep('TimeAtDepthBin', etuff$VariableName),] # %>% select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  #zbins <- etuff[grep('HistDepth', etuff$VariableName),] %>% distinct(VariableValue)
  #df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')

  #tlims <- c(min(df$DateTime), max(df$DateTime))
  #zlims <- c(min(c(min(df$depthMin, na.rm = T), min(df$depth, na.rm = T))),
  #           max(c(max(df$depthMax, na.rm = T), max(df$depth, na.rm = T))))

  #==========
  ## BUILD PLOT
  #==========

  # get depth limits
  #zlims <- c(min(etuff$VariableValue[which(etuff$VariableName %in% c('depth','depthMin','depthMax'))]),
  #           max(etuff$VariableValue[which(etuff$VariableName %in% c('depth','depthMin','depthMax'))]))

  # get sst limits
  #sst_lims <- c(min(etuff$VariableValue[which(etuff$VariableName %in% c('sst','sstMean','sstMin','sstMax'))]),
  #           max(etuff$VariableValue[which(etuff$VariableName %in% c('sst','sstMean','sstMin','sstMax'))]))

  # simple depth light and sst plots
  etuff$DateTime <- as.POSIXct(etuff$DateTime, tz='UTC')
  #with(etuff[which(etuff$VariableName == 'sst'),], plot(DateTime, VariableValue, ylim=sst_lims, ylab='SST (C)', xlab=''))
  #with(etuff[which(etuff$VariableName == 'light'),], plot(DateTime, VariableValue, ylab='Light', xlab=''))
  #with(etuff[which(etuff$VariableName == 'depth'),], plot(DateTime, VariableValue, ylim=c(zlims[2], zlims[1]), ylab='Depth (m)', xlab=''))

  # tad/tat plots
  tad_bins <- etuff[grep('HistDepthBin', etuff$VariableName),]
  if (nrow(tad_bins) == 0){
    tad.plot <- ggplot()+geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
    tat.plot <- ggplot()+geom_blank(aes(1,1)) +
      cowplot::theme_nothing()
  } else{
    tad <- etuff[grep('TimeAtDepth', etuff$VariableName),]
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

    tat_bins <- etuff[grep('HistTempBin', etuff$VariableName),]
    tat <- etuff[grep('TimeAtTemp', etuff$VariableName),]
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

  p1 <- ggplot(etuff[which(etuff$VariableName == 'depth'),], aes(x=DateTime, y=VariableValue * -1)) +
    geom_point(colour = 'black') + ylab('Depth (m)') +
    geom_path(data = etuff[which(etuff$VariableName == 'depthMax'),], aes(x=DateTime, y=VariableValue * -1), colour='red') +
    geom_path(data = etuff[which(etuff$VariableName == 'depthMin'),], aes(x=DateTime, y=VariableValue * -1), colour='blue')
  p2 <- ggplot(data=etuff[which(etuff$VariableName == 'sst'),], aes(x=DateTime, y = VariableValue)) + geom_path(colour = 'black') + ylab('SST (C)') + xlab('')
  p3 <- ggplot(data=etuff[which(etuff$VariableName == 'light'),], aes(x=DateTime, y = VariableValue)) + geom_point(colour = 'black') + ylab('Light') + xlab('')

  lay <- rbind(c(1,4),
               c(2,4),
               c(3,5))

  if (writePNG){
    g <- gridExtra::arrangeGrob(grobs = list(p3, p2, p1, tad.plot, tat.plot), heights = c(2,2,4),
                              width = c(5,5), layout_matrix = lay)
    ggsave(file = paste(meta_row$instrument_name, '-psat_qc.png', sep=''), width=12, height=10, units = 'in', g)
    print(paste('Maps written to ', meta_row$instrument_name, '-psat_qc.pdf.', sep=''))
  } else{
    gridExtra::arrangeGrob(grobs = list(p3, p2, p1, tad.plot, tat.plot), heights = c(2,2,4),
                           width = c(5,5), layout_matrix = lay)
    g <- gridExtra::arrangeGrob(grobs = list(p3, p2, p1, tad.plot, tat.plot), heights = c(2,2,4),
                                width = c(5,5), layout_matrix = lay)
    print('Should have output plot to graphics device.')
  }

  return(g)

}
