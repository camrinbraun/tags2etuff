
collocate_eddies_df <- function(move, eddies, distLim, radLim, hist_bins){

  #print(paste('Collocating move data to eddies...'))
  eddies <- eddies$eddies
  #move <- as.data.frame(move)
  #print(str(move))
  #print(str(eddies))
  #print(as.Date(movedays']))

  move$minDist <- NA; move$eddy_index <- NA; move$ismeander <- NA
  move$dx <- NA; move$dy <- NA; move$etype <- NA; move$tbin <- NA

  idx <- which(eddies$dt %in% as.Date(move$date))
  #print(idx)

  if(length(idx) != 0){

    # measure dx (km). index eddies by jday idx
    #dx <- 111.11 * cos(eddies$x[ii]) * (move$lon[r] - eddies$x[ii])
    #dy <- 111.11 * (move$lat[r] - eddies$y[ii])
    #dist <- sqrt(dx^2 + dy^2)
    rdist <- fields::rdist.earth.vec(cbind(as.numeric(move$lon), as.numeric(move$lat)), cbind(eddies$lon[idx], eddies$lat[idx])) * 1.60934

    if (length(rdist) != 0){
      # get min distance and the index of that eddy
      move$minDist <- min(rdist, na.rm=T)

      eLs <- eddies$rad[idx[which.min(rdist)]]

      if (!is.null(distLim)){
        getEddyIdx <- (min(rdist) <= (radLim * eLs) &
                         min(rdist) <= distLim)

      } else{
        getEddyIdx <- (min(rdist) <= (radLim * eLs))

      }

      if (getEddyIdx){
        move$eddy_index <- eddies$id[idx[which.min(rdist)]]
        move$ismeander <- eddies$ismeander[idx[which.min(rdist)]]
      } else{
        move$eddy_index <- NA
      }

    } #length rdist

  } # length idx

  ## get dx, dy and eddy type
  if(!is.na(move$eddy_index)){
    idx <- which(eddies$id == move$eddy_index & eddies$dt %in% as.Date(move$date))
    move$dx <- fields::rdist.earth.vec(cbind(move$lon, move$lat), cbind(eddies$lon[idx], move$lat), miles = F) / (eddies$rad[idx]) # in km
    move$dy <- fields::rdist.earth.vec(cbind(move$lon, move$lat), cbind(move$lon, eddies$lat[idx]), miles = F) / (eddies$rad[idx]) # in km

    if(move$lat < eddies$lat[idx]) move$dy <- move$dy * -1
    if(move$lon < eddies$lon[idx]) move$dx <- move$dx * -1

    move$etype <- eddies$type[idx]
  }

  ## normalize per unit area for histogram
  # get area for each portion of eddy then normalize number per unit area
  # compute area
  dxdy.idx <- c(grep('^dx', names(move)):grep('^dy', names(move)))
  if(!is.na(move$dx)){
    maxmove <- abs(move[,dxdy.idx][which.max(c(abs(move$dx), abs(move$dy)))])
    move$tbin <- hist_bins[findInterval(maxmove, hist_bins)]
  }

  move

}

