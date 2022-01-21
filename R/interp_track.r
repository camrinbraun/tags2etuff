#' Interpolate 2d data from eTUFF
#'
#' Interpolate spatial movements to desired resolution
#'
#' @param etuff_file is path to etuff file that will be read to an object of class etuff or etuff archival
#' @param res_in (optional) is numeric (hours) indicating input temporal resolution, typically for PSAT data
#' @param res_out (optional) is numeric (hours) indicating desired temporal resolution of output track.
#' @param interp_method (optional) is character indicating desired interpolation method if res_in and res_out are not equal. Options are "interval" or "SSM". The latter implements \code{fit_ssm} from the foieGras package. This is typically used for PSAT data only.
#' @return a dataframe of interpolated 2d movements
#' @export
#'
interp_track <- function(etuff_file,...){

  args <- list(...)
  etuff <- read_archival(etuff_file)

  ## read whatever track data is in the etuff file
  track <- get_track(etuff,...)

  ## temporal res of output track
  if('res_out' %in% names(args)){
    res_out <- args$res_out
    logger::log_info(paste0('res_out set to ', res_out, ' hours.'))
  } else{
    logger::log_info(paste0('res_out not specified. Fixing temporal resolution of output to 24 hours.'))
    res_out <- 24
  }

  if (etuff$meta$instrument_type == 's') etuff$meta$instrument_type <- 'satellite'
  if (etuff$meta$instrument_type %in% c('PSAT','psat')) etuff$meta$instrument_type <- 'popup'
  if (!('waypoints_source' %in% names(etuff$meta)) & etuff$meta$instrument_type == 'satellite') etuff$meta$waypoints_source <- 'Argos'

  logger::log_info(paste0('Building interpolated track for instrument_type = ',
                          etuff$meta$instrument_type, ' and waypoints_source = ',
                          etuff$meta$waypoints_source, '.'))

  ## determine type of dataset we're dealing with
  if (etuff$meta$instrument_type == 'satellite' |
      (etuff$meta$instrument_type == 'popup' & etuff$meta$waypoints_source == 'Argos')){
    if ('argosLC' %in% names(df)){
      type = 1
    } else{
      type = 2
    }
  } else if (etuff$meta$instrument_type %in% c('popup') &
             etuff$meta$waypoints_source == 'modeled'){
    type = 2
  }


  #-------------------
  ## raw Argos with LCs
  #-------------------

  if (type == 1){

    df <- track
    df$id <- 1

    df <- df[,c('id','DateTime','argosLC','longitude','latitude')]
    names(df) <- c('id','date','lc','lon','lat')

    time_step = 6; bb=6
    df.locs <- split(df, df$id)
    ssm_fit <- lapply(df.locs, function(x) foieGras::fit_ssm(x, model='crw', time.step = time_step, vmax=10))
    idx <- lapply(ssm_fit, function(x) x$converged) %>% do.call(rbind, .)
    if (any(!idx)){
      for (ii in which(!idx)){
        logger::log_info('Some initial use of fit_ssm() did not converge. Trying other time_step values.')
        #df.ii <- foieGras::grab(ssm_fit[[ii]], 'data', as_sf=F)
        df.ii <- df.locs[[ii]]
        #df.ii <- ssm_fit[[ii]]$ssm[[1]]$data
        df.ii <- data.frame(df.ii[,names(df)])

        # iterate through some reasonable time steps
        poss.tstep <- c(1,3,6,12,24)
        poss.tstep <- poss.tstep[-which(poss.tstep %in% time_step)]
        for (bb in poss.tstep){
          try_fit <- foieGras::fit_ssm(df.ii, model='crw', time.step = bb, vmax=10, optim='nlminb')
          if (try_fit$converged){
            ssm_fit[[ii]] <- try_fit
            logger::log_info(paste('Converged on time step', bb, 'hours.'))
            break
          } else if (!try_fit$converged & bb == 24){
            logger::log_info('None of the alternative time steps resulted in fit_ssm() model convergence.')
            logger::log_info('Cancelling simulations.')
            sim <- FALSE; tmp_out <- NULL
          } else{
            next
          }
        } # bb
      } # which idx
    } # if any

    ## grab predicted locations output from fit_ssm
    plocs <- lapply(ssm_fit, FUN=function(x) foieGras::grab(x, what = "p", as_sf = FALSE)) %>%
      do.call(rbind, .) %>%
      tibble::as_tibble() %>%
      mutate(id = as.character(id)) %>% group_by(id)

    ## subsample from predicted locations
    plocs <- split(plocs, plocs$id)

    for (tt in 1:length(plocs)){
      res.tt <- as.numeric(difftime(plocs[[tt]]$date[2], plocs[[tt]]$date[1], units = 'hours'))
      res_slice <- res_out / res.tt
      #if (res_slice < 1) res_slice <- 1
      plocs[[tt]] <- plocs[[tt]] %>% do(slice(., seq(1, n(), by = res_slice)))
    }

    plocs <- plocs %>%
      do.call(rbind, .) %>%
      tibble::as_tibble() %>%
      mutate(id = as.character(id)) %>% group_by(id)

    tr <- plocs[,c('date','lat','lon','y.se','x.se')]
    names(tr) <- c('DateTime','latitude','longitude','latitudeError','longitudeError')
    tr$instrument_name <- etuff$meta$instrument_name
    tr$platform <- etuff$meta$platform
    tr <- tr %>% select(c('instrument_name', 'platform','DateTime','latitude','latitudeError','longitude','longitudeError')) %>% as.data.frame()

    ## approx error in dec degrees from meters
    tr$latitudeError <- tr$latitudeError / 110
    tr$longitudeError <- tr$longitudeError / 110

    #-------------------
    ## PSAT with modeled locs (i.e. GPE3, HMMoce, etc)
    #-------------------
  } else if (type == 2){

    tr <- track %>% select(c('DateTime','latitude','latitudeError','longitude','longitudeError'))

    ## check temporal resolution and adjust accordingly
    ## if no temporal resolution is specified, try to detect it (this should nearly always work with a PSAT tag)
    if (!('res_in' %in% names(args))){
      res_in <- Mode(as.numeric(diff(tr$DateTime), units='hours'))
      logger::log_info(paste('No temporal resolution specified. Mode of diff(timeseries) yielded ', res_in, ' hours', sep=''))
    } else{
      logger::log_info(paste('Using specified ', res_in, 'hours as the input of the PSAT track data.', sep=''))
    }

    ## fix temporal resolution if doesnt match desired output
    if (res_in < res_out | res_in > res_out){

      tr$instrument_name <- etuff$meta$instrument_name

      if ('interp_method' %in% names(args)){
        interp_method <- args$interp_method
      } else{
        if (res_in < res_out) interp_method <- 'interval'
        if (res_in > res_out) interp_method <- 'SSM'
      }

      logger::log_info(paste0('Input PSAT track resolution (', res_in, ' hours) is not equal to desired output (', res_out, ' hours). Coercing using interpolation method ', interp_method, '...'))

      if (interp_method == 'SSM'){
        tr$lc <- rep('GL', length.out = nrow(tr))
        tr <- tr %>% select(instrument_name, DateTime, lc, longitude, latitude, longitudeError, latitudeError)
        names(tr) <- c('id','date','lc','lon','lat','lonerr','laterr')

        time_step = 12
        df.locs <- split(tr, tr$id)
        ssm_fit <- lapply(df.locs, function(x) foieGras::fit_ssm(x, model='crw', time.step = time_step, vmax=10))
        idx <- lapply(ssm_fit, function(x) x$converged) %>% do.call(rbind, .)
        if (any(!idx)){
          for (ii in which(!idx)){
            logger::log_info('Some initial use of fit_ssm() did not converge. Trying other time_step values.')
            #df.ii <- foieGras::grab(ssm_fit[[ii]], 'data', as_sf=F)
            df.ii <- df.locs[[ii]]
            #df.ii <- ssm_fit[[ii]]$ssm[[1]]$data
            df.ii <- data.frame(df.ii[,names(df)])

            # iterate through some reasonable time steps
            poss.tstep <- c(1,3,6,12,24)
            poss.tstep <- poss.tstep[-which(poss.tstep %in% time_step)]
            for (bb in poss.tstep){
              try_fit <- foieGras::fit_ssm(df.ii, model='crw', time.step = bb, vmax=10, optim='nlminb')
              if (try_fit$converged){
                ssm_fit[[ii]] <- try_fit
                logger::log_info(paste('Converged on time step', bb, 'hours.'))
                break
              } else if (!try_fit$converged & bb == 24){
                logger::log_info('None of the alternative time steps resulted in fit_ssm() model convergence.')
                logger::log_info('Cancelling simulations.')
                sim <- FALSE; tmp_out <- NULL
              } else{
                next
              }
            } # bb
          } # which idx
        } # if any

        ## grab predicted locations output from fit_ssm
        plocs <- lapply(ssm_fit, FUN=function(x) foieGras::grab(x, what = "p", as_sf = FALSE)) %>%
          do.call(rbind, .) %>%
          tbl_df() %>%
          mutate(id = as.character(id)) %>% group_by(id)

        ## subsample from predicted locations
        plocs <- split(plocs, plocs$id)

        for (tt in 1:length(plocs)){
          res.tt <- as.numeric(difftime(plocs[[tt]]$date[2], plocs[[tt]]$date[1], units = 'hours'))
          res_slice <- res_out / res.tt
          #if (res_slice < 1) res_slice <- 1
          plocs[[tt]] <- plocs[[tt]] %>% do(slice(., seq(1, n(), by = res_slice)))
        }

        plocs <- plocs %>%
          do.call(rbind, .) %>%
          tbl_df() %>%
          mutate(id = as.character(id)) %>% group_by(id)

        tr <- plocs[,c('date','lat','lon','y.se','x.se')]
        names(tr) <- c('DateTime','latitude','longitude','latitudeError','longitudeError')
        tr$instrument_name <- etuff$meta$instrument_name
        tr$platform <- etuff$meta$platform
        tr <- tr %>% select(c('instrument_name', 'platform','DateTime','latitude','latitudeError','longitude','longitudeError')) %>% as.data.frame()

        ## approx error in dec degrees from meters
        tr$latitudeError <- tr$latitudeError / 110
        tr$longitudeError <- tr$longitudeError / 110
      } else if (interp_method == 'interval'){

        date_vec <- seq.POSIXt(track$DateTime[1], track$DateTime[nrow(track)], by=paste0(res_out, ' hours'))
        date_idx <- findInterval(track$DateTime, date_vec)
        tr <- track[!duplicated(date_idx),]
        tr$instrument_name <- etuff$meta$instrument_name
        tr$platform <- etuff$meta$platform
        tr <- tr %>% select(c('instrument_name', 'platform','DateTime','latitude','latitudeError','longitude','longitudeError')) %>% as.data.frame()
      }

    } else if(res_in == res_out){
      logger::log_info(paste0('Input and output resolution are equal (', res_in, ' and ', res_out, ', respectively. No change needed.'))
    }



  #} else if(all(names(track) %in% names('DateTime', 'latitude','longitude'))){
  #  tr <- track[,c('DateTime','latitude','longitude')]
  #  names(tr)[1] <- 'track_dt'
  } else{
    logger::log_warn('No method found for converting output of get_track() to interpolated movement data.')
  }

  return(tr)
}
