#' Interpolate 2d data from eTUFF
#'
#' Interpolate spatial movements to desired resolution
#'
#' @param etuff_file is path to etuff file that will be read to an object of class etuff or etuff archival
#' @param temp_res (optional) is temporal resolution as valid argument to "by" in \code{seq.POSIXct}
#' @param res_out (optional) is numeric indicating desired temporal resolution of output track in hours.
#' @return a dataframe of interpolated 2d movements
#' @export
#'
interp_track <- function(etuff_file,...){

  args <- list(...)
  etuff <- read_archival(etuff_file)

  ## read whatever track data is in the etuff file
  track <- get_track(etuff)

  ## temporal res of output track
  if('res_out' %in% args){
    res_out <- args$res_out
    log_info(paste0('res_out set to ', res_out, ' hours.'))
  } else{
    log_info(paste0('res_out not specified. Fixing temporal resolution of output to 24 hours.'))
    res_out <- 24
  }


  #-------------------
  ## raw Argos with LCs
  #-------------------
  if (etuff$meta$instrument_type == 'satellite' |
      (etuff$meta$instrument_type == 'popup' & etuff$meta$waypoints_source == 'Argos')){
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
        log_info('Some initial use of fit_ssm() did not converge. Trying other time_step values.')
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
            log_info(paste('Converged on time step', bb, 'hours.'))
            break
          } else if (!try_fit$converged & bb == 24){
            log_info('None of the alternative time steps resulted in fit_ssm() model convergence.')
            log_info('Cancelling simulations.')
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

    ## subsample from predicted locations, if applicable
    if (!is.null(res_out)){
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

    }

    tr <- plocs[,c('date','lat','lon','y.se','x.se')]
    names(tr) <- c('DateTime','latitude','longitude','latitudeError','longitudeError')
    tr <- tr %>% select(c('DateTime','latitude','latitudeError','longitude','longitudeError'))

    ## approx error in dec degrees from meters
    tr$latitudeError <- tr$latitudeError / 110
    tr$longitudeError <- tr$longitudeError / 110

  #-------------------
  ## PSAT with modeled locs (i.e. GPE3, HMMoce, etc)
  #-------------------
  } else if (etuff$meta$instrument_type %in% c('popup') & etuff$meta$waypoints_source == 'modeled'){

    tr <- track %>% select(c('DateTime','latitude','latitudeError','longitude','longitudeError'))

    ## check temporal resolution and adjust accordingly
    ## if no temporal resolution is specified, try to detect it (this should nearly always work with a PSAT tag)
    if (is.null(temp_res)){
      temp_res <- Mode(as.numeric(diff(tr$DateTime)))
      log_info(paste('No temporal resolution specified. Mode of diff(timeseries) yielded ', temp_res, 'seconds.', sep=''))
    }

  } else if(all(names(track) %in% names('DateTime', 'latitude','longitude'))){
    tr <- track[,c('DateTime','latitude','longitude')]
    names(tr)[1] <- 'track_dt'
  } else{
    logger::log_warn('No method found for converting output of get_track() to interpolated movement data.')
  }

  return(tr)
}
