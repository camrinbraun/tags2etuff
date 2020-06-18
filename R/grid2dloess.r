#' LOESS smooth for tag PDT data
#'
#' 2-dimensional loess gridder. The smoothed value at each grid point
#' is found from a weighted least-squares regression of the points within
#' +/- SPAN_X and SPAN_Y of the grid point to a quadratic surface.
#'
#' @param data is a row vector array of the data to be smoothed. Missing value flag is NaN.
#' @param xgrid is x locations of DATA. This must be the same size as DATA array.
#' @param ygrid is y locations of DATA. This must be the same size as DATA array.
#' @param span_x,span_y is filter half-power points (each a scalar). The larger the number, the more smoothing is performed. For the tricubic weighting function used here, the smoothing is approximately equivalent to using a running average of length equal to ~0.6*SPAN_X. However, the spectral characteristics of this smoother are usually much more desirable. The filter cutoff frequencies are approximately (1/SPAN_X, 1/SPAN_Y).
#' @param xgrid_est,ygrid_est are column and row vectors, respectivly, where smoothed estimates are desired. The estimate grid can be irregular and non-monotonic. Any points in XGRID_EST and YGRID_EST outside of the range of XGRID and YGRID, respectively, will have SM_DATA=NaN.
#' @param flagout is logical indicating whether you want to output FLAG as described below
#'
#' @return sm_data is a 2-dimensional array with LENGTH(XGRID_EST) columns and LENGTH(YGRID_EST) rows with the smoothed DATA.
#' @return flag is an array the same size as SM_DATA that is set to 1 when the smoothed estimate is outside the range of the data within +/- SPAN_X of that grid point and 0 otherwise. When the smoothed estimate is out of range, the estimate will be included in the output SM_DATA. This will typically occur near the edges of the DATA series or when SPAN_X only encompasses a small number of grid points in XGRID. While the smoothed estimate is usually only marginally out-of-range in these cases, care should be used when considering these points because the smoothed estimate may not be very good at that particular point. If there are many such points, consider using a larger SPAN_X (smoothing over more points).
#' @note Translated to R by Camrin Braun, April 2014. Written by Peter Gaube in MatLab, December 8, 2009. Based on smooth2d_loess.m by Larry O'Neill, September 25, 2007

grid2dloess <- function(data, xgrid, ygrid, span_x, span_y,
                         xgrid_est = NULL, ygrid_est = NULL, flagout = TRUE){

  if(is.null(ygrid_est)){
    ygrid_est <- 0:round(max(ygrid, na.rm = T), -2)
  }
  ygrid_out <- ygrid_est

  if(is.null(xgrid_est)){
    xgrid_est <- seq(min(xgrid), max(xgrid), by = 1)
  }
  xgrid_out <- xgrid_est

  ny = length(data)

  xgrid_size = dim(matrix(xgrid))
  ygrid_size = dim(matrix(ygrid))

  xgrid_est_size = dim(matrix(xgrid_est))
  ygrid_est_size = dim(matrix(ygrid_est))

  #
  # Check that XGRID_EST, YGRID_EST, XGRID, and YGRID are vectors
  #

  if (!is.vector(xgrid_est)){
    stop('XGRID_EST must be a 1-D vector.')
  }
  if (!is.vector(ygrid_est)){ #not exactly what we want here, 2D would slide
    stop('YGRID_EST must be a 1-D vector.')
  }
  if (!is.vector(xgrid_size)){
    stop('XGRID must be a 1-D vector.')
  }
  if (!is.vector(ygrid_size)){
    stop('YGRID must be a 1-D vector.')
  }

  #
  # Make input vectors row vectors
  #

  if (xgrid_est_size[1] < xgrid_est_size[2]){
    xgrid_est = t(xgrid_est)
  }
  if (ygrid_est_size[1] < ygrid_est_size[2]){
    ygrid_est = t(ygrid_est)
  }
  if (xgrid_size[1] < xgrid_size[2]){
    xgrid = t(xgrid)
  }
  if (ygrid_size[1] < ygrid_size[2]){
    ygrid = t(ygrid)
  }

  nx_est = length(xgrid_est)
  ny_est = length(ygrid_est)

  #
  # Check to see if XGRID and YGRID have a length consistent with DATA
  #

  if (length(xgrid) != ny){
    stop('XGRID is not equal to the size of DATA')
  }

  if (length(ygrid) != ny){
    stop('YGRID is not equal to the size of DATA')
  }


  #
  # Only consider points where XGRID_EST and YGRID_EST are within the range
  # of XGRID and YGRID, respectively
  #

  sx = t(which(xgrid_est >= min(xgrid,na.rm=T) & xgrid_est <= max(xgrid,na.rm=T)))
  sy = t(which(ygrid_est >= min(ygrid,na.rm=T) & ygrid_est <= max(ygrid,na.rm=T)))

  mxgrid = xgrid
  mygrid = ygrid

  #
  # Normalize MXGRID and XGRID_EST by SPAN_X and
  # MYGRID and YGRID_EST by SPAN_Y
  #

  mxgrid = mxgrid / span_x
  xgrid_est = xgrid_est / span_x
  mygrid = mygrid / span_y
  ygrid_est = ygrid_est / span_y

  #
  # Preallocate the output arrays
  #

  sm_data = matrix(ncol = nx_est, nrow = ny_est)

  # Preallocate NaNs for output SM_DATA array

  if (flagout){
    flag = matrix(0, nrow = ny_est, ncol = nx_est)
    # Preallocate 0's for output FLAG array
  }

  #
  # Find locations of missing data, marked by NaN's
  #

  nan_data_loc = is.na(data)
  if (any(nan_data_loc)){
    warning("DATA array contains nothing but NAN''s", call. = FALSE)
  } else{
    nan_data_loc = 0
  }


  #
  # Loop through all points in the estimate grid
  #
  pb <- txtProgressBar(min = min(sy), max = max(sy), style = 3)

  for (j in sy){
    dy = mygrid - ygrid_est[j]
    dy2 = dy * dy

    for (i in sx){
      # how many data points available for regression
      dx = mxgrid - xgrid_est[i]
      dist = dx * dx + dy2
      igood = (dist + nan_data_loc) < 1
      ngood = sum(as.vector(igood), na.rm = T)
      if (is.na(ngood)){
        ngood = 0
      }

      if (ngood >= 10){    # Need at least 10 data points for regression
        datasel = data[igood]
        dxsel = dx[igood]
        dysel = dy[igood]
        distsel2 = dist[igood]
        distsel = sqrt(distsel2)

        #
        # Use the tricubic weighting function for the filter weights. This
        # is a more computationally efficient way of writing out
        # w = (1-distsel.^3).^3;
        #

        w = 1 - distsel2 * distsel
        w = w * w * w

        #
        # Compute array of w*(1+x+y+x^2+y^2+xy)
        #

        #equivalent to matlab's repmat function to repeat a matrix
        xin = kronecker(matrix(1,1,6), w)

        xin[,2] = xin[,2] * dxsel
        xin[,3] = xin[,3] * dysel
        xin[,4] = xin[,2] * dxsel
        xin[,5] = xin[,3] * dysel
        xin[,6] = xin[,2] * dysel

        #
        # Least-squares solution to the over-determined set of
        # equations as specified by the "\" operator. Basically solves
        # the equation wYhat=B*(wX), where Yhat is the smoothed estimate
        # of the input DATA, X is the coordinate relative to the grid
        # point of the smoothed estimate, and B are the regression
        # coefficients. Solves with QR decomposition.
        # R differs from MatLab here in that mldivide will fail
        # if the input matrix is close to singular where the QR
        # decomp in MatLab adds an NA and continues the script.
        # Thus, I've added in the try() function with errors suppressed
        # and added a numeric check afterward to assign B=NA
        # if mldivide() fails.
        #

        B = try(pracma::mldivide(xin, (w * datasel)), silent = T)

        if(!is.numeric(B)){
          B = NA
        }

        #
        # Smoothed value is just the first regression coefficient, since
        # the grid point was chosen such that it's at x=0. Also need to
        # check that the regression point is within the range of the data
        # points used in fitting the quadratic surface. It should be out
        # of range only rarely, and if it is, the smoothed estimate at
        # that point is given and the FLAG is set to =1.
        #

        sm_data[j,i] = as.numeric(B[1])

        if (flagout){
          if (B[1] > min(datasel) & B[1] < max(datasel)){
            flag[j,i] = 1
          }
        }
      }
    }

    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, j)

  }

  close(pb)
  result = list(sm_data = sm_data, flag = flag, xgrid = xgrid_out, ygrid = ygrid_out)
  return(result)
  # end function
}
