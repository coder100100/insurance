
#' @importFrom geometry convhulln
#'
#' @description Draws a scale curve: measure of dispersion.
#'
#' @param x Multivariate data as a matrix.
#' @param y Additional matrix with multivariate data.
#' 
#' ######**** alpha = p  ################
#' @param alpha = p  Vector with values of central area to be used in computation.
#' 
#' 
#' @param name Name of matrix X used in legend.
#' @param name_y Name of matrix Y used in legend.
#' @param title title of the plot.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#'
#' @details

####################################################################################################

scaleCurve_AD <- function(x, y = NULL, alpha = seq(0, 1, 0.01), name = "X",
                       name_y = "Y", title = "Scale Curve",
                       depth_params = list(method = "Projection")) {
  
  
  
  library(DepthProc)
  library(geometry)
  
  x <- na.omit(x)
  
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("x must be a matrix or data frame!")
  }
  if (!is.null(y)) {
    
    if (is.data.frame(y)) {
      y <- as.matrix(y)
    }
    if (!is.matrix(y)) {
      stop("y must be a matrix or data frame!")
    }
  } 
  
  dim_x <- dim(x)[2]
  
  uxname_list <- list(u = x, X = x)
  
  depth_est <- do.call(depth, c(uxname_list, depth_params))
  
  k <- length(alpha)
  vol <- 1:k
  
  alpha_border <- ecdf(depth_est)(depth_est)
  for (i in 1:k) {
    tmp_x <- x[alpha_border >= alpha[i], ]
    np <- nrow(as.matrix(tmp_x))
    
    if (np > dim_x) {
      vol[i] <- convhulln(tmp_x, options = "FA")$vol
    } else {
      vol[i] <- 0
    }
  }
  
  scale_curve <- new("ScaleCurve", rev(vol), alpha = alpha, depth = depth_est,
                     title = "")
  
  if (!is.null(y)) {
    name <- name_y
    sc_tmp <- scaleCurve(x = y, y = NULL, alpha = alpha, name = name,
                         name_y = "Y", depth_params = depth_params)
    scale_curve <- combineDepthCurves(scale_curve, sc_tmp)
  }
  
  
  Volume<-rev(vol)
  d_A<-data.frame(Volume,alpha)
  return(d_A)
  
  
}




