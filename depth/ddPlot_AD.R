

 


library(DepthProc)


ddPlot_AD <- function(x, y, scale = FALSE, location = FALSE, name = "X", 
                   name_y = "Y", title = "Depth vs. depth plot",
                   depth_params = list()) {
  
  if (ncol(x) != ncol(y)) {
    stop("Wrong dimensions of the datasets! ncol(x) != ncol(y)")
  }
  if (scale) {
    uxname_list_x <- list(u = x, X = x)
    uxname_list_y <- list(u = y, X = y)
    depth_sample_x <- do.call(depth, c(uxname_list_x, depth_params))
    depth_sample_y <- do.call(depth, c(uxname_list_y, depth_params))
    varcovx <- cov(x[which(depth_sample_x >= median(depth_sample_x)), ])
    varcovy <- cov(y[which(depth_sample_y >= median(depth_sample_y)), ])
    x_new <- t(solve(chol(varcovx)) %*% t(x))
    y_new <- t(solve(chol(varcovy)) %*% t(y))
  } else {
    x_new <- x
    y_new <- y
  }
  if (location) {
    medx <- depthMedian(x_new, depth_params)
    medy <- depthMedian(y_new, depth_params)
    x_new <- sweep(x_new, 2, medx, "-")
    y_new <- sweep(y_new, 2, medy, "-")
  }
  
  data <- rbind(x_new, y_new)
  uxname_list_x_new <- list(u = data, X = x_new)
  uxname_list_y_new <- list(u = data, X = y_new)
  depth_x <- do.call(depth, c(uxname_list_x_new, depth_params))
  depth_y <- do.call(depth, c(uxname_list_y_new, depth_params))
  
  ddplotA <-data.frame(depth_x,depth_y)
  
  return(ddplotA)
}

