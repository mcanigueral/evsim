
#' Parallel processing for windows
#'
#' mclapply version that works on windows
#'
#' @param X a vector (atomic or list) or an expressions vector.
#' Other objects (including classed objects) will be coerced by as.list.
#' @param FUN the function to be applied to (mclapply) each element of X.
#' @param ... For mclapply, optional named arguments to FUN.
#' @param mc.cores integer, number of cores to be used.
#' Could be overruled if number of items in list is lower.
#'
#' @importFrom parallel makeCluster parLapply stopCluster
#'
#' @return list
#' @keywords internal
#'
#' @details
#' This is a workaround to allow parallel processing in windows since
#' `parallel` package only works with linux distributions.
#' Source: https://www.r-bloggers.com/2014/07/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/
#'
#'
mclapply.windows <- function (X, FUN, ..., mc.cores) {

  # do not use more cores than items in the list
  cores <- min(length(X), as.integer(mc.cores))

  # create cluster for multiprocessing
  cl <- makeCluster(cores, type="PSOCK") # number of cores

  # in case the code crashes the cluster needs to be closed properly
  # so we catch every error allowing us the chance to change things
  # finally also runs if no errors
  results <- tryCatch( {
    return( parLapply(cl, X, FUN, ...) )
  },
  error = function(err) {print(err)},
  warning = function(war) {print(war)},
  finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
  return(results)
}

