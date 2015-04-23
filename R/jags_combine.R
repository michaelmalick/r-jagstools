#' @title Combine individual MCMC chains into an mcmc.list object
#'
#' @description
#'      \code{jags_combine} is a helper function for running jags in parallel
#'      that combines the individual chains run on separate cores into an
#'      mcmc.list object. This function is supplied to the .combine argument in
#'      the foreach function.
#'
#' @param \dots
#'      mcmc object
#'  
#' @return
#'      An \code{mcmc.list}
#'
#' @seealso \code{\link{mcmc}} \code{\link{mcmc.list}}
#' 
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#' \dontrun{jags_combine(result1, result2, result3)}
#'
jags_combine <- function(...)
     return(coda::as.mcmc.list(sapply(list(...), coda::mcmc)))
