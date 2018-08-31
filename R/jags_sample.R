#' @title Run JAGS models with each MCMC chain computed in serial or parallel
#'
#' @description
#'      \code{jags_sample} is a wrapper for functions in the \code{rjags}
#'      package that allows for running JAGS from R with each MCMC chain
#'      computed serially or in parallel using the foreach package.
#'
#' @param data
#'      a list containing the data for the model; see
#'      \code{?rjags::jags.model} for details.
#'
#' @param inits
#'      a list or function giving the initial values for each variable; see
#'      \code{?rjags::jags.model} for details.
#'
#' @param file
#'      path to the file containing the JAGS model; see
#'      \code{?rjags::jags.model} for details.
#'
#' @param variable.names
#'      character vector of names of variables to monitor in JAGS; see
#'      \code{?rjags::coda.samples} for details.
#'
#' @param n.chains
#'      number of MCMC chains to run; see \code{?rjags::jags.model} for
#'      details.
#'
#' @param n.adapt
#'      number of iterations for adaption; see \code{?rjags::jags.model} for
#'      details.
#'
#' @param burnin
#'      number of iterations to use for burnin of the MCMC chains. The burnin
#'      iterations are not saved.
#'
#' @param n.iter
#'      number of samples for each Markov chain; see
#'      \code{?rjags::coda.samples} for details.
#'
#' @param thin
#'      thinning interval; see \code{?coda.samples} for details.
#'
#' @param load.modules
#'      JAGS modules to load before running model; see
#'      \code{?rjags::load.modules} for details.
#'
#' @param method
#'      string identifying method for computation; either 'serial' or
#'      'parallel'; defaults to 'serial'.
#'
#' @param progress.bar
#'      type of progress bar to use; see \code{rjags::update.jags} for details
#'
#' @param parallel
#'      a list of arguments for parallel computing:
#'      \itemize{
#'        \item{\code{n.clusters }}{number of cluster to use for computation;
#'          currently needs to be set equal to \code{n.chains}.}
#'        \item{\code{RNGseed }}{seed for the RNG stream sent to the
#'              clusters; setting this to a positive integer allows
#'              the chains to be reproducible.}
#'        \item{\code{type }}{type of cluster to create; either "PSOCK" or
#'          "FORK", or "MC"; Both "PSOCK" and "FORK" are based on the
#'          \code{snow} package, whereas "MC" is based on the \code{multicore}
#'          package; type = "MC" is likely not available for Windows machines;
#'          see \code{?parallel::makeCluster} for more details}
#'        \item{\code{verbose }}{logical, should output from each cluster be
#'          sent to terminal. Can be useful for debugging.}
#'      }
#'
#'
#' @details
#'      If method = 'serial' the function runs JAGS using the
#'      \code{rjags::coda.samples} function. If method = 'parallel' the function
#'      uses the \code{parallel} and \code{foreach} packages to send each chain
#'      to different clusters. Unfortunately, when the parallel type = 'PSOCK'
#'      no progress bars are available. If parallel type = 'FORK' and verbose =
#'      TRUE, progress bars should be printed to the terminal among other
#'      potentially useful output.
#'
#'      Because the funtion returns an mcmc.list object, the model (as defined by
#'      \code{rjags::jags.model}) is not retained and therefore, updating or
#'      extending the MCMC runs is currently unavailable. Adding this
#'      functionality would require saving the model object, which would make
#'      the return not an mcmc.list object and would likely require the creation
#'      of a new class and additional functions to deal with this new class.
#'
#' @return
#'      An \code{mcmc.list}
#'
#' @seealso \code{\link{jags.model}}
#'          \code{\link{coda.samples}}
#'          \code{\link{mcmc.list}}
#'          \code{\link{load.module}}
#'          \code{\link{unload.module}}
#'
#' @export
#'
#' @author Michael Malick
#'
#' @examples
#'
#' # ----------------------------
#' # Simulate data
#' # ----------------------------
#'
#' library(rjags)
#' library(foreach)
#'
#' ## True parameter values
#' alpha <- 3
#' beta  <- 0.5
#' sigma <- 5.0
#' N     <- 100
#'
#' ## Simulate data
#' set.seed(129)
#' x       <- runif(N, 5, 95)
#' epsilon <- rnorm(N, 0, sigma)
#' y       <- alpha + beta*x + epsilon
#'
#' # Return data in a list
#' dat <- list(N = N, x = x, y = y)
#'
#'
#'
#' # ----------------------------
#' # Setup JAGS model and write
#' # to file
#' # ----------------------------
#' model.string <-
#'     'model {
#'         for (i in 1:N){
#'             y[i] ~ dnorm(mu.y[i], tau)
#'             mu.y[i] <- alpha + beta * x[i]
#'         }
#'         # Priors
#'         alpha ~ dnorm(0, .0001)
#'         beta  ~ dnorm(0, .0001)
#'         tau   <- pow(sigma, -2)
#'         sigma ~ dunif(0, 100)
#'     }'
#' writeLines(model.string, con = "example_jags.bug")
#'
#'
#' # ----------------------------
#' # Setup monitors and inits
#' # ----------------------------
#' ## Parameters to monitor
#' params <- c("alpha", "beta", "sigma", "deviance")
#'
#' ## Setup function for initial values
#' inits <- function() {
#'     list(
#'         .RNG.name  = "lecuyer::RngStream",
#'         .RNG.seed  = runif(1, 0, 2^31),
#'         alpha = rnorm(1), beta = rnorm(1), sigma = runif(1))
#' }
#'
#'
#' # ----------------------------
#' # Sample all chains in serial
#' # ----------------------------
#' fit.s <- jags_sample(
#'     data = dat,
#'     inits = inits,
#'     file = "example_jags.bug",
#'     variable.names = params,
#'     n.chains = 2,
#'     n.adapt = 100,
#'     burnin = 100,
#'     n.iter = 1000,
#'     thin = 1,
#'     load.modules = c("dic", "lecuyer"),
#'     method = "serial",
#'     progress.bar = "text")
#'
#' # ----------------------------
#' # Sample all chains in parallel
#' # ----------------------------
#' fit.p <- jags_sample(
#'     data = dat,
#'     inits = inits,
#'     file = "example_jags.bug",
#'     variable.names = params,
#'     n.chains = 2,
#'     n.adapt = 100,
#'     burnin = 100,
#'     n.iter = 1000,
#'     thin = 1,
#'     load.modules = c("dic", "lecuyer"),
#'     method = "parallel",
#'     progress.bar = "text",
#'     parallel = list(
#'         n.clusters = 2,
#'         RNGseed = 123,
#'         type = "PSOCK",
#'         verbose = TRUE))
#'
#' fit.p <- jags_sample(
#'     data = dat,
#'     inits = inits,
#'     file = "example_jags.bug",
#'     variable.names = params,
#'     n.chains = 2,
#'     n.adapt = 100,
#'     burnin = 100,
#'     n.iter = 1000,
#'     thin = 1,
#'     load.modules = c("dic", "lecuyer"),
#'     method = "parallel",
#'     progress.bar = "text",
#'     parallel = list(
#'         n.clusters = 2,
#'         RNGseed = 123,
#'         type = "MC",
#'         verbose = TRUE))


## jags_sample ---------------------------------------------
jags_sample <- function(data,
                        inits,
                        file,
                        variable.names,
                        n.chains = 1,
                        n.adapt = 100,
                        burnin = 100,
                        n.iter = 1000,
                        thin = 1,
                        load.modules = NULL,
                        method = "serial",
                        progress.bar = "text",
                        parallel = list(n.clusters = n.chains,
                                        RNGseed = 123,
                                        type = "PSOCK",
                                        verbose = FALSE)) {

    if(!method %in% c("serial", "parallel"))
        stop("method must be either 'serial' or 'parallel'")

    if(method == "serial") {
        out <- jags_serial(data = data,
                           inits = inits,
                           file = file,
                           variable.names = variable.names,
                           n.chains = n.chains,
                           n.adapt = n.adapt,
                           burnin = burnin,
                           n.iter = n.iter,
                           thin = thin,
                           load.modules = load.modules,
                           progress.bar = progress.bar)
    }

    if(method == "parallel") {
        out <- jags_parallel(data = data,
                             inits = inits,
                             file = file,
                             variable.names = variable.names,
                             n.chains = n.chains,
                             n.adapt = n.adapt,
                             burnin = burnin,
                             n.iter = n.iter,
                             thin = thin,
                             load.modules = load.modules,
                             progress.bar = progress.bar,
                             parallel = parallel)
    }
    return(out)
}



## jags_parallel -------------------------------------------
#' @rdname jags_sample
#' @export

jags_parallel <- function(data,
                          inits,
                          file,
                          variable.names,
                          n.chains = 1,
                          n.adapt = 100,
                          burnin = 100,
                          n.iter = 1000,
                          thin = 1,
                          load.modules = NULL,
                          progress.bar = "text",
                          parallel = list(n.clusters = n.chains,
                                          RNGseed = 123,
                                          type = "PSOCK",
                                          verbose = FALSE)) {

    ## Set 'parallel' arguments
    arg.par <- list(n.clusters = n.chains,
                    RNGseed = 123,
                    type = "PSOCK",
                    verbose = FALSE)
    ind <- which(!names(arg.par) %in% names(parallel))
    parallel <- c(arg.par[ind], parallel)


    n.clusters <- parallel$n.clusters

    if(n.clusters != n.chains)
        stop("Number of chains must equal number of clusters")

    if(parallel$verbose)
        outfile <- ""
    else
        outfile <- "/dev/null"

    if(parallel$type == "PSOCK" || parallel$type == "FORK") {
        ## Make cluster
        if(parallel$type == "PSOCK")
            cl <- parallel::makeCluster(n.clusters,
                                        methods = FALSE,
                                        type = "PSOCK",
                                        outfile = outfile)

        if(parallel$type == "FORK")
            cl <- parallel::makeCluster(n.clusters,
                                        methods = FALSE,
                                        type = "FORK",
                                        outfile = outfile)

        doParallel::registerDoParallel(cl)
        parallel::clusterSetRNGStream(cl, parallel$RNGseed)
    }

    ## Need to manual handle RNG for 'multicore'
    seeds <- vector("list", n.clusters)
    if(parallel$type == "MC") {
        doMC::registerDoMC(n.clusters)
        RNGkind("L'Ecuyer-CMRG")
        set.seed(parallel$RNGseed)
        seeds[[1]] <- .Random.seed
        if(n.clusters > 1)
            for(i in 2:n.clusters)
                seeds[[i]] <- parallel::nextRNGStream(seeds[[i - 1]])
    }

    samp <- foreach::foreach(i = iterators::icount(n.clusters),
                             .combine = jags_combine,
                             .packages = c("rjags"),
                             .inorder = FALSE,
                             .export = c("jags_serial"),
                             .multicombine = TRUE) %dopar% {

        if(parallel$type == "MC")
            .Random.seed <- seeds[[i]]

        ## Setup inits into a list
        inits.list <- logical()
        if(is.null(inits))
            inits.list <- NULL
        if(is.list(inits) && length(inits) == n.chains)
            inits.list <- inits[[i]]
        if(is.list(inits) && length(inits) != n.chains)
            inits.list <- inits
        if(is.function(inits) && is.null(formals(inits)))
            inits.list <- inits()
        if(is.function(inits) && any(names(formals(inits)) == "chain"))
            inits.list <- inits(chain = i)
        if(is.logical(class(inits.list)))
            stop("inits are invalid: see ?rjags::jags.model for details")

        fit <- jags_serial(data = data,
                           inits = inits.list,
                           file = file,
                           n.chains = 1,
                           n.adapt = n.adapt,
                           burnin = burnin,
                           n.iter = n.iter,
                           thin = thin,
                           variable.names = variable.names,
                           load.modules = load.modules,
                           progress.bar = progress.bar)
        return(fit)
    }

    if(parallel$type == "PSOCK" || parallel$type == "FORK")
        parallel::stopCluster(cl)

    return(samp)
}



## jags_serial ---------------------------------------------
#' @rdname jags_sample
#' @export

jags_serial <- function(data,
                        inits,
                        file,
                        variable.names,
                        n.chains = 1,
                        n.adapt = 100,
                        burnin = 100,
                        n.iter = 1000,
                        thin = 1,
                        load.modules = NULL,
                        progress.bar = "text") {

    ## Load modules
    if(!is.null(load.modules))
        for(i in 1:length(load.modules))
            rjags::load.module(load.modules[i], quiet = TRUE)

    ## Setup jags model
    model <- rjags::jags.model(data = data,
                               inits = inits,
                               n.chains = n.chains,
                               n.adapt = n.adapt,
                               quiet = TRUE,
                               file = file)

    ## Burnin
    if(burnin > 0)
        stats::update(model,
                      n.iter = burnin,
                      progress.bar = progress.bar)

    ## Sample posterior
    samp <- rjags::coda.samples(model = model,
                                n.iter = n.iter,
                                thin = thin,
                                variable.names = variable.names,
                                progress.bar = progress.bar)

    return(samp)
}
