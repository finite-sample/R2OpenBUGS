as.mcmc.list.bugs <- function(x, ...) {
  if (!inherits(x, "bugs")) {
    cli::cli_abort("{.fn as.mcmc.list.bugs} needs a {.cls bugs} object.")
  }
  if (dim(x$sims.array)[2] != x$n.chains) {
    cli::cli_abort(
      "The {.cls bugs} object's {.field sims.array} and {.field n.chains} disagree on the chains."
    )
  }
  mclis <- vector("list", x$n.chains)
  strt <- x$n.burnin + 1
  end <- x$n.iter
  ord <- order(dimnames(x$sims.array)[[3]])
  for (i in 1:x$n.chains) {
    tmp1 <- x$sims.array[, i, ord]
    mclis[[i]] <- mcmc(tmp1, start = strt, end = end, thin = x$n.thin)
  }
  as.mcmc.list(mclis)
}
