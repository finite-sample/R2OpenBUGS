# R2OpenBUGS

[![CRAN status](https://www.r-pkg.org/badges/version/R2OpenBUGS)](https://CRAN.R-project.org/package=R2OpenBUGS)
[![R-CMD-check](https://github.com/finite-sample/R2OpenBUGS/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/finite-sample/R2OpenBUGS/actions/workflows/R-CMD-check.yml)

R2OpenBUGS runs an [OpenBUGS](https://www.mrc-bsu.cam.ac.uk/software/bugs-project/)
model from R. You write the model in the BUGS language, pass data and initial
values from R, and get back the simulations as R arrays, with a summary table
of each parameter's posterior mean, quantiles, and the convergence statistics
`Rhat` and effective sample size, plus plots of both.

It is the OpenBUGS counterpart of
[R2WinBUGS](https://github.com/finite-sample/R2WinBUGS).

## Installation

OpenBUGS itself must be installed separately (version 3.2.2 or later). On
Linux, build it from the
[3.2.3 source tarball](https://github.com/chjackson/openbugsdoc); on Windows,
use the installer from the BUGS project.

```r
# Released version
install.packages("R2OpenBUGS")

# Development version
# install.packages("pak")
pak::pak("finite-sample/R2OpenBUGS")
```

## Example

The eight-schools model (Gelman et al., *Bayesian Data Analysis*): the
estimated effect of coaching on SAT scores in eight schools, each with its
standard error, pooled through a hierarchical normal model.

```r
library(R2OpenBUGS)

model_file <- system.file("model", "schools.txt", package = "R2OpenBUGS")
data(schools)

J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd

inits <- function() {
  list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100),
       sigma.theta = runif(1, 0, 100))
}

fit <- bugs(
  data = list("J", "y", "sigma.y"),
  inits = inits,
  parameters.to.save = c("theta", "mu.theta", "sigma.theta"),
  model.file = model_file,
  n.chains = 3,
  n.iter = 5000
)
print(fit)
plot(fit)
```

## Citation

Sturtz, S., Ligges, U., and Gelman, A. (2005). R2WinBUGS: A Package for
Running WinBUGS from R. *Journal of Statistical Software*, 12(3), 1–16.
[doi:10.18637/jss.v012.i03](https://doi.org/10.18637/jss.v012.i03)

## License

GPL-2.
