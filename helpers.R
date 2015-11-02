# helpers.R
# miscellaneous functions of use in analysis and plotting

library(rstan)
library(ggplot2)
library(GGally)
library(grid)

pairplot <- function(pt_estimates, density_samples, variance_samples, indices) {
  # make a pairs plot
  # lower triangle plots are scatterplots of point estimates over density plots of posterior samples
  # from the population
  # diagonals are densities of point estimates and posteriors from population
  # upper triangle plots are posterior densities of correlation coefficients
  # pt_estimates are point estimates for individuals within the population, named by columns
  # density samples is a data frame of samples from the posterior for each of these variables
  # indices is a vector of which indices to plot

  vnames <- names(pt_estimates)[indices]
  P <- length(vnames)
  p <- ggpairs(pt_betas[, indices]) + theme_bw()

  for (r in 1:P) {
    for (c in 1:P) {
      pp <- getPlot(p, r, c)  # get previous plot in this cell
      if (r == c) {
        gg <- ggplot() + geom_density(data=density_samples, aes_q(x=as.name(vnames[c])), color='blue') +
          geom_density(data=pt_estimates, aes_q(x=as.name(vnames[c]))) + theme_bw()
        p <- putPlot(p, gg, r, c)
      } else if (r > c) {
        newplot <- geom_density2d(data=density_samples, aes_q(x=as.name(vnames[c]), y=as.name(vnames[r])))
        pp$layers <- c(newplot, pp$layers)  # density to add
        p <- putPlot(p, pp, r, c)
      } else {
        linind <- indices[r] + (indices[c] - 1) * dim(pt_estimates)[2]
        dd <- data.frame(v=variance_samples[, linind])
        gg <- ggplot(data=dd) + geom_vline(xintercept=0) + geom_density(aes(x=v), color='red') + xlim(-1, 1) + theme_bw()
        p <- putPlot(p, gg, r, c)
      }
    }
  }
  return(p)
}
