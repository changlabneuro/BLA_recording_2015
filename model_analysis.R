# model_analysis.R
# analysis of Bayesian models fit with STAN

library(rstan)

# load original data
load("data/countdata")

# load model samples
fname <- "fitobj_targacq_multi_t"
load(fname)

# calculate some useful quantities
U <- length(unique(countdata$unit))
P <- dim(X)[2]
# TT <- length(unique(countdata$outcome))
# vnames <- paste(rep(levels(countdata$outcome), each=P), rep(colnames(X), TT), sep='_')
vnames <- colnames(X)
vnames <- sub(':', '.', vnames)

# make a scatter plot of betas
fit_summary <- summary(fit, pars='beta')[[1]]
pt_betas <- fit_summary[,6]  # medians
# dim(pt_betas) <- c(TT * P, U)
dim(pt_betas) <- c(P, U)
pt_betas <- t(pt_betas)
colnames(pt_betas) <- vnames
pairs(pt_betas)

# get samples from posterior for beta
genbeta <- as.data.frame(rstan::extract(fit, pars="genbeta")[[1]])
names(genbeta) <- vnames

# make pairs plot with density underlaid
library(ggplot2)
p <- ggpairs(pt_betas)
for (r in 2:P) {
  for (c in 1:(r - 1)) {
    pp <- getPlot(p, r, c)  # get previous plot in this cell
    gg <- geom_density2d(data=genbeta, aes_q(x=as.name(vnames[c]), y=as.name(vnames[r])))
    p <- putPlot(p, pp + gg, r, c)
  }
}
p

