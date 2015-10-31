# model_analysis.R
# analysis of Bayesian models fit with STAN

library(rstan)

# load original data
load("data/countdata")

# load model samples
fname <- "fitobj_targacq_multi_t_cued"
load(fname)

# calculate some useful quantities
U <- length(unique(countdata$unit))
P <- dim(X)[2]
# TT <- length(unique(countdata$outcome))
# vnames <- paste(rep(levels(countdata$outcome), each=P), rep(colnames(X), TT), sep='_')
vnames <- colnames(X)
vnames <- gsub(':', '.', vnames)
vnames <- gsub('outcome', '', vnames)
vnames <- gsub('reward', 's', vnames)
vnames <- gsub('cued', '', vnames)

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
library(GGally)
p <- ggpairs(pt_betas) + theme_bw()
for (r in 1:P) {
  for (c in 1:P) {
    pp <- getPlot(p, r, c)  # get previous plot in this cell
    if (r == c) {
      gg <- ggplot() + geom_density(data=genbeta, aes_q(x=as.name(vnames[c])), color='blue') +
        geom_density(data=as.data.frame(pt_betas), aes_q(x=as.name(vnames[c]))) + theme_bw()
      p <- putPlot(p, gg, r, c)
    } else if (r > c) {
      newplot <- geom_density2d(data=genbeta, aes_q(x=as.name(vnames[c]), y=as.name(vnames[r])))
      pp$layers <- c(newplot, pp$layers)  # density to add
      p <- putPlot(p, pp, r, c)
    } else {
      var <- paste('Sigma[', r, ',', c,']', sep='')
      dd <- data.frame(v=rstan::extract(fit, var)[[1]])
      gg <- ggplot(data=as.data.frame(dd)) + geom_vline(xintercept=0) + geom_density(aes(x=v), color='red') + xlim(-1, 1) + theme_bw()
      p <- putPlot(p, gg, r, c)
    }
  }
}
p

