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

# make pairs plot with density
dd <- data.frame(v=rstan::extract(fit, pars='Sigma')[[1]])
source("helpers.R")
p <- pairplot(as.data.frame(pt_betas), genbeta, dd, seq(9, 12))
p

