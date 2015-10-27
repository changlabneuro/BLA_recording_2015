# model_analysis.R
# analysis of Bayesian models fit with STAN

library(rstan)

# load original data
load("/data/countdata")

# load model samples
fname <- "fitobj_targacq_prod_t"
load(fname)

# calculate some useful quantities
U <- length(unique(countdata$unit))
P <- dim(X)[2]
TT <- length(unique(countdata$outcome))
vnames <- paste(rep(colnames(X), each=TT), rep(levels(countdata$outcome), P), sep='.')

# make a scatter plot of betas
fit_summary <- summary(fit, pars='beta')[[1]]
pt_betas <- fit_summary[,6]  # medians
dim(pt_betas) <- c(U, P * TT)
colnames(pt_betas) <- vnames
pairs(pt_betas)
