# runmodel.R
# code for fitting models

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
load("data/countdata")

# subsample for testing
#countdata <- countdata[as.integer(as.factor(countdata$unit)) < 10,]

# first, pick the data we want to model
countvar <- "TargetAcquire_sp_count"

# make variables
count <- countdata[[countvar]]
unit <- as.integer(as.factor(countdata$unit))
type <- as.integer(countdata$outcome)
rwd <- countdata$reward
# X <- model.matrix(as.formula("~ -1 + outcome + outcome:reward +
#                              outcome:cued + outcome:reward:cued"), data=countdata)
X <- model.matrix(as.formula("~ -1 + outcome + outcome:reward"), data=countdata)

# get data ready for stan
stan_dat <- list(N = length(count)[1],
                 U = length(unique(unit)),
                 T = length(unique(type)),
                 P = dim(X)[2],
                 c = count,
                 type = type,
                 unit = unit,
                 X = X
                 )

# get ready to run stan
watched_pars <- c("beta", "mu", "tau", "Sigma", "nu")
fit <- stan(file = 'models/model6.stan', data = stan_dat,
             pars = watched_pars,
             iter = 1000, thin=2, chains = 8)

# save fit object
fname <- "fitobj_norm_targacq_cued"
save(file=fname, list=c('fit', 'X'))

# make a scatter plot of betas
fit_summary <- summary(fit, pars='beta')[[1]]
pt_betas <- fit_summary[,6]  # medians
U <- length(unique(unit))
P <- dim(X)[2]
dim(pt_betas) <- c(U, P)
colnames(pt_betas) <- colnames(X)
pairs(pt_betas)

# extract correlation matrix
fit_summary <- summary(fit, pars='Sigma')[[1]]
pt_Sigma <- fit_summary[,6]  # medians
P <- dim(X)[2]
dim(pt_Sigma) <- c(P, P)
colnames(pt_Sigma) <- colnames(X)
rownames(pt_Sigma) <- colnames(X)

# plot