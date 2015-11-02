# runmodel.R
# code for fitting models

set.seed(12345)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
load("data/countdata")

# subsample for testing
# countdata <- countdata[as.integer(as.factor(countdata$unit)) < 10,]

# first, pick the data we want to model
countvar <- "TargetAcquire_sp_count"

# make variables
count <- countdata[[countvar]]
unit <- as.integer(as.factor(countdata$unit))
type <- as.integer(countdata$outcome)
rwd <- countdata$reward
# X <- model.matrix(as.formula("~ reward"), data=countdata)
# X <- model.matrix(as.formula("~ (-1 + outcome + outcome:reward):cued"), data=countdata)
X <- model.matrix(as.formula("~ -1 + outcome + outcome:reward"), data=countdata)

# get data ready for stan
stan_dat <- list(N = length(count)[1],
                 U = length(unique(unit)),
                 T = length(unique(type)),
                 P = dim(X)[2],
                 c = count,
                 outcome = type,
                 unit = unit,
                 X = X
                 )

# get ready to run stan
watched_pars <- c("beta", "mu", "tau", "Sigma", "nu", "genbeta")
fit <- stan(file = 'models/model6.stan', data = stan_dat,
             pars = watched_pars,
             iter = 1000, thin=2, chains = 8)

# save fit object
fname <- "fitobj_targacq_multi_t"
save(file=fname, list=c('fit', 'X'))

