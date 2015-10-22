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
countvar <- "RewardOn_sp_count"

# make variables
count <- countdata[[countvar]]
unit <- as.integer(as.factor(countdata$unit))
type <- as.integer(countdata$outcome)
rwd <- countdata$reward

# get data ready for stan
stan_dat <- list(N = length(count)[1],
                 U = length(unique(unit)),
                 T = length(unique(type)),
                 c = count,
                 type = type,
                 unit = unit,
                 reward = rwd
                 )

# get ready to run stan
watched_pars <- c("beta", "sens", "mu_beta", "mu_sens", "sig_beta", "sig_sens")
fit <- stan(file = 'models/model1.stan', data = stan_dat,
            pars = watched_pars,
            iter = 1000, chains = 4)