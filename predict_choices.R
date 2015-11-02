# train some machine learning models to predict choices from spike counts
library(glmnet)
library(dplyr)
library(caret)

set.seed(12345)

# load data
load("data/countdata")

# filter data with too few observations
countdata <- countdata %>% group_by(unit) %>% filter(length(outcome) > 100)

partition_data <- function(outcome, nfolds) {
  suppressMessages(require('caret'))

  folds <- createFolds(outcome, nfolds)

  foldid <- rep(NA, length(outcome))

  for (ind in 1:nfolds) {
    foldid[folds[[ind]]] <- ind
  }

  return(foldid)
}

# set epoch of interest
countvar <- "TargetAcquire_sp_count"

# model to train
form <- as.formula(paste(" ~ ", countvar))

# NB:
# It's a little odd to use the formua above, which includes a constant, when
# glmnet already assumes one. In fact, the constant above will be penalized,
# while the glmnet constant is not (by default). However, glmnet requires at
# least two regressors to run, so we just throw in the extra constant,
# knowing it will get thrown out.

# define function to train glm
train_glm <- function(df, nfolds) {
  folds <- partition_data(df$y, nfolds)
  tryCatch({
    fit <- cv.glmnet(model.matrix(form, data=df), df$y, family="binomial",
              type.measure = "auc", foldid = folds)
    maxlambda <- fit$lambda.1se
    maxind <- which(fit$lambda == fit$lambda.1se)
    return(c(fit$cvm[maxind], fit$cvsd[maxind]))
  }, error=function(e) {return(c(NA, NA))})
}

# find auc for each unit, filter for those with good enough zscore
thresh <- qnorm(0.95)
auc <- countdata %>%
  mutate(y=outcome %in% c("other", "both")) %>%
  select_("unit", countvar, "y", "reward") %>%
  group_by(unit) %>%
  do(res = train_glm(., 5)) %>%
  filter(!is.na(res[1])) %>%
  mutate(mean=res[1], sd=res[2]) %>%
  filter(mean - thresh * sd > 0.5)

ngood <- nrow(auc)
ntot <- length(unique(countdata$unit))
