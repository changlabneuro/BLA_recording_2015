# exploratory_analysis.R
# look at maximum likelihood/glm modeling of firing responses, without hierarchy

library(dplyr)
library(tidyr)
library(GGally)

# load data
load("data/countdata")

# now fit a glm for each unit
epoch <- "TargetAcquire_sp_count"
form <- as.formula(paste(epoch, "~ reward"))

coefmat <- countdata %>% group_by(unit, outcome) %>%
  do(coef=coef(glm(form, family=poisson(link="log"), data= .))) %>%
  mutate(baseline=coef[1], slope=coef[2]) %>%
  select(-coef)

# correlations among slope and baseline
ggpairs(data=coefmat, columns=3:dim(coefmat)[2], color='outcome')

# get outcome into columns
coef_wide <- coefmat %>% gather(coef, value, baseline:slope) %>%
  unite(vname, outcome, coef, sep='.') %>%
  spread(vname, value) %>%
  select(-unit)

ggpairs(coef_wide)
