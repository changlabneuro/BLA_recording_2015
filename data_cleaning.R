# data_cleaning.R
# this file prepares data for modeling

library(R.matlab)  # for reading Matlab data
library(dplyr)
library(tidyr)

datadir <- "data/"

# load count data from Matlab files
countfile <- "SUMMARY_counts_TbT.mat"
colnamefile <- "colnames.mat"

# convert to data frame
countdata <- as.data.frame(readMat(paste(datadir, countfile, sep=""))[[1]])

# read column names and merge
count_colnames_list <- (readMat(paste(datadir, colnamefile, sep=""))[[1]])
count_colnames <- lapply(count_colnames_list, function(x) {x[[1]][1, 1]})
count_colnames <- gsub(" ", "_", count_colnames)
colnames(countdata) <- count_colnames

# add trial number column
countdata <- countdata %>%
  mutate(trial=row_number())

# convert multiple binary columns to outcome type and trial type
outcome <- countdata %>%
  select(trial, matches("[[:upper:]]{2,}", ignore.case=FALSE)) %>%  # select trial type columns and trial
  gather(label, flag, -trial) %>%  # collapse multiple columns into one
  filter(flag == 1) %>%  # select only the "column" corresponding to this trial
  select(trial, label)  %>% # keep trial and label columns only
  extract(label, c("cued", "outcome"), "(Cued)*([[:upper:]]{2,})")

# further cleaning:
# set cued to 0/1
outcome$cued[is.na(outcome$cued)] <- 0
outcome$cued[outcome$cued == "Cued"] <- 1
# set outcome to lowercase
outcome$outcome <- sapply(outcome$outcome, tolower)

# now merge in new columns
countdata <- countdata %>%
  select(-matches("[[:upper:]]{2,}", ignore.case=FALSE)) %>%
  merge(outcome)

# normalize reward data as percent of max for that unit
# for some reason, the max observed is only 0.9 * MAX, so need to compensate
countdata <- countdata %>%
  group_by(unit) %>%
  mutate(RewardSize=RewardSize/(max(RewardSize) / 0.9)) %>%
  rename(reward=RewardSize)

# save as R object
outfile <- "countdata"
save(file=paste(datadir, outfile, sep=""), list=c('countdata'))

