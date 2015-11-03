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
  select(trial, matches("[[:upper:]]{3,}", ignore.case=FALSE)) %>%  # select trial type columns and trial
  gather(label, flag, -trial) %>%  # collapse multiple columns into one
  filter(flag == 1) %>%  # select only the "column" corresponding to this trial
  select(trial, label)  %>% # keep trial and label columns only
  extract(label, c("cued", "outcome"), "(Cued)*([[:upper:]]{3,})")

# further cleaning:
# set cued to 0/1
outcome$cued[is.na(outcome$cued)] <- 0
outcome$cued[outcome$cued == "Cued"] <- 1
# set outcome to lowercase
outcome$outcome <- sapply(outcome$outcome, tolower)
outcome$outcome <- factor(outcome$outcome, c("none", "other", "both", "self"))

# now merge in new columns
countdata <- countdata %>%
  select(-matches("[[:upper:]]{3,}", ignore.case=FALSE)) %>%
  merge(outcome)

# normalize reward data as percent of max for that unit
# for some reason, the max observed is only 0.9 * MAX, so need to compensate
countdata <- countdata %>%
  group_by(unit) %>%
  mutate(RewardSize=RewardSize/(max(RewardSize) / 0.9)) %>%
  rename(reward=RewardSize)

# add subject identifier
datobjs <- c("DATA_TbT_solly", "DATA_TbT_yerkes")
subj_names <- c("1", "2")
subj_frame <- data.frame()
for (idx in 1:length(datobjs)) {
  load(paste(datadir, datobjs[idx], sep=""))
  subj_df <- as.data.frame(DATA_TbT)
  id_df <- subj_df %>%
    select(units) %>%
    distinct() %>%
    mutate(subj=subj_names[idx])
  subj_frame <- rbind(subj_frame, id_df)
}
# add subject column to countdata; match is faster than naive merge
countdata$subj <- subj_frame$subj[match(countdata$unit, subj_frame$units)]

# save as csv
outfile <- "countdata"
write.csv(countdata, file=paste(datadir, outfile, '.csv', sep=""), row.names=FALSE)

######## OT data #############
datobjs <- c("OT_dlpfc", "OT_BLA")

# which units were which injections?
OT_inj <- c(1900.1, 1902.1, 1902.2, 1908.1, 1908.2, 1909.1, 1909.2, 1909.3, 1909.4, 5010.1, 5012.1,
            5012.2,5012.3, 5014.1, 5021.1, 5023.1, 5025.1, 2001.1,2001.2,2001.3,2001.4,2003.1,2003.2,
            2003.3,2005.1, 2005.2, 2007.1, 2007.2, 3001.1,3003.1, 3003.2, 3003.3, 3005.1, 3005.2, 3005.3)
Sal_inj <- c(1901.1, 1901.2, 1903.1, 1903.2, 1905.1, 1905.2, 1905.3, 1907.1, 1907.2, 5011.1,
             5013.1, 5022.1, 5024.1, 5024.2, 5024.3, 5026.1, 2000.1, 2000.2, 2004.1, 2006.1, 2006.2,
             2006.3, 2006.4, 2006.5,2008.1,3000.1, 3002.1, 3002.2, 3004.1, 3004.2)

# make a dataframe linking units to subject and treatment type
cond_list <- character()
subj_list <- character()
for (idx in OT_inj) {
  cond_list[[as.character(idx)]] <- "OT"
}
for (idx in Sal_inj) {
  cond_list[[as.character(idx)]] <- "Saline"
}
for (idx in c(OT_inj, Sal_inj)) {
  if (idx < 3000)
    subj_list <- c(subj_list, "1")
  else if (idx >= 3000 & idx < 5000)
    subj_list <- c(subj_list, "3")
  else
    subj_list <- c(subj_list, "4")
}

cond_and_subj_df <- data.frame(units=c(OT_inj, Sal_inj), injection=cond_list, subj=subj_list)


# load up OT data
subj_frame <- data.frame()
for (idx in 1:length(datobjs)) {
  load(paste(datadir, datobjs[idx], sep=""))
}

# add area data to injection
ot_dlpfc <- as.data.frame(OT_dlpfc) %>% mutate(area="dlpfc")
ot_bla <- as.data.frame(OT_BLA) %>% mutate(area="bla")
ot_df <- rbind(ot_bla, ot_dlpfc)

# add trial number column
ot_df <- ot_df %>%
  mutate(trial=row_number())

# convert multiple binary columns to outcome type and trial type
outcome <- ot_df %>%
  select(trial, matches("[[:upper:]]{3,}", ignore.case=FALSE)) %>%  # select trial type columns and trial
  gather(label, flag, -trial) %>%  # collapse multiple columns into one
  filter(flag == 1) %>%  # select only the "column" corresponding to this trial
  select(trial, label)  %>% # keep trial and label columns only
  extract(label, c("cued", "outcome"), "(Cued)*([[:upper:]]{3,})")

# further cleaning:
# set cued to 0/1
outcome$cued[is.na(outcome$cued)] <- 0
outcome$cued[outcome$cued == "Cued"] <- 1
# set outcome to lowercase
outcome$outcome <- sapply(outcome$outcome, tolower)
outcome$outcome <- factor(outcome$outcome, c("none", "other", "both", "self"))

# now merge in new columns
ot_df <- ot_df %>%
  select(-matches("[[:upper:]]{3,}", ignore.case=FALSE)) %>%
  merge(outcome)

ot_data <- ot_df %>% merge(cond_and_subj_df)

# save as csv
outfile <- "ot_data"
write.csv(ot_data, file=paste(datadir, outfile, '.csv', sep=""), row.names=FALSE)