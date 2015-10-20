# data_cleaning.R
# this file prepares data for modeling

library(R.matlab)  # for reading Matlab data

datadir <- "data/"

# load count data from Matlab files
countfile <- "SUMMARY_counts_TbT.mat"
colnamefile <- "colnames.mat"

# convert to data frame
countdata <- as.data.frame(readMat(paste(datadir, countfile, sep=""))[[1]])

# read column names and merge
count_colnames_list <- (readMat(paste(datadir, colnamefile, sep=""))[[1]])
count_colnames <- lapply(count_colnames_list, function(x) {x[[1]][1, 1]})
colnames(countdata) <- count_colnames

# save as R object
outfile <- "countdata"
save(file=paste(datadir, outfile, sep=""), list=c('countdata'))

