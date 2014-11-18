#SPRUCE Data Analysis 
#September 2014
setwd("~/Desktop/BU/Research/minnesota/Picarro Data 6.25.14/R analysis/09/data files")
filenames <- list.files("test data", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
dim(ldf)

load_data <- function(path) { 
  files <- dir("test data", pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

data <- load_data("test data")
