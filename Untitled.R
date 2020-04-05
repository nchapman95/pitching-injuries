install.packages("plyr", options(repos="http://streaming.stat.iastate.edu/CRAN"))

library(pitchRx)

dat <- scrape(start = "2013-06-01", end = "2013-06-01")