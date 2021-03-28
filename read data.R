# Store current working directory
projdir <- getwd()
projdir
# Change working directory to covid19 data folder
setwd("./covid-19-data/")
US <- read.csv("us.csv")
setwd(projdir)