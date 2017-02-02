# This is the solution to Assignment 1 - Spring 2017
# Create a new repository called Assignment 1 on GitHub. 
# Import the following data into R. 
# https://data.baltimorecity.gov/api/views/wsfq-mvij/rows.csv?accessType=DOWNLOAD&bom=true
# Generate a summary (mean, and standard deviation) of all numerical columns.
# Do the same as above, but now grouped by crime code.
# Count the number of incidents by crime code and District
# Create a cross-table of crime code and weapons used.

setwd("/Users/mshanker/Downloads/RCode/TidyData")
fileref <- "https://data.baltimorecity.gov/api/views/wsfq-mvij/rows.csv?accessType=DOWNLOAD"
download.file(fileref,destfile = "bpd.csv",method = "curl")
Bpd <- read.csv("bpd.csv")
summary(Bpd)

Bpd <- mutate_at(Bpd,vars(District),funs(toupper)) # converting all lowercase to uppercase

# Note that numeric columns may contain NAs, as such remember to set
# the rm.na flag to true
sapply(Bpd[sapply(Bpd,is.numeric)],mean,na.rm=T) # select only numeric columns
sapply(Bpd[sapply(Bpd,is.numeric)],sd,na.rm=T)

library("dplyr")
by_c <- group_by(Bpd,CrimeCode) # group by Code
summarize(by_c, mpost = mean(Post,na.rm=T), spost = sd(Post,na.rm=T))


# Cross tabs for number of incidents by Crime code and District
xtabs(Total.Incidents ~ CrimeCode+District,Bpd) %>% ftable()
xtabs(~ CrimeCode+Weapon,Bpd)

