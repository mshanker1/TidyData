#This program will download data from College Scorecard, and convert it into a data fram
fileURL <- "https://ed-public-download.apps.cloud.gov/downloads/Most-Recent-Cohorts-Scorecard-Elements.csv"
download.file(fileURL,destfile = "CollegeData.csv",method = "auto")
list.files(".")
# The csv contains both NULL values and privacysuppressed designations in numeric fields
CollegeScorecard <- read.csv("CollegeData.csv",na.strings = c("NULL","PrivacySuppressed"))
CS <- tbl_df(CollegeScorecard) #allows for better printing compared to a dataframe
dim(CS)
library(dplyr) # useful library for manipulating data
# Examples of use follow
filter(CS,!is.na(SATWR75)) %>% head() # remove all na values in that column and show
distinct(CS,SATVR25) # show distinct values by variable
summarise(CS,ACT=mean(ACTCM25,na.rm=T)) # summarize by variable

# Using group_by funtions
by_state <- group_by(CS,STABBR)
acts <- summarise(by_state,count= n(),MACT = mean(ACTCM25,na.rm=T),
                  MSAT = mean(SATVR25,na.rm=T))
sacts <- filter(acts,MACT>20 & MSAT > 300)

# Chaining:  dplyr provides the %>% operator. x %>% f(y) turns into f(x, y) so 
# you can use it to rewrite multiple operations that you can read left-to-right, 
# top-to-bottom

CollegeScorecard %>% group_by(STABBR) %>%
  select(STABBR,ACTCM25,SATMT25) %>%
  summarize(
    count = n(),
    mact = mean(ACTCM25, na.rm=T),
    msat = mean(SATMT25, na.rm=T)
  ) %>%
  filter(mact > 25 | msat > 200)
