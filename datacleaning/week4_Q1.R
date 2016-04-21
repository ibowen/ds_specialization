#Question 1
#The American Community Survey distributes downloadable data about United States communities. 
#Download the 2006 microdata survey about housing for the state of Idaho using download.file() 
#from here: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
#and load the data into R. The code book, describing the variable names is here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
#Apply strsplit() to split all the names of the data frame on the characters "wgtp". What is the value of the 123 element of the resulting list?

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "./getdata_data_ss06hid.csv")
data1 <- read.csv("getdata_data_ss06hid.csv")
splitdata <- strsplit(namelist, "wgtp") #split the namelist by wgtp
splitdata[[123]] #get the 123th value

#Question 2
#Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
#Remove the commas from the GDP numbers in millions of dollars and average them. 
#What is the average?
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "getdata_data_GDP.csv")
data2 <- read.csv("getdata_data_GDP.csv", nrows = 210)[5:194, ]
mean(as.numeric(gsub(',', '', data2[, "X.3"])), na.rm = TRUE)
#Question 3
#In the data set from Question 2 what is a regular expression that would allow you to count the number of countries 
#whose name begins with "United"? Assume that the variable with the country names in it is named countryNames. 
#How many countries begin with United?
countryNames <- data2$X.2
grep("^United", countryNames)

#Question 4
#Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
#Load the educational data from this data set:
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
#Match the data based on the country shortcode. Of the countries for which the end of the fiscal year is available, how many end in June?
library(dplyr)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",  destfile = "getdata_data_EDSTATS_Country.csv")
gdp <- read.csv('getdata_data_GDP.csv', skip=4, nrows = 190) # get 190 rows of data after skipping the first 4 rows
new_gdp <- tbl_df(gdp) %>% select(1) # select the first countrycode row
names(new_gdp) <- c("CountryCode") # assign the new name of CountryCode
edu <- read.csv("getdata_data_EDSTATS_Country.csv")
new_edu <- tbl_df(edu) %>% select(1, 10)
mdata <- merge(new_gdp, new_edu, all = TRUE, by = c("CountryCode")) # merge two data frames by the same CountryCode
table(grepl("Fiscal year end: June", mdata$Special.Notes)) # find the needed June

#Question 5
#You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices for publicly traded companies on the NASDAQ and NYSE. 
#Use the following code to download data on Amazon's stock price and get the times the data was sampled.
#library(quantmod)
#amzn = getSymbols("AMZN",auto.assign=FALSE)
#sampleTimes = index(amzn)
#How many values were collected in 2012? How many values were collected on Mondays in 2012?
install.packages('quantmod')
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

table(grepl("2012", sampleTimes))
table(grepl("Mon 2012", format(sampleTimes, "%a %Y")))
