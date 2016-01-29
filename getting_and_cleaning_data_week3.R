#1. The American Community Survey distributes downloadable data about United States communities
#. Download the 2006 microdata survey about housing for the state of Idaho using download.file() 
#from here: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# Create a logical vector that identifies the households on greater than 10 acres 
# who sold more than $10,000 worth of agriculture products. 
# Assign that logical vector to the variable agricultureLogical. 
# Apply the which() function like this to identify the rows of the data frame 
# where the logical vector is TRUE.
# which(agricultureLogical) What are the first 3 values that result?

idaho <- read.csv("idaho.csv")
ans1 <- which(idaho$AGS == 6 & idaho$ACR == 3)[1:3]

#2. Using the jpeg package read in the following picture of your instructor into R
#https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
#Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting data? 
# (some Linux systems may produce an answer 638 different for the 30th quantile)

install.packages('jpeg')
library('jpeg')
img <- readJPEG("jeff.jpg", native = TRUE)
ans2 <- quantile(img, probs=c(0.3,0.8))

#3. Load the Gross Domestic Product data for the 190 ranked countries in this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
#Load the educational data from this data set:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# Match the data based on the country shortcode. How many of the IDs match? 
# Sort the data frame in descending order by GDP rank (so United States is last). 
# What is the 13th country in the resulting data frame?

library(dplyr)
# read data by removing the heading 4 rows
gdp <- read.csv('getdata-data-GDP.csv', skip=4, stringsAsFactors = FALSE, nrows = 190)
# select the need colmns and rename the columns
gdp2 <- tbl_df(gdp) %>% select(X, X.1, X.3, X.4)
colnames(gdp2) <- c("CountryCode", "Ranking", "Country", "GDP")
# read the education data
edu <- read.csv("getdata-data-EDSTATS_Country.csv", stringsAsFactors = FALSE)
edu2 <- tbl_df(edu)
# count the row
filter(gdp2, unique(CountryCode) %in% unique(edu2$CountryCode)) %>% nrow()
# sort the GDP rank
arrange(gdp2, desc(Ranking))[13,]

# What is the average GDP ranking for the "High income: OECD" and "High income: 
# nonOECD" group?
# merge the two data sets
mdata <- merge(gdp2, edu2, all = TRUE, by = c("CountryCode"))
group_by(mdata, Income.Group) %>% summarise(mean(Ranking, na.rm = TRUE))

# Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. 
# How many countries are Lower middle income but among the 38 nations with highest GDP?
# acquire the quantile range
breaks <- quantile(mdata$Ranking, probs = seq(0, 1, 0.2), na.rm = TRUE)
# apply the quantile range on the new variable
mutate(mdata, quantile_gdp = cut(mdata$Ranking, breaks = breaks)) %>% # apply value based on the quantile
            filter(Income.Group == "Lower middle income") %>% # where
                group_by(Income.Group, quantile_gdp) %>% # group by
                    select(Income.Group, quantile_gdp) %>% # select
                        summarise(sum(!is.na(quantile_gdp))) # sum the total quantity

