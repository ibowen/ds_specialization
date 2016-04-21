# download the file
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "./exdata_NEI_data.zip")
unzip(zipfile = "./exdata_NEI_data.zip")

# read files
if(!exists("./NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("./SCC")) {
    SCC <- readRDS("Source_Classification_Code.rds")
}

# check files
str(NEI)
str(SCC)

# plot 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999 2008 for Baltimore City? 
# Which have seen increases in emissions from 1999 2008? 
# Use the ggplot2 plotting system to make a plot answer this question.


BM24510 <- NEI[NEI$fips == "24510", ]
aggregate_by_year <- aggregate(Emissions ~ year + type, BM24510, sum)

library(ggplot2)
png('plot3.png')
ggplot(aggregate_by_year, aes(year, Emissions, color=type)) + 
    geom_line() + 
    xlab("year") + ylab("PM2.5(tons)") + 
    ggtitle("Total PM2.5 Emissions By Years in Baltimore City, Maryland")

dev.off()
