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

# plot 4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
library(ggplot2)

# merge NEI and SCC by SCC
NEI_SCC <- merge(NEI, SCC, by = "SCC")

# find the coal related rows
coalNEI <- NEI_SCC[grepl("coal", NEI_SCC$Short.Name, ignore.case=TRUE), ]
# aggregate by year
aggregate_by_year <- aggregate(Emissions ~ year, coalNEI, sum)

png('plot4.png')
ggplot(aggregate_by_year, aes(year, Emissions)) + 
    geom_bar(stat = "identity") + 
    xlab("year") + ylab("PM2.5(tons)") + 
    ggtitle("Total PM2.5 Emissions from 1999 to 2008")

dev.off()
