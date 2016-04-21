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

# plot 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
library(ggplot2)

# merge NEI and SCC by SCC
NEI_SCC <- merge(NEI, SCC, by = "SCC")

# find the emissions from motor vehicle(on-road) in Baltimore city
subNEI <- NEI[NEI$fips=='24510' & NEI$type=='ON-ROAD', ]
# aggregate by year
aggregate_by_year <- aggregate(Emissions ~ year, subNEI, sum)

png('plot5.png')
ggplot(aggregate_by_year, aes(year, Emissions)) + 
    geom_bar(stat = "identity") + 
    xlab("year") + ylab("PM2.5(tons)") + 
    ggtitle("Total PM2.5 Emissions from motor vehicle in Baltimore City")

dev.off()

