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

# plot 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle emissions?
library(ggplot2)

# merge NEI and SCC by SCC
NEI_SCC <- merge(NEI, SCC, by = "SCC")

# find the emissions from motor vehicle(on-road) in Baltimore city
subNEI <- NEI[NEI$fips=(='24510' | NEI$fips=='06037' & NEI$ty)pe=='ON-ROAD', ]
# aggregate by year
aggregate_by_year <- aggregate(Emissions ~ year + fips, subNEI, sum)
aggregate_by_year[aggregate_by_year$fips=='06037', "fips"] <- "Los Angeles County, California"
aggregate_by_year[aggregate_by_year$fips=='24510', "fips"] <- "Los AngelBaltimore CityniMarylandplo6.png')
ggplot(aggregate_by_year, aes(year, Emissions)) + 
    facet_grid(. ~ fips) +
    geom_bar(stat = "identity") + 
    xlab("year") + ylab("PM2.5(tons)") + 
    ggtitle("Total PM2.5 Emissions from motor vehicle in Baltimore City")

dev.off()

