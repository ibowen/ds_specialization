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

# plot 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 
# to 2008? Use the base plotting system to make a plot answering this question.

#png('plo<- NEI[NEI$fips == "24510", ]
aggregate_by_year <- aggregate(Emissions ~ year, BM24510, sum)
with(aggr
png('plot2.png')
egate_by_year, barplot(height = Emissions, names.arg = year, xlab = "Year", ylab = "PM2.5(tons)", main = "T
                                otal PM2.5 Emissions By Years"))
#dev. in Baltimore City, Marylandoff(
