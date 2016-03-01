# download the datafile
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = "./exdata_household_power_consumption.zip")
unzip("./exdata_household_power_consumption.zip", exdir = "./")

# subset the data
data <- read.table("./household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
data2 <- subset(data, Date %in% c('1/2/2007', '2/2/2007'))
data2$datetime <- strptime(paste(data2$Date, data2$Time), "%d/%m/%Y %H:%M:%S")

# plot 2
png("plot2.png", width = 480, height = 480)
plot(data2$datetime, data2$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()

