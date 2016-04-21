library(shiny)
library(ggplot2)
# download the file
#download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "./exdata_NEI_data.zip")
#unzip(zipfile = "./exdata_NEI_data.zip")

# read files
if(!exists("./NEI")) {
    NEI <- readRDS("summarySCC_PM25.rds")
}

shinyServer(
    function(input, output){
        output$newPlot <- renderPlot({
            # Sample the source data
            inNEI <- sample(1:nrow(NEI), input$sample_size, replace = FALSE)
            NEI <- NEI[inNEI, ]
            NEI_2 <- NEI[NEI$year %in% input$year, ]
            NEI_2 <- NEI_2[NEI_2$type %in% input$type, ]
            
            aggregate_by_year <- aggregate(Emissions ~ year + type, NEI_2, sum)
            ggplot(data = aggregate_by_year, aes(x = as.character(year), y = Emissions, colour = type)) +
                geom_point(size = 15, alpha = 0.5) +
                xlab("years") +
                ylab("PM2.5 Emissions tons") +
                ggtitle("PM2.5 Emissions by year and by type")
        })
    }
)