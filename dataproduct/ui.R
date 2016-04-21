library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("PM2.5 Plotting"),
    sidebarPanel(
        sliderInput('sample_size', 'Sample Size', min = 10000, max = 100000, step = 5000, value = 10000),
        checkboxGroupInput('year', label = "years", choices = list("1999" = 1999,
                "2002" = 2002, "2005" = 2005, "2008" = 2008), selected = 2002),
        checkboxGroupInput('type', label = "pollution types", choices = list("Point" = "POINT",
                "Nonpoint" = "NONPOINT", "On-road" = "ON-ROAD", "Non-road" = "NON-ROAD"), selected = "ON-ROAD"),
        submitButton("submit")
    ),
    mainPanel(
        h4('Total PM2.5 Emissions By Year and Type'),
        br(),
        h4('Instructions'),
        helpText("This application is for show PM2.5 emissions by years and types."),
        helpText("Excuse me, it will take a while to load the data."),
        plotOutput('newPlot')
    )
))