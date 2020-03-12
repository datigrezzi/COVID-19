#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("plottingfunctions.R")

# Define UI for application that draws a histogram
ui <- navbarPage("COVID-19 in Italy",
    tabPanel("COVID-19", 
        sidebarPanel(
            selectInput("region", "Region", choices = "Abruzzo"),
            checkboxInput("accumulative", "Accumulative", value = FALSE)
        ),
        mainPanel(
            plotlyOutput("plotoutput", height = "100%")
        )
    ),
    tabPanel("About", 
        br(),
        includeMarkdown("readme.Rmd")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # read in data
    datapath <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
    thisdata <- preprocessing(datapath)
    # add regions tabs
    regions <- unique(thisdata$denominazione_regione)
    # Can also set the label and select items
    updateSelectInput(session, "region", choices = regions, selected = regions[1])
    
    # use ggplotly(ggplot) for each tab
    output$plotoutput <- renderPlotly({
        regionplot <- regionalplot(thisdata, input$region, input$accumulative)
        ggplotly(regionplot) %>%
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

