#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(shinyWidgets)
library(stringr)
library(zoo)


df_exportimport <- readRDS('df_exportimport_final.rds')

ui <- fluidPage(
    
    # Application title
    titlePanel("Foreign Trade Statistics - Vehicle and Trailer Tracking"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Select Year:",
                        choices = list("Year"=unique(lubridate::year(df_exportimport$Date)),selected = 2021)),
            selectInput("exportimport",
                        "Export Or Import:",
                        choices = list("Export Or Import"=c('EXPORT','IMPORT'),selected = 'IMPORT')),
            multiInput(
                inputId = "selectcountries", label = "Countries :",
                choices =unique(df_exportimport$ExportImportCountry),
                selected = "Germany", width = "350px"
            ),
            verbatimTextOutput(outputId = "res"),
        helpText("Select multiple countries, year and Import or Export to compare monthly Export-Import numbers on line plot.")
        

            
        ),
    mainPanel(
        plotOutput("line_plot",width = "100%",height="800px")
    )
        

)

)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$line_plot <- renderPlot({
        
        df_exportimport %>%  
            filter(ExportImport == input$exportimport, lubridate::year(Date) == input$year, ExportImportCountry %in% input$selectcountries) %>% 
            group_by(year=substr(Date, 1, 4), ExportImportCountry) %>% 
            summarize(Total = sum(Level)) %>%  
            ggplot(aes(x=year, y=Total, group=ExportImportCountry, color=ExportImportCountry)) +
            geom_line() +
            ggtitle(paste("Monthly",str_to_title(input$exportimport),"Numbers")) +
            xlab("MONTH") +
            ylab(paste(input$exportimport,"NUMBERS"))
    
            
        
        
        
        
    })

    

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
