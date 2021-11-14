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


fish_market <- read.csv("balik_hal_fiyatlari.csv",sep=";",encoding="UTF-8")
fish_market$TARIH <- as.Date(fish_market$TARIH)
fish_market <- fish_market %>% mutate(YILAY = format(TARIH,'%Y-%m'))
fish_market_ym <- fish_market %>% 
                    mutate(GUNLUK_ORTALAMA_UCRET = (AZAMI_UCRET + ASGARI_UCRET / 2)) %>% 
                    group_by(MAL_ADI,YILAY) %>% 
                    summarise(AYLIK_ORTALAMA_UCRET = mean(GUNLUK_ORTALAMA_UCRET))
level_order <- c("2021-01", "2021-02" ,"2021-03", "2021-04", "2021-05" ,"2021-06" ,"2021-07", "2021-08" ,"2021-09" ,"2021-10")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Izmir's Fish Market"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("fishname",
                        "Select Fish:",
                        choices = list("ALL" = "ALL", "FISH"=unique(fish_market_ym$MAL_ADI)),selected = "DIL"),
            selectInput("fishname2",
                        "Select Second Fish To Compare:",
                        choices = list("ALL" = "ALL", "FISH"=unique(fish_market_ym$MAL_ADI)),selected = "AHTAPOT(DONUK)"),
           
            helpText("Select two fishes to compare average monthly prices on line plot.
                     If not chosen, only the data table will update and shows all fishes in the market")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("fish_plot",width = "100%"),
           tableOutput("fish_market_ym")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$fish_market_ym <- renderTable({
        
        if(input$fishname == "ALL"  & input$fishname2  == "ALL"){
            fish_market_ym
        }
        else if(input$fishname != "ALL" & input$fishname2 != "ALL"){
            fish_market_ym %>% filter(MAL_ADI == input$fishname | MAL_ADI == input$fishname2)
            
        }
        else
        {
            fish_market_ym %>% filter(MAL_ADI == input$fishname)
        }
        
        
    },caption = "Fish market Average Prices by Year and Month ",caption.placement = getOption("xtable.caption.placement", "top"),striped =TRUE)
    
    output$fish_plot <- renderPlot({
        
        if(input$fishname == "ALL"  & input$fishname2  == "ALL"){
            
            paste("None")
        }
        else if(input$fishname != "ALL" & input$fishname2 != "ALL"){
            
            ggplot(fish_market_ym %>% filter(MAL_ADI == input$fishname | MAL_ADI == input$fishname2) ,aes(x=YILAY,y=AYLIK_ORTALAMA_UCRET,color = MAL_ADI, group = MAL_ADI)) + geom_line()
        }
        
        else
        {
            ggplot(fish_market_ym %>% filter(MAL_ADI == input$fishname),aes(x=YILAY,y=AYLIK_ORTALAMA_UCRET,group=1)) + geom_line()
        }
      
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
