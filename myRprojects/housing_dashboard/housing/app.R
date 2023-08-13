#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
read.csv('housing.csv')
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Housing Graphics"),
    
    mainPanel(
      plotOutput("count"),
      fluidRow(    
    # Show a plot of the count by location
        column(6, plotOutput("plot2")),
        column(6, plotOutput("plot3"))
        ))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$count <- renderPlot({
      household_location = housing %>% group_by(ocean_proximity) %>%
        summarise(sum_households=sum(households))
      
      ttl_households = household_location %>% as.data.frame()
      
      ggplot(
        data = ttl_households,
        aes(x = reorder(ocean_proximity, -sum_households), y = sum_households/1000000))+
        geom_col(fill = "deepskyblue3")+
        ggtitle("Household Count by Ocean Proximity")+
        xlab("Location")+
        ylab("Number of Houses (millions)")
      })
    
    output$plot2 <- renderPlot({
      ggplot(
        data = housing,
        aes(x = population, y = households))+
        geom_point()+
        ggtitle("Region Population VS Households")+
        xlab("Population")+
        ylab("Households")
    })
    
    output$plot3 <- renderPlot({
      ggplot(
        data = housing,
        aes(x = total_rooms, y = total_bedrooms))+
        geom_point()+
        ggtitle("Total Rooms vs. Total Bedrooms")+
        xlab("Total Rooms")+
        ylab("Total Bedrooms")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
