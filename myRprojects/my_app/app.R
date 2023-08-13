#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
load("Tree.RData")
library(shiny)
library(RColorBrewer)
library(ggplot2)
palette = brewer.pal(3, "Set2")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
titlePanel(h1("Iris Data", align = "center")),
img(src='iris.jpg', height=180, width=1300),
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("Iris Species"),
      p("Select the Petal length and width of your iris to determine what species of Iris it is."),
      sliderInput("length", h3("Petal Length (cm)"),
                  min = 1, max = 7, value = 4),
      sliderInput("width", h3("Petal Width (cm)"),
                  min = 0, max = 2.5, step = 0.25, value = 1.5),
    ),
    mainPanel(
      h3(textOutput("text")), align = "center",
      plotOutput("iris_predict"),
      h3("Petal Length Distribution"),
      plotOutput('iris_violin'),
      h3("Petal Length vs. Petal Width"),
      plotOutput('ratio')
    )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$text = renderText({
    predictors = data.frame(
      Petal.Length = input$length,
      Petal.Width = input$width,
      Sepal.Length = 0,
      Sepal.Width = 0)
    
    prediction = predict(
      object = model,
      newdata = predictors,
      type = "class")
    
    paste("The predicted species is ",
          as.character(prediction))
  })
  
  #plot iris data
  output$iris_predict = renderPlot({
    plot(
      x = iris$Petal.Length,
      y = iris$Petal.Width,
      pch = 19,
      col = palette[as.numeric(iris$Species)],
      main = "Iris Petal Length vs. Petal Width",
      xlab = "Petal Length",
      ylab = "Petal Width")
    
    #plot decision tree boundaries
    partition.tree(
      tree = model,
      label = "Species",
      add = TRUE)
    
    #draw prediction on plot
    points(
      x = input$length,
      y = input$width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2)
  })
  
  output$iris_violin = renderPlot({
    ggplot(
      data = iris,
      aes(x=Species, y = Petal.Length)) +
      geom_violin(fill= "deepskyblue3") +
      ggtitle("Distribution of Petal Length") +
      xlab("Species") +
      ylab("Petal Length (cm)")
  })
  
  iris["Petal.Ratio"] = iris$Petal.Length / iris$Petal.Width
  
  output$ratio = renderPlot({
    ggplot(
      data = iris,
      aes(x = Species, y=Petal.Ratio)) +
      geom_boxplot(fill = "deepskyblue3") +
      ggtitle("Ratio of Petal Length to Petal Width") +
      xlab("Species") +
      ylab("Petal Length/Petal Width")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
