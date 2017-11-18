#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      sidebarPanel(
        # h3("Input Area"),
        textInput(inputId = "Input",
                  label = "Enter your text input here:",
                  value = "",
                  placeholder = "E.g., Mary had a little"),
        actionButton(inputId = "button",
                     label = "Click for likeliest next word"),
        h5("Note:  data may take a moment to load")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       # plotOutput("distPlot"),
       # textOutput("Head"),
        h1("Predicted Next Word:"),
        p(strong(h2(span(textOutput("Word1"), style = "color:royalblue")))),
        hr(),
        # textOutput("Word2"),
        # textOutput("Word3")
        # tableOutput("Table"),
        # plotOutput("distPlotBase"),
        plotOutput("distPlotGG")
    )
  )
))
