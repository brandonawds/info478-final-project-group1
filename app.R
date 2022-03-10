#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("brandon.R")
source("fatin.R")
# Define UI for application that draws a histogram
ui <- navbarPage("Final Project",
    tabPanel("Mental Health of Disabled People and Covid-19",
             titlePanel("Depression and Anxiety Indicators by Disability Status"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("disorder", "Mental Health Disorder:",
                             c("Depression" = "depression",
                               "Anxiety" = "anxiety")),
                 radioButtons("disabilityDemo", "Disability Demographic",
                              c("With Disability", "Without Disability", "Both")),
               ),
               
               mainPanel(
                 plotOutput(output = "disabilityPlot")
               )
             )),
    tabPanel("Mental Health Through the Phases of the Pandemic",
             titlePanel("Fatin's Page"),
             h1("Phase Comparison Plot"),
             sliderInput(inputId = "phases",
                         label = "Choose Phases to Display",
                         min = 1, max = 6, value = 1),
             h3("Phase Time Periods"),
             tableOutput("table"),
             plotOutput("plot"))
    
)

server <- function(input, output) {
    
    output$disabilityPlot <- renderPlot({
      if(input$disabilityDemo == "With Disability" && input$disorder == "depression") {
        plot(disability_depression_plot)
      } else if(input$disabilityDemo == "Without Disability" && input$disorder == "depression") {
        plot(without_disability_depression_plot)
      } else if (input$disabilityDemo == "Both" && input$disorder == "depression"){
        plot(both_disability_depression_plot)
      } else if(input$disabilityDemo == "With Disability" && input$disorder == "anxiety") {
        plot(disability_anxiety_plot)
      } else if(input$disabilityDemo == "Without Disability" && input$disorder == "anxiety") {
        plot(without_disability_anxiety_plot)
      } else if (input$disabilityDemo == "Both" && input$disorder == "anxiety"){
        plot(both_disability_anxiety_plot)
      }
    })
    
    df <- read.csv("phasedata.csv")
    
    output$table <- renderTable(
      df[,2:3]
    )
    
    sliderValues <- reactive({
      df[1: input$phases,]
    })
    
    output$plot <- renderPlot({
      g <- ggplot(sliderValues(), aes(x= c2, y= c1))
      g + geom_bar(stat = "sum") + xlab("Phases") + ylab("Mean Value") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
