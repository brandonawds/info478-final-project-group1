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
ui <- navbarPage("INFO 478 Final Project: Mental Health and Covid-19",
      tabPanel("Overview",
                titlePanel("Mental Health and Covid-19"),
                h1("By: Brandon Ly, Fatin Almaroof, Tiffany Tse, Meena Attringal"),
                h3("The purpose of our project is to examine the ways in which COVID-19 and the pandemic has affected the health of individuals in the United States.
                   We will analyze data from the CDC and various other resources that provide us data on the rate of COVID-19 cases and the reported signs/symptoms of 
                   various mental health disorders such as depression and anxeity. The question we hope to answer is: to what extend has COVID-19 impacted the mental 
                   health of adults in the Unites States?")
                 ),
    tabPanel("Mental Health of Disabled People and Covid-19",
             titlePanel("Depression and Anxiety Indicators by Disability Status"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("disorder", "Mental Health Disorder:",
                             c("Depression" = "depression",
                               "Anxiety" = "anxiety")),
                 radioButtons("disabilityDemo", "Disability Status:",
                              c("With Disability", "Without Disability", "Both")),
               ),
               
               mainPanel(
                 plotOutput(output = "disabilityPlot")
               )
             )),
    tabPanel("Mental Health Through the Phases of the Pandemic",
             titlePanel("Mental Health Through the Phases of the Pandemic"),
             h1("Phase Comparison Plot"),
             sliderInput(inputId = "phases",
                         label = "Choose Phases to Display",
                         min = 1, max = 6, value = 1),
             h3("Phase Time Periods"),
             tableOutput("table"),
             plotOutput("plot")
             ),
    tabPanel("Conclusion and Insights",
             titlePanel("Conclusions and Insights"),
             h1("Mental Health of Disabled People in a Pandemic"),
             h2("ANALYSIS"),
             h1("Mental Health Through the Phases of the Pandemic"),
             h2("ANALYSIS")
             )
    
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
