#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
source("brandon.R")
source("fatin.R")
source("distribution_of_variable_brandon.r")
# Define UI for application that draws a histogram
ui <- navbarPage("INFO 478 Final Project: Mental Health and Covid-19",
      tabPanel("Overview",
                titlePanel("Mental Health and Covid-19"),
                h1("By: Brandon Ly, Fatin Almaroof, Tiffany Tse, Meena Attringal"),
                h3("The purpose of our project is to examine the ways in which COVID-19 and the pandemic has affected the health of individuals in the United States.
                   We will analyze data from the CDC and various other resources that provide us data on the rate of COVID-19 cases and the reported signs/symptoms of 
                   various mental health disorders such as depression and anxeity. The question we hope to answer is: to what extend has COVID-19 impacted the mental 
                   health of adults in the Unites States?"),
               h3("We believe that the answer to this question is useful for the general public to understand.
                  Our hope is to help spread awareness of the effects of COVID-19 and to contribute quantitative facts about the mental health of Americans.
                  We want these figures to help normalize conversations about mental health and show how many people were affected mentally due to the pandemic.")
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
             plotOutput("plot"),
             ),
    tabPanel("Conclusion and Insights",
             titlePanel("Conclusions and Insights"),
             h1("Mental Health of Disabled People in a Pandemic"),
             h2("Methods"),
             h3("The dataset was filtered by either symptoms of depressive or anxiety disorder. 
                NA values were removed. Because this visualization focused on understanding mental health of those with and without disabilities, 
                the data was also filtered by with disability or without before being displayed on the visualization. "),
             h2("Analysis and Results"),
             h3("For both depression and anxiety, we can see from the graphs that the percentage of survey respondents who indicated symptoms of these mental disorders remained relatively consistent throughout. 
                About 16-18% percent of the survey respondents who do not have a disability showed indications for depression (the red line) throughout the data collection time period. While about 46-54% of survey respondents who have a disability showed indications for depression (blue line).
                About 20-25% percent of the survey respondents who do not have a disability showed indications for anxiety (the red line) throughout the data collection time period. While about 50-58% of survey respondents who have a disability showed indications for anxiety (blue line).
                There is a notable drop in percentage of survey respondents with a disability and indicate either mental disorder (anxiety or depression) during the middle of the data collection period (Summer 2021, June-July). Overall, we can observe that people with disabilities are suffering from mental health disorders at a much higher rate than those without disabilities"),
             h2("Limitations"),
             h3("A limitation that the survey's technical documentation covered is the nonresponse bias. Some of those who received this email about this survey did not fill it out, potentially skewing some results. Some Americans may not own a device that can access the internet, may not have internet, or may not have an email account."),
             h1("Mental Health Through the Phases of the Pandemic"),
             h2("The above visual correlates to the percentage of cases that reported mental disorders in each phase of Covid. Firstly, what we found helpful that we were able to divide the data set based on timeline to 6 different phases. Within each of these phase we were able to analyze the percentile of mental health disorders reported. While we didn't want to expand the variables too much with analysis for each separate mental health disorder, we decided to keep it concise and combine all mental disorders into different phases and find the average for each of these phases.
By doing this we found that the most health disorders reported was during phase 3 and the least was during phase 4. What we found surprising was that the least and greatest reporting of health disorders occurred in consecutive phases. However, this does make reasonable sense when correlating this data in terms of how strict the government was while enforcing quarantine restrictions. 
When there is more enforcement to quarantine, individuals tend to socialize less and be more isolated and alone at home. This ties in with the high mental health disorder reporting rates. Once the government strictly enforces Covid restrictions, the Covid rates ultimately decrease as well. Therefore, once the ban for such resurrections is taken people tend to socialize and go out far more, reverting back to their normal lifestyle. This ultimately ties in with the data that shows the phase 4 as being the lowest percentile phase for reporting health disorders.
Overall, this shows a strong correlation with the percentile of mental disorder cases reported and the kind of social lifestyle that users are redirected to be put in.  ")
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
deployApp(appName="info478final")
