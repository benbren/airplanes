#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# load data, function
source("~/Desktop/untitled folder/svm_testing.R")

# airline list
airline = c("Delta Airline", "American Airline", "United Airline", "Jetblue Airline")
cabin = c("Econony", "Premium Economy", "Business", "First Class")
traveller = c("Business", "Couple", "Family", "Solo")

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Customer Review Prediction")

sidebar <- dashboardSidebar(
    # selection: select basket
    h3(" User Instruction:"),
    helpText("This dashboard is created for Airline marketing team in prediction of customers recommendation using (method)....")
    
)

body <- dashboardBody(
    
    
    fluidRow(
        box(title = "Text comments",
            textAreaInput("text_comment", "Please type your comments:", row = 10),
            status = "primary"),
        
        box(title = "Dropdown selection",
            selectInput("selection_airline", label = "Select type of Airline", choices = airline),
            selectInput("selection_cabin", label = "Select type of Cabin", choices = cabin),
            selectInput("selection_traveller", label = "Select type of Traveller", choices = traveller),
            status = "primary")
    ),
    
    fluidRow(
        box(title = "Customer Experience:",
            sliderInput("overall_rating", "Please rate your overall experience:", 1, 10, 1),
            sliderInput("cabin_rating", "Please rate your experience on cabin staff:", 1, 10, 1),
            sliderInput("wifi_connectivity", "Please rate your experience on wifi connectivity:", 1, 10, 1),
            status = "primary"),
        
        box(title = "Results predicted from the model",
            textOutput("result"),
            status = "warning")
    )
    
)



# Define server logic required to draw a histogram
server <- function(input, output){
    output$result = renderText({
        test = c()
        traveller = input$selection_traveller
        overall_r = input$overall_rating
        cabin_r = input$cabin_rating
        wifi_r = input$wifi_connectivity
        if (traveller == "Business"){
            pat = c(1,0,0,0)
            test = c(test, pat)
        } else if (traveller == "Couple"){
            pat = c(0,1,0,0)
            test = c(test, pat)
        } else if (traveller == "Family"){
            pat = c(0,0,1,0)
            test = c(test, pat)
        } else{
            pat = c(0,0,0,1)
            test = c(test, pat)
        }
        test = c(test, overall_r,cabin_r,wifi_r)
        result = predict(classifier, newdata = t(as.matrix(test)))
        if (as.vector(result) == 0){
            paste("Not recommend")
        } else {paste("Recommend!!")}
    })

    
}

# Run the application 
# Run the application 
# shinyApp(ui = ui, server = server)
shinyApp(
    ui = dashboardPage(
        header,
        sidebar,
        body
    ),
    server = server
)


