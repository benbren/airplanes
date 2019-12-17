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
library(tidyverse)
library(tidytext)
library(tm)
library(glmnet)

# load data, function
LR_cov_model = readRDS("../rds/logistic_model_cov.rds")
col_names_cov = readRDS("../rds/col_names_cov.rds")
col_names_cov[which(col_names_cov == "break")] = "brk"


# airline list
# airline = c("Delta Airline", "American Airline", "United Airline", "Jetblue Airline")
US = c("US", "Not US")
Rich = c("Rich", "Poor")

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
            #selectInput("selection_airline", label = "Select type of Airline", choices = airline),
            selectInput("selection_country", label = "Select type of Cabin", choices = US),
            selectInput("selection_income", label = "Select type of Traveller", choices = Rich),
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
        ##### dropdown ########
        country = input$selection_country
        income = input$selection_income
        if (country == "US"){ us_cat = 1
        } else {  us_cat = 0} 
        
        if (income == "Rich"){ rich_cat = 1
        } else { rich_cat = 0 } 
        
        ###### comment ########
        comment = input$text_comment
        test = data.frame(ID = 1, content = comment) 
        
        test = test %>% 
            unnest_tokens(word, content)
        bing = get_sentiments(lexicon = "bing")
        
        
        test_bing = test %>%
            select(ID, word) %>%
            unique() %>%
            left_join(bing, by = "word") %>%
            na.omit()
        
        # add UNK if there are no words
        if (dim(test_bing)[1] == 0){
            UNK = data.frame(ID = 1, word = "UNK", sentiment = NA)
            test_bing = rbind(test_bing, UNK)
        } else {test_bing = test_bing}
        
        test_dtm = test_bing %>%
            #get word count per document to pass to cast_dtm
            count(ID, word, sort = TRUE) %>%
            ungroup() %>%
            #create a DTM with docs as rows and words as columns
            cast_dtm(document = ID, term = word, value = n)
        
        # create the big matrix contain all the words (like in training set)
        test_input = colnames(as.matrix(test_dtm))
        test_matrix = matrix(0, nrow = 1, ncol = length(col_names_cov))
        colnames(test_matrix) = col_names_cov
        for(i in test_input){
            if (i == "break"){
                i == "brk"
            }
            pos = as.vector(grep(i, colnames(test_matrix)))[1]
            test_matrix[1,pos] = 1
        }
        
        test_matrix[1,3513] = us_cat
        test_matrix[1,3514] = rich_cat
        
        
        result = ifelse(predict.glmnet(LR_cov_model, test_matrix, type="response") > 0.5,1,0)
        
        # result = predict(classifier, newdata = t(as.matrix(test)))
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


