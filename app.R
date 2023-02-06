

library(shiny)
library(shinythemes)
library(MASS)
library(dplyr)
library(Hmisc)
library(psych)
library(corrplot)



ui <- fluidPage(theme = shinytheme("united"),

   navbarPage(
       "House Price Prediction",
       tabPanel("Dataset",
                mainPanel(
                h1("Dataset"),
                dataTableOutput("dataset")
                
                ) 
                
       ),
    tabPanel("Price Prediction",
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId='marla', label='Enter Marla of House : ', value = 2,min = 2, max = 50),
            numericInput(inputId='bedrooms', label='Enter bedrooms of House : ' , value = 1,min = 1, max = 9),
            numericInput(inputId='bathrooms', label='Enter bathrooms of House : ', value = 1,min = 1, max =8),
            numericInput(inputId='floors', label='Enter floors of House : ', value = 1,min = 1, max = 4),
            numericInput(inputId='waterfront', label=' Do you want waterfront in House (Enter 0 for NO and 1 for YES) : ', value = 0,min = 0, max = 1),
            numericInput(inputId='condition', label=' Enter view of house (From range  0 to 4): ', value = 0,min = 0, max = 4),
            numericInput(inputId='view', label='Enter Condition of house (From range  0 to 5): ', value = 0,min = 0, max = 4),
            numericInput(inputId='basement', label=' Do you want basement in House (Enter 0 for NO and 1 for YES) : ', value = 0,min = 0, max = 1),
            
        ),
    
        
        
            mainPanel(
                tags$b("Predicted Price of House"),
                verbatimTextOutput("Pred"),
                plotOutput("plot1"),
                br(),
                plotOutput("plot2")
                
                
                
                )
        )
    ),
    tabPanel("HIstograms",
             sidebarLayout(
                 sidebarPanel(
                     sliderInput(inputId = "bins",
                                 "Number of Column:",
                                 min = 1,
                                 max = 9,
                                 value = 1)
                 ),
        
                 mainPanel(
                     tags$b("Histrogram for Analyzing Dataset"),
                     plotOutput("histogram")
                 )
            
    )
    
    
   )
)

)



server <- function(input, output) {
    
    dataset=Project_Dataset
    tprice=0
    tprice=as.integer(tprice)
    
    train <- subset(dataset, select=c(price, marla, bedrooms, bathrooms, floors, waterfront, view, condition, basement))

    sapply(train, function(x) sum(is.na(x)))

    
    
    model <-  lm(train$price ~ ., data=train)

    cor(train$marla, train$bedrooms+train$bathrooms, method='pearson')

    confint(model, conf.level=0.95)

    db <- reactive({
        tmarla=input$marla
        tbedrooms=input$bedrooms
        tbathrooms=input$bathrooms
        tfloors=input$floors
        twaterfront=input$waterfront
        tcondition=input$condition
        tview=input$view
        tbasement=input$basement
        matrix(c(tprice,tmarla, tbedrooms, tbathrooms, tfloors, twaterfront, tview, tcondition, tbasement),ncol=9)
        

       
    })
   

    pred<- reactive({
        test=data.frame(db())
        colnames(test) <- c("price", "marla", "bedrooms", "bathrooms", "floors", "waterfront", "view", "condition", "basement")
        
        predict(model,test)
    })
    
    
    
    
    
    output$dataset <- renderDataTable({
        Project_Dataset
    }) 
    output$Pred <- renderText(pred())
    output$plot1 <- renderPlot({
        
        plot(input$marla,pred(),xlab = "Marla", ylab = "Predicted Price")
    }) 
    
    output$plot2 <- renderPlot({
    str(dataset)
    col_numeric <- sapply(dataset, is.numeric)
    numericalVars <- dataset[,col_numeric]
    str(numericalVars)
    corrMat <- cor(numericalVars)
    options(scipen=10)
    corrplot(corrMat, method = "ellipse",title = " Correlation Plot of Dataset ")
    })
    
    output$histogram <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- dataset[, input$bins]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        options(scipen=10)
        
        hist(x, breaks = bins, col = 'darkgray', border = 'red')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
