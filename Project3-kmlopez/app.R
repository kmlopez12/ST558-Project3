#Project 3 Karen Lopez Nov. 18, 2020
#code from video 3 used as template for shiny app practice

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tree)
library(randomForest)
beerData <- read_csv("../beer.csv")
beerData <- beerData %>% na.omit()

ui <- dashboardPage(skin="purple",
                    
                    #add title
                    dashboardHeader(title="Analysis of Beer Data",titleWidth=1000),
                    
                    #create sidebar items
                    dashboardSidebar(sidebarMenu(
                        menuItem("Information", tabName = "about", icon = icon("archive")),
                        menuItem("Data Exploration", tabName = "app", icon = icon("laptop")),
                        menuItem("Clustering Analysis", tabName = "cluster", icon = icon("laptop")),
                        menuItem("Data Modeling", tabName = "model", icon = icon("laptop")),
                        menuItem("Exporting Data", tabName = "export", icon = icon("laptop"))
                    )),
                    
                    #define app body
                    dashboardBody(
                        tabItems(
                            #about tab content
                            tabItem(tabName = "about",
                                    fluidRow(
                                        #add in latex functionality if needed
                                        withMathJax(),
                                        
                                        #three columns for each of the three info items
                                        column(4,
                                               #description of App
                                               h1("About the Data"),
                                               #box to contain description
                                               box(background="purple",width=12,
                                                   h4("This data contains over 2000 canned craft beers and over 500 breweries in the US. Two data sets were downloaded from kaggle.com and then combined into one larger data set for this application."),
                                                   h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                                   h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                               )
                                        ),
                                        
                                        column(4,
                                               #purpose of app
                                               h1("Purpose of the App"),
                                               #box to contain purpose
                                               box(background="purple",width=12,
                                                   h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                                   h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                                   h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                                   h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                               )
                                        ),
                                        
                                        column(4,
                                               #how to navigate app
                                               h1("Navigate the App"),
                                               #box to contain navigation
                                               box(background="purple",width=12,
                                                   h4("The controls for the app are located to the left and the visualizations are available on the right."),
                                                   h4("To change the number of successes observed (for example the number of coins landing head side up), the slider on the top left can be used."),
                                                   h4("To change the prior distribution, the hyperparameters can be set using the input boxes on the left.  The changes in this distribution can be seen on the first graph."),
                                                   h4("The resulting changes to the posterior distribution can be seen on the second graph.")
                                               )
                                        )
                                    )
                            ),
                            
                            #app tab layout      
                            tabItem(tabName = "app",
                                    fluidRow(
                                        column(width=3,
                                               box(width=12,
                                                   title="Beer Styles by State Parameter",
                                                   background="purple",
                                                   selectizeInput("state", "State", selected = "NC", choices = levels(as.factor(beerData$state)))
                                               ),
                                               box(width=12,
                                                   title="ABV Distribution Parameter",
                                                   background="purple",
                                                   solidHeader=TRUE,
                                                   sliderInput("bins", "Number of Bins", min = 1, max = 50, value = 25)
                                               )
                                        ),
                                        column(width=9,
                                               fluidRow(
                                                   box(width=5,
                                                       tableOutput("numSum"),
                                                       br(),
                                                       h4("Count of Beer Styles by State")
                                                   ),
                                                   box(width=7,
                                                       plotOutput("graphSum"),
                                                       br(),
                                                       h4("Distribution of All Beer ABV")
                                                   )
                                               )
                                        )
                                    )
                            ),
                            
                            #cluster tab layout      
                            tabItem(tabName = "cluster",
                                    fluidRow(
                                        column(width=3,
                                               box(width=12,
                                                   title="k-means Clustering",
                                                   background="purple",
                                                   selectInput("xvar","X Variable", choices = c("abv","ibu","id","brewery_id", "ounces")),
                                                   selectInput("yvar","Y Variable", choices = c("abv","ibu","id","brewery_id", "ounces")),
                                                   numericInput("clusterCount", "Cluster Count", min=1, max=9, value=5)
                                               )
                                        ),
                                        column(width=9,
                                               box(width=12,
                                                   plotOutput("clusterP"),
                                                   br(),
                                                   h4("Cluster plot")
                                               )
                                        )
                                    )
                            ),
                            
                            #model tab layout      
                            tabItem(tabName = "model",
                                    fluidRow(
                                        column(width=4,
                                               box(width=12,
                                                   title="Modeling Options",
                                                   background="purple",
                                                   #parameters for user to change
                                                   radioButtons("model","Select Model", choices = c("Classification via ABV", "Regression via Style")),
                                                   conditionalPanel(condition = "input.model == 'Classification via ABV'",
                                                                    selectInput("classResponse", "Classification Response", choices = c("brewery_name", "style"))),
                                                   conditionalPanel(condition = "input.model == 'Regression via Style'", 
                                                                    selectInput("regResponse", "Regression Response", choices = c("brewery_id", "abv"))),
                                                   numericInput("trees", "Number of Trees", min=100, max=500, step = 100, value=200),
                                                   #add checkbox for prediction
                                                   checkboxInput("prediction", h5("Turn on Prediction", style = "color:white;", value=0)),
                                                   #add conditionalPanel for predictor
                                                   conditionalPanel(condition = "input.prediction == 1",
                                                                    numericInput("predictor", h5("Number for Prediction", value=0, style = "color:white;"),min=0.01, max=10.00, value=1.00),
                                                                    box(width=12,
                                                                        h4("Prediction"),
                                                                        tableOutput("prediction")))
                                               )
                                        ),
                                        column(width=8,
                                               box(width=12,
                                                   h4("Tree Model"),
                                                   plotOutput("modelP")
                                               )
                                        )
                                    )
                            ),
                            
                            #export tab layout      
                            tabItem(tabName = "export",
                                    fluidRow(
                                        column(width=6,
                                               box(width=12,
                                                   title="Table Options",
                                                   background="purple",
                                                   actionButton("export", "Export File")
                                                   
                                               )
                                        ),
                                        column(width=6,
                                               box(width=12,
                                                   title="Table Preview",
                                                   tableOutput("preview")
                                               )
                                               
                                               
                                            
                                        )
                                    )
                            )
                        )
                    )
)

#server logic required to create output
server <- shinyServer(function(input, output) {
    
    #create numeric summary
    output$numSum<-renderTable({
        
        #create function to get data based on input
        getData <- reactive({
            newData <- beerData %>% filter(state == input$state) %>% select(style) %>% na.omit()
        })
        
        #generate table
        table(getData())
        
    })
    
    #create graphical summary  
    output$graphSum <- renderPlot({
        
        #select data to plot
        x <- beerData$abv %>% na.omit()
        
        #get value from input
        bins <- input$bins
        
        #set default if not supplied
        if (is.na(bins)){bins<-25}
        
        #set bins based on input
        binNum <- seq(min(x), max(x), length.out=bins+1)
        
        #plot the numeric summary
        hist(x=x, breaks = binNum, main="Distribution of Alcohol By Volume",xlab="ABV", ylab="Count",type="l")
        
    })
    
    #create cluster plot
    output$clusterP <- renderPlot({
        
        #subset data based on input
        subData <- reactive({
            beerData[, c(input$xvar, input$yvar)] %>% na.omit()
        })
        
        #use input to create cluster count
        clusters <- reactive({
            kmeans(subData(), input$clusterCount)
        })
        
        #create color palette & plot, modified code from clustering lecture 
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        par(mar = c(4, 4, 1, 1)) #set plot margins
        plot(subData(), col = clusters()$cluster, pch = 20, cex = 3, main="k-means Clustering Analysis")
        points(clusters()$centers, pch = 1, cex = 1, lwd = 1)
        
    })
    
    #create models
    output$modelP <- renderPlot({
        
        #input$model
        #choices = c("Classification via ABV", "Regression via Style")
        
        #input$classResponse
        #choices = c("Brewery", "Style")
        
        #input$regResponse
        #choices = c("Brewery", "ABV")
        
        #input$trees
        
        #create variables
        treeFit <- NULL
        predictor <- NULL
        
        #subset data for prediction, 80% train & 20% test
        set.seed(12)
        train <- sample(1:nrow(beerData), size = nrow(beerData)*0.8)
        test <- dplyr::setdiff(1:nrow(beerData), train)
        beerTrain <- beerData[train, ]
        beerTest <- beerData[test, ]
        
        #create tree model based on user input
        if(input$model=="Classification via ABV"){
            #classification tree model & plot
            predictor <- "abv"
            treeFit <- tree(input$classResponse ~ abv, data = beerData)
            plot(treeFit)
            text(treeFit)
        } else {
            #regression random forest tree model
            predictor <- "style"
            treeFit <- randomForest(input$regResponse ~ style, data = beerTrain, mtry = ncol(beerTrain)/3, ntree = input$trees, importance = TRUE)
        }
        
    })
    
    #create prediction
    output$prediction <- renderTable({
        #add option for predictor, conditional on prediction
        if(input$predictor && input$model=="Classification via ABV"){
            predict(treeFit, newData = data.frame(predictor = input$predictor))
        } else if(input$predictor && input$model=="Regression via Style"){    
            predict(treeFit, newData = dplyr::select(beerTrain, -input$regResponse))
        } else {
            
        }
    })
    
    #create output file
    output$preview <- renderTable({
        #use inputs from user (make reactive)
        
        #export table when button is clicked
    })
})

shinyApp(ui = ui, server = server)
