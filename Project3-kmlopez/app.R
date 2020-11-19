#Project 3 Karen Lopez Nov. 18, 2020
#code from video 3 used as template for shiny app practice

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tree)
library(randomForest)
library(knitr)
library(ggplot2)
library(plotly)
beerData <- read_csv("../beer.csv") #read in data
beerData <- beerData %>% na.omit() #remove NAs

ui <- dashboardPage(skin="purple",
                    
                    #add title
                    dashboardHeader(title="Analysis of Beer Data", titleWidth=1000),
                    
                    #create sidebar with 5 tabs
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
                                                   h4(HTML("The data used in this application contains over 2000 canned craft beers and over 500 breweries in the United States. The two data sets that were downloaded from kaggle.com and combined to create this data set are <a href=https://www.kaggle.com/nickhould/craft-cans?select=beers.csv>beers.csv</a> and <a href=https://www.kaggle.com/nickhould/craft-cans?select=breweries.csv>breweries.csv</a>.")),
                                                   h4("The data set includes information such as abv (alcohol by volume), ibu, numeric beer id, beer name, beer style, ounces, brewery name, brewery city, and brewery state. To avoid errors in analysis, any entries with NA values are removed. According to the description, this data was collected in January 2017 from CraftCans.com, which is currently a suspended site.")
                                               )
                                        ),
                                        
                                        column(4,
                                               #purpose of app
                                               h1("App Purpose"),
                                               #box to contain purpose
                                               box(background="purple",width=12,
                                                   h4("This application provides a means to explore the beer data, demonstrates ways to explore relationships in the data, and allows for export of desired parts of the data. This data set does not have a large set of variables, but this application can easily be edited to conform to larger data sets with more variables. Some of the relationships are explored via numerical and categorical plots, clustering analysis, and tree modeling."),
                                                   h4("Most of the relationships demonstrated in this application are not as meaningful as originally desired, but serves as examples of what can be done with beer data. Ideally, a data set with additional variables, such as shelf life, rating, average price, production cost, time to sell out, and distribution radius, would result in more useful analyses.")
                                               )
                                        ),
                                        
                                        column(4,
                                               #how to navigate app
                                               h1("App Navigation"),
                                               #box to contain navigation
                                               box(background="purple",width=12,
                                                   h4(HTML("There are four tabs in the navigation panel that lead to the four utilities of this application. <b>Data Exploration</b> contains a numeric summary and saveable/interactive plotly histogram, <b>Clustering Analysis</b> contains a saveable k-means cluster analysis, <b>Data Modeling</b> contains classification and regression tree models, and <b>Exporting Data</b> allows for reviewing, subsetting, and saving the data.")),
                                                   h4("Each tab has controls on the left side that can be modified by the user. The resulting changes are viewed in the visualizations that are located to the right of the input controls. These controls vary from drop-down menus to selection, and some even cause secondary controls to appear.")
                                               )
                                        )
                                    )
                            ),
                            
                            #app tab layout
                            tabItem(tabName = "app",
                                    fluidRow(
                                        column(width=3,
                                               #box to allow user to select state
                                               box(width=12, 
                                                   title="Beer Styles by State Parameter",
                                                   background="purple",
                                                   selectizeInput("state", "State", selected = "NC", choices = levels(as.factor(beerData$state)))
                                               ),
                                               #box to allow user to select bin number
                                               box(width=12, 
                                                   title="ABV Distribution Parameter",
                                                   background="purple",
                                                   solidHeader=TRUE,
                                                   sliderInput("bins", "Number of Bins", min = 1, max = 50, value = 25)
                                               )
                                        ),
                                        column(width=9,
                                               fluidRow(
                                                   #output box for numeric summary table
                                                   box(width=5, 
                                                       tableOutput("numSum"),
                                                       br(),
                                                       h4("Count of Beer Styles by State")
                                                   ),
                                                   #output box for graphical summary plot
                                                   box(width=7, 
                                                       plotlyOutput("graphSum"),
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
                                               #box to allow user to select k-means cluster plot variables
                                               box(width=12,
                                                   title="k-means Clustering",
                                                   background="purple",
                                                   selectInput("xvar","X Variable", choices = c("abv","ibu","id","brewery_id", "ounces")),
                                                   selectInput("yvar","Y Variable", choices = c("abv","ibu","id","brewery_id", "ounces")),
                                                   numericInput("clusterCount", "Cluster Count", min=1, max=9, value=5),
                                                   downloadButton("savePlot", "Save Plot")
                                               )
                                        ),
                                        column(width=9,
                                               #output box for the cluster plot
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
                                               #box to allow user to select model options
                                               box(width=12,
                                                   title="Modeling Options",
                                                   h6(HTML("These models use a small subset of the data set, in order to keep factor numbers under the threshold. The variable shown below, <em>m</em>, equals randomly selected predictors.")),
                                                   br(),
                                                   background="purple",
                                                   #parameters for user to change
                                                   radioButtons("model","Select Model", choices = c("Classification via ABV", "Regression via Style")),
                                                   #add conditional panel for classification variable selection
                                                   conditionalPanel(condition = "input.model == 'Classification via ABV'",
                                                                    withMathJax(h5("$$m = \\sqrt{p}$$")),
                                                                    selectInput("classResponse", "Classification Response", choices = c("state", "brewery_name"))),
                                                   #add conditional panel for regression variable selection
                                                   conditionalPanel(condition = "input.model == 'Regression via Style'", 
                                                                    withMathJax(h5("$$m = \\frac{p}{3}$$")),
                                                                    selectInput("regResponse", "Regression Response", choices = c("brewery_id", "abv"))),
                                                   #add conditional panel for tree number
                                                   conditionalPanel(condition = "input.model == 'Regression via Style'",
                                                       numericInput("trees", "Number of Trees", min=100, max=500, step = 100, value=200)
                                                   ),
                                                   #add checkbox for prediction
                                                   checkboxInput("prediction", h5("Turn on Prediction", style = "color:white;", value=0)),
                                                   #add conditionalPanel for predictor
                                                   conditionalPanel(condition = "input.prediction == 1",
                                                                    numericInput("predictor", h5("Number for Prediction", value=0, style = "color:white;"),min=0.01, max=10.00, value=1.00)
                                                   )
                                               )
                                        ),
                                        column(width=8,
                                               #box for tree model output
                                               box(width=12,
                                                   h4("Tree Model"),
                                                   plotOutput("modelP")
                                               ),
                                               #add conditional panel for prediction results
                                               conditionalPanel(condition = "input.prediction == 1",
                                                                box(width=12,
                                                                    h4("Prediction"),
                                                                    tableOutput("prediction")))
                                        )
                                    )
                            ),
                            
                            #export tab layout
                            tabItem(tabName = "export",
                                    fluidRow(
                                        column(width=3,
                                               #box for user to select variables for export file
                                               box(width=12,
                                                   title="CSV Export Options",
                                                   background="purple",
                                                   selectInput("var1", "Variable 1", choices = c("abv", "ibu", "id", "beer_name", "style", "brewery_id", "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var2", "Variable 2", choices = c("ibu", "abv", "id", "beer_name", "style", "brewery_id", "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var3", "Variable 3", choices = c("id", "abv", "ibu", "beer_name", "style", "brewery_id", "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var4", "Variable 4", choices = c("beer_name", "abv", "ibu", "id", "style", "brewery_id", "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var5", "Variable 5", choices = c("style", "abv", "ibu", "id", "beer_name", "brewery_id", "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var6", "Variable 6", choices = c("brewery_id","abv", "ibu", "id", "beer_name", "style",  "ounces", "brewery_name", "city", "state", "none")),
                                                   selectInput("var7", "Variable 7", choices = c("ounces", "abv", "ibu", "id", "beer_name", "style", "brewery_id", "brewery_name", "city", "state", "none")),
                                                   selectInput("var8", "Variable 8", choices = c("brewery_name", "abv", "ibu", "id", "beer_name", "style", "brewery_id", "ounces", "city", "state", "none")),
                                                   selectInput("var9", "Variable 9", choices = c("city", "abv", "ibu", "id", "beer_name", "style", "brewery_id", "ounces", "brewery_name", "state", "none")),
                                                   selectInput("var10", "Variable 10", choices = c("state", "abv", "ibu", "id", "beer_name", "style", "brewery_id", "ounces", "brewery_name", "city", "none")),
                                                   downloadButton("export", "Export File")
                                               )
                                        ),
                                        column(width=9,
                                               #box for user to preview and interact with all data
                                               box(width=12,
                                                   title="Preview All Data",
                                                   DT::dataTableOutput("preview")
                                               )
                                               
                                               
                                            
                                        )
                                    )
                            )
                        )
                    )
)

#server logic required to create output
server <- shinyServer(function(input, output, session) {
    
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
    output$graphSum <- renderPlotly({
        
        #select data to plot
        #x <- beerData$abv %>% na.omit()
        
        #get value from input
        bins <- input$bins
        
        #set default if not supplied
        if (is.na(bins)){bins<-25}
        
        #set bins based on input
        #binNum <- seq(min(x), max(x), length.out=bins+1)
        
        #plot the numeric summary
        #hist(x=x, breaks = binNum, main="Distribution of Alcohol By Volume",xlab="ABV", ylab="Count",type="l")
        
        #converted plot to plotly so user can hover over plot
        p <- ggplot(beerData, aes(abv)) + geom_histogram(bins=bins)
        ggplotly(p)
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
    
    #save cluster plot
    output$savePlot <- downloadHandler(
        plotPNG <- function(){
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
        },
        #save .png file for when button is clicked
        filename = function(){"clusterPlot.png"},
        content = function(file){
            #ggsave(file, subData(), device = png)
            ggsave(file, plotPNG(), device = png, width = 2, height = 2, units = "cm")#scale = 0.1) #image too large, even after reducing dimensions & scale
        }
    )
    
    #create tree models
    output$modelP <- renderPlot({
        
        #input$model
        #choices = c("Classification via ABV", "Regression via Style")
        
        #input$classResponse
        #choices = c("state", "brewery_name")
        
        #input$regResponse
        #choices = c("brewery_id", "abv")
        
        #input$trees
        
        #create variables
        treeFit <- NULL
        predictor <- NULL
        
        #subset data for modeling, otherwise there are too many factors
        beerDataSub <- head(beerData, n=100)
        
        #subset data for prediction, 80% train & 20% test
        set.seed(12)
        train <- sample(1:nrow(beerDataSub), size = nrow(beerDataSub)*0.8)
        test <- dplyr::setdiff(1:nrow(beerDataSub), train)
        beerTrain <- beerDataSub[train, ]
        beerTest <- beerDataSub[test, ]
        
        #create tree model based on user input
        if(input$model=="Classification via ABV"){
            #classification tree model & plot
            predictor <- "abv"
            if(input$classResponse=="state"){
                treeFit <- tree(as.factor(state) ~ abv, data = beerDataSub)
                plot(treeFit)
                text(treeFit)
            } else {
                treeFit <- tree(as.factor(brewery_name) ~ abv, data = beerDataSub)
                plot(treeFit)
                text(treeFit)
            }
        } else {
            #regression random forest tree model & plot
            predictor <- "style"
            if(input$regResponse=="brewery_id"){
                treeFit <- randomForest(as.factor(brewery_id) ~ style, data = beerTrain, mtry = ncol(beerTrain)/3, ntree = input$trees, importance = TRUE)
                plot(treeFit)
            } else {
                treeFit <- randomForest(as.factor(abv) ~ style, data = beerTrain, mtry = ncol(beerTrain)/3, ntree = input$trees, importance = TRUE)
                plot(treeFit)
            }
        }
        
    })
    #browser()
    #create prediction
    output$prediction <- renderTable({
        #add option for predictor, conditional on prediction
        if(input$predictor && input$model=="Classification via ABV"){
            predict(treeFit, newData = data.frame(predictor = input$predictor))
        } else if(input$predictor && input$model=="Regression via Style"){    
            predict(treeFit, newData = dplyr::select(beerTrain, -input$regResponse))
        }
    })
    
    #create output file
    output$preview <- DT::renderDataTable({
        #allow user to scroll through data 
        beerData[, 2:11]
    })
    
    #function for subsetting data based on user input
    getExport <- reactive({
        #create empty list
        columns <- ""
        
        for(i in 1:10){
            if(paste0("var", i)!="none"){
                columns <- paste0(as.character(columns), as.character(paste0(var, i, ", ")))
            }
        } 
        
        #subset the string to remove last 2 characters
        columns <- substring(columns, first = 1, last = nchar(columns)-2)
        
        #use columns from user to subset data
        beerDataSub <- beerData[, columns]
        print(beerDataSub)
    })
    #browser()
    output$export <- downloadHandler(
        #export table when button is clicked
        filename = function(){"beerDataSub.csv"},
        content = function(file){
            write.csv(getExport(), file)
        }
    )
    
})

shinyApp(ui = ui, server = server)