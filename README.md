# ST558-Project3
final exam project

### Packages required

code to add all packages:  

library(c("shiny", "shinydashboard", "tidyverse", "tree", "randomForest", "knitr", "ggplot2"))  

### Run repo

code to run repo from RStudio:  

`shiny::runGitHub("ST558-Project3", username = "kmlopez12", ref = "master", subdir = "/Project3-kmlopez/")`  


### Data
Data obtained from the two websites listed here and combined using code in project3data.Rmd file.  
  - <https://www.kaggle.com/nickhould/craft-cans?select=beers.csv>
  - <https://www.kaggle.com/nickhould/craft-cans?select=breweries.csv>

We will run your app from gitHub using RStudio so make sure it is set up correctly to do that (see lecture for reference to setting it up, try it yourself to make sure it works!).  

Be sure to have a section on your readme page that includes all packages required for the app to work. You should have a line that would install all the packages used (so we can easily grab that and run it prior to running your app).  

You should also include a line of code on the readme that we can copy and paste that will run your repo from RStudio. You should check that this works with an “empty” version of RStudio (that is, one that doesn’t have objects you’ve already created existing in your environment). You want to make sure anyone can run the app using the code!