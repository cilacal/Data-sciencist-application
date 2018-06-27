if (!require("shiny")){
  install.packages("shiny")
  library(shiny)
} else library(shiny)
if (!require("ggplot2")){
  install.packages("ggplot2")
  library(ggplot2)
} else library(ggplot2)
if (!require("data.table")){
  install.packages("data.table")
  library(data.table)
} else library(data.table)
if (!require("rstudioapi")){
  install.packages("rstudioapi")
  library(rstudioapi)
} else library(rstudioapi)

#Loading the data
wd <- getActiveDocumentContext()$path
wd <- gsub("/","//",wd)
wd <- gsub("//shiny_app//server.R","",wd)
setwd(wd)
data_train <- as.data.table(read.csv2("data_train.csv"))
colnames(data_train)[1] <- "age"

#Adding a column with IDs to the data
data_train$ID <- 1:nrow(data_train)
#Correcting data quality issues
data_train$marital.status <- as.factor(gsub(" ","",as.character(data_train$marital.status)))
levels(data_train$marital.status)[levels(data_train$marital.status) == ""] <- "Missing data"

#Changing the order of the levels not to show the "Missing data" first
data_train$marital.status <- factor(data_train$marital.status,levels(data_train$marital.status)[c(2:length(levels(data_train$marital.status)),1)])

#### Shiny application to show the distribution of age by marital-status ####
shinyServer(function(input, output){
  #Using the function renderPlot() the graph is going to be regenerated
  # every time if the input variable is changed
  output$distPlot <- renderPlot({
    #unique(data_train$marital.status)
    #filtering the data for the plot according to the selected marital.status
    data_train2 <- data_train[as.character(data_train$marital.status) == input$marital.status]
    #Creating a pivot-like table for plotting
    prep_data <- data_train2[,list("nr_of_ppl" = length(ID)),
                             by = list("age" = age)]
    #Creating the graph based on the pivot-like table
    dist <- ggplot(prep_data, aes(x = age,y = nr_of_ppl)) + 
            geom_histogram(stat ="identity") + labs(x = "Age", y = "Number of the people")
    print(dist)
  })
  
})

