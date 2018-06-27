############################
#   Engineering a dataset  #
############################

# Create a list of numbers with the following properties:
#  - n >= 100
#  - mean = 1000 (+/- 0.5)
#  - sd = 10 (+/- 0.1)

# Generation of the data
data_set <- rnorm(10000, mean = 1000, sd = 10)

# Check the solution 
length(data_set)          # > 100
any(duplicated(data_set)) # distinct values
mean(data_set)            # mean = 1000(+/- 0.5)
sd(data_set)              #sd 10(+/- 0.1)

##########################
#  Writing a simulation  #
##########################

options(scipen = 999)
sim_country <- function(n_couple = 10000,boy_needed = TRUE){
  # We assume that the couples are made by a male and a female
  pop_origin <- c("male"= n_couple/2, "female" = n_couple/2)
  # We simulate the birth of a child
  for(i in 1:n_couple){
    # The couples continue to have childer until they have their first boy
    child <- 3 #initializing number. As it can have only values 0 and 1 later, it doesn't have any effect on the simulation
    while(child != 1){
      #Assuming that the probability of the birth of a boy/girl is 0.5
      #1 denotes the birth of a boy and 0 the birth of a girl
      child <- rbinom(1,1,0.5)
      pop_origin[ifelse(child == 1,"male","female")] <- pop_origin[ifelse(child == 1,"male","female")]+1 
    }
  }
  return(c(pop_origin,"long-term ratio of boys to girls" = pop_origin["male"]/pop_origin["female"]))
}
sim_country()
# Let's run the simulation let say 1000 times to see if the ratio converges
simulation <- lapply(seq_len(1000),sim_country)
mean(sapply(simulation,"[[",3))
# We can therefore conclude, that the long-term ratio fo boys to girls converges to 1,
# which means, that the intent to have more boys then girls is not reached
# through the given modification.

#############################
#    Analyzing a dataset    #
#############################

# Loading/Installing the needed library/ies
if (!require("ggplot2")){
  install.packages("ggplot2") 
  library(ggplot2)
} else library(ggplot2)
if (!require("data.table")){
  install.packages("data.table") 
  library(ggplot2)
} else library(ggplot2)
if (!require("rstudioapi")){
  install.packages("rstudioapi")
  library(rstudioapi)
} else library(rstudioapi) 
if (!require("reshape2")){ 
  install.packages("reshape2")
  library(reshape2)
} else library(reshape2) 
if (!require("GGally")){
  install.packages("GGally")
  library(GGally)
} else library(GGally)
if (!require("hexbin")){
  install.packages("hexbin")
  library(hexbin)
} else library(hexbin)

#Loading the data
wd <- getActiveDocumentContext()$path
wd <- gsub("/","//",wd)
wd <- gsub("//data_science_application.R","",wd)
setwd(wd)

# However, the names of the datasets on the internet suggest that the larger dataset is the testing one,
# I used that for training.
data_train <- data.frame(read.csv2("data_train.csv"))
colnames(data_train)[1] <- "age"
data_test <- data.frame(read.csv2("data_test.csv"))
colnames(data_test) <- colnames(data_train)

# Merging  test and training data to make sure, that the same modifications are made also on the test data.
data_all <- rbind(data_train,data_test)

#############################################
#    Data  exploration and visualization    #
#############################################

# Check the type of the columns
types <- sapply(data_all, class)
types

# We can notice, that there are many variables which are categorical. For them we will use dummies in the model. 
# (Thanks to the design of the R-s lm() function, if it recognizes that it has to deal with a factor variable,
# generates automatically dummies. More about that later.

# Let's deal with the NA-s. 

# Firstly, let check if there is an NA at all in the dataset
NAs <- c()
for(i in 1:ncol(data_all)){
  NAs <- c(NAs,any(is.na(data_all[,i])))
}
any(NAs)
# Ok, there are some NA. One possibility is to set the values of NA 0 if the variable is integer type and
# define a new factor, "No values", if the variable is a factor. Other possibility would be just don't take
# these rows into consideration.

# Cleaning the data
for(i in 1:ncol(data_all)){
  if(class(data_all[,i]) == "factor"){
      data_all[,i] <- as.character(data_all[,i]) 
      data_all[,i] <- gsub("\\.| |\\?| ?","",data_all[,i])
      
      data_all[,i][is.na(data_all[,i]) |
                  data_all[,i] == ""] <- "No values"
      
      data_all[,i] <- as.factor(data_all[,i])
  } else {
      data_all[,i] <- gsub("\\.| ","",data_all[,i])
      data_all[,i][is.na(data_all[,i]) | 
                   data_all[,i] == " ?" | 
                   data_all[,i] == "" |
                   data_all[,i] == " "] <- 0
      data_all[,i] <- as.numeric(data_all[,i])
  }
}

# Check the result
NAs <- c()
for(i in 1:ncol(data_all)){
  NAs <- c(NAs,any(is.na(data_all[,i])))
}
any(NAs)
# Ok, NAs are eliminated.

# However, the field of descriptive statistics is large, I restrict myself for this task
# to a correlation heatmap, boxplots and scatterplots of each (numerical*) variables.

data_all_num <- data_all[colnames(data_all) %in% names(types[types %in% c("integer","numeric")])]
data_all_num$Income <- data_all$Income

# *To make boxplots, qq-plots and scatterplots about categorical variables is not that straightforward.
# To visualize the categorical variables, we could use basically the same plots, but with dummies or changing them to numerical values.
# However, in a real use case I would consider to change them to numerical, but to show some plots, where every variable is shown, 
# (for example the ggpairs) is already computational expensive only with the numerical ones.
# Another problem would be that in some cases there are too many categories to show them on the plots. To solve that,
# data engineering could be utilized in order to merge some categories of a factor variable. However, these solutions are beyond the 
# scope of the required task.
# Therefore, I decided exclude them now for the summary plots, where every variable is plotted. 

# Correlation heatmap #
#Calculation of the correlation matrix
temp <- sapply(data_all,as.numeric)
corr_m <- round(cor(temp),2)
corr_m_melted <- melt(corr_m)
setorderv(corr_m_melted,"value")

#Check the variables with the largest and the smallest correlation [excluding the 1 and -1 correlations]
head(corr_m_melted[corr_m_melted$value != 1 & corr_m_melted$value != -1,])
tail(corr_m_melted[corr_m_melted$value != 1 & corr_m_melted$value != -1,])

# Visualize that with a correlation heatmap
heatmap <- ggplot(data = corr_m_melted, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
heatmap

# Scatterplots
ggpairs(data_all_num)

## From these two set of plots we can already tell a lot about the data. Some example conclusions based on the dataset:
#  - from the heatmap and from the correlation values, we can conclude without statistical tests (e.g. cor.test()) 
#    that there is no signifant correlation between the variables, which would harm the assumptions of the linear regression model.
#  - the scatterplots are useful to see the connection between the variables and their distribution.
#    Especially the connection between the given variable and the Income can be very interesting.
#    However, given the number of the variables, we can get really just a sense about our dataset.
#    For example, we can conclude that:
#       + the fnlwgt, capital gain and capital loss variables are very skewed,
#       + the most of the people work in 40 hours, so to include this variable to a model wouldn't help us a lot.


# Some other findings:
ggplot(data = data_all, aes(x = Income, y = age)) + 
    geom_boxplot() + theme_bw()
# - Not so surprisingly, the age and the income has positive correlation. The average age with income >50k
#   is higher then the one for <= 50k. It is also true for the lower and upper quantile. It explains also
#   that there are more outliers with higher age in the category <=50k.

plot(data_all$Income, data_all$sex)
# - Unfortunately, we can see from the spineplot that the ratio of the sexes in the category >50k is 
#   in favour to the males. However, the same can be said also from the people in category <=50k.
# Note: in the task was given to use ideally ggplot2, however, in ggplot2 there is no well implemented
# solution for spineplot, to visualize two categorical variables with each other.

ggplot(data_all, aes(x = hours.per.week,y = age)) + geom_hex(bins= 30) + 
  xlab("Working hours per week")
# - We can see that in all ages the 40 hours/week is the highest density. It is also observable from this graph,
#   that the extreme high working hours are not typical neither for the most youngest people, but rather not for 
#   the older one. The extreme working hours are most typical to the people in age between ~ 25 and 55.

########################################################
#      Building a linear model to predict whether      #
#           a person makes over 50K a year             #
########################################################

# To select variables for the model we could use backward selection, i.e. we start with a model with all variables
# and remove the variable with the smallest P-value (indicates the statisticaly significance of the variable).
# Then fit the model without this variable and so on until we reach a stopping rule.
# This method is implemented for both of the following models.

# For the linear regression, we have to set the response variable, the "Income" to numerical. 
# By default, R renames the variables by the order of the levels, so in our case,
# 1 denotes the category "<=50k", 2 the category ">50k" and 3 the category "No values".
data_all$Income_num <- as.numeric(data_all$Income)

# Seperating the modified data again to training and test dataset
data_test <- data_all[1:nrow(data_test),]
data_train <- data_all[(nrow(data_test)+1):nrow(data_all),]

#Defining the initial variables of the model
variables <- colnames(data_train)[colnames(data_train) != "Income_num" & colnames(data_train) != "Income"]
#Creating the initial model
fit <- summary(lm(as.formula(paste(c("Income_num ~ ", variables), collapse =" + ")), data_train))
fit

n <- length(fit$coefficients[,4])
#if not the intercept has the highest p value then take the one with the highest p-value, else the 2nd highest
max_p <- max(fit$coefficients[,4])
if(rownames(fit$coefficients)[fit$coefficients[,4] == max_p] == "(Intercept)"){
  max_p <- sort(fit$coefficients[,4],partial=n-1)[n-1]
}

# However the significance level normaly 0.05, I set a tenfold larger number as a limit, due to the categorical variables,i.e.
# for a categorical variable to be significant it is not necessary that all the categories are significant.
while(max_p > 0.5){
  # Find the variable with the largest p-value
  var_name <- rownames(fit$coefficients)[fit$coefficients[,4] == max_p]
  # Get rid of that variable. Given, that there are several categorical variables 
  for(i in 1:nchar(var_name)){
    var_name_temp <- substr(var_name,1,i)
    if(any(grepl(var_name_temp,variables))){
      var_name_needed <- var_name_temp
    }
  }
  variables <- variables[variables != var_name_needed]
  
  fit <- summary(lm(as.formula(paste(c("Income_num ~ ", variables), collapse =" + ")), data_train))
  n <- length(fit$coefficients[,4])
  #if not the intercept has the highest p value then take the one with the highest p-value, else the 2nd highest
  max_p <- max(fit$coefficients[,4])
  if(rownames(fit$coefficients)[fit$coefficients[,4] == max_p] == "(Intercept)"){
    max_p <- sort(fit$coefficients[,4],partial=n-1)[n-1]
  }
} 
# Fitting the model with the selected variables
fit_lin <- lm(as.formula(paste(c("Income_num ~ ", variables), collapse =" + ")), data_train)
summary(fit_lin)
# Making the prediction
prediction <- predict(fit_lin,data_test)
# Given that this is a classification problem, the predictions >= 1 is classified as class 2 (>50k)
# and the predictions < 1 as class 1(<=50k)
prediction[prediction >= 1] <- 2
prediction[prediction < 1] <- 1
data_test$Income_pred <- prediction

# Evaluating the result by the so called confusion matrix.
conf_m <- as.matrix(table(Actual = data_test$Income_num, Predicted = data_test$Income_pred))
conf_m

n = sum(conf_m) # number of instances
n_classes = nrow(conf_m) # number of classes
corrects = diag(conf_m) # number of correctly classified instances per class 
rowsums = apply(conf_m, 1, sum) # number of instances per class
colsums = apply(conf_m, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy = sum(corrects) / n 
accuracy ## which is quite bad

precision = corrects / colsums 
recall = corrects / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

#################################################################################################################################
#!! Note:  By looking at, for example, the adjusted R-squared of the model or the measurements calculated above,              !!#
#!! it is obvious that the performance of the model is very weak.                                                             !!#
#!! Of course these results could be improved by using some data engineering techniques to manipulate the data, or            !!#
#!! by using polynomial,log and interaction terms in the models and                                                           !!#
#!! by using cross-validation to tune the parameters and coefficients further.                                                !!#
#!! However, these techniques are beyond the scope of this task.                                                              !!#
#################################################################################################################################

###################################################
#   Building an another model type to predict     # 
#     whether a person makes over 50K a year      #
###################################################
# Given, that our response variable can have two values, i.e. under 50k/above 50k,
# we can consider this problem as a classification problem. Therefore, as another model
# we could use logistic regression or discriminant analysis.
# For this task, I choosed the logistic regression, which yields us the probability of being the  predicted
# response variable the one from the given category.

#Defining the initial variables of the model
variables <- colnames(data_train)[colnames(data_train) != "Income" & colnames(data_train) != "Income_num"]
#Creating the initial model
fit <- summary(glm(as.formula(paste(c("Income ~ ", variables), collapse =" + ")), data_train, family = binomial))
fit

n <- length(fit$coefficients[,4])
#if not the intercept has the highest p value then take the one with the highest p-value, else the 2nd highest
max_p <- max(fit$coefficients[,4])
if(rownames(fit$coefficients)[fit$coefficients[,4] == max_p] == "(Intercept)"){
  max_p <- sort(fit$coefficients[,4],partial=n-1)[n-1]
}

# However the significance level normaly 0.05, I set a tenfold larger number as a limit, due to the categorical variables,i.e.
# for a categorical variable to be significant it is not necessary that all the categories are significant.
while(max_p > 0.5){ 
  # Find the variable with the largest p-value
  var_name <- rownames(fit$coefficients)[fit$coefficients[,4] == max_p]
  # Get rid of that variable. Given, that there are several categorical variables 
  for(i in 1:nchar(var_name)){
    var_name_temp <- substr(var_name,1,i)
    if(any(grepl(var_name_temp,variables))){
      var_name_needed <- var_name_temp
    }
  }
  variables <- variables[variables != var_name_needed]
  
  fit <- summary(glm(as.formula(paste(c("Income ~ ", variables), collapse =" + ")), data_train, family = binomial))
  n <- length(fit$coefficients[,4])
  #if not the intercept has the highest p value then take the one with the highest p-value, else the 2nd highest
  max_p <- max(fit$coefficients[,4])
  if(rownames(fit$coefficients)[fit$coefficients[,4] == max_p] == "(Intercept)"){
    max_p <- sort(fit$coefficients[,4],partial=n-1)[n-1]
  }
} 

fit_logistic <- glm(as.formula(paste(c("Income ~ ", variables), collapse =" + ")), data_train, family = binomial)
summary(fit_logistic)

# Making the prediction
prediction <- predict.glm(fit_logistic,data_test, type = "response")
# Given that this is a classification problem, the predictions let's say >= 0.5 is classified as class 2 (>50k)
# and the predictions < 0.5 as class 1(<=50k)
prediction[prediction >= 0.5] <- 2
prediction[prediction < 0.5] <- 1
data_test$Income_pred_logis <- prediction

# Evaluating the result by the so called confusion matrix.
conf_m <- as.matrix(table(Actual = data_test$Income_num, Predicted = data_test$Income_pred_logis))
conf_m

n = sum(conf_m) # number of instances
n_class = nrow(conf_m) # number of classes
corrects = diag(conf_m) # number of correctly classified examples (per class)
rowsums = apply(conf_m, 1, sum) # number of instances per class
colsums = apply(conf_m, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy = sum(corrects) / n 
accuracy # which is way better then for the linear regression.

precision = corrects / colsums 
recall = corrects / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 
# The other measurements suggest also that the logistic regression would be a better choice for this task.

#################################################################################################################################
#!! Of course this model could be improved by using some data engineering techniques to manipulate the data,                  !!#
#!! by using for example other threshold to decide whether the result belongs to the given class  and                         !!#
#!! by using cross validation to tune the parameters further.                                                                 !!#
#!! However, these techniques are beyond the scope of this task.                                                              !!#
#################################################################################################################################

##########################################################################################################################
# I hope, I could demonstrate for you my skills and convince you, that my knowledge is perfectly suitable for the        #
# position.                                                                                                              #
#               Best Regards,                                                                                            #
#                 Laszlo Marton                                                                                          #
##########################################################################################################################