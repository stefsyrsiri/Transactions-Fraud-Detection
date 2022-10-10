library(data.table)  # read the data
library(dplyr)       # data management / manipulation
library(rpart)       # decision trees
library(rpart.plot)  # decision tree plotting



######## READ & PREPARE THE DATA ###############################################
data <- fread("R/online payments fraud.csv")  # read the dataset
data <- as_tibble(data); data  # view of the data and its structure

data$type <- factor(data$type) # from chr to factor
# from int to factor with strings as level names
data$isFraud <- factor(ifelse(data$isFraud==1, "Fraud", "Not a Fraud"))
data

data %>% select(- c(step, isFlaggedFraud)) %>% summary()  # descriptive analysis

####### CREATE A TRAIN & A TEST DATASET ########################################
# sample size n=500, because the occurance is rare (below 5%)
sample <- sample(3000, 2000, replace = FALSE) 

# training data is 500 random observations from the dataset
data_train <- data[sample,]

# testing data is the rest of the observations but the 500 we randomly picked
data_test <- data[- sample, ]  



###### LEARNING PHASE (TRAINING OF THE MODEL) using the train dataset ##########
tree <- rpart(isFraud~type+amount+oldbalanceOrg+newbalanceOrig+
                      oldbalanceDest+newbalanceDest,
                    method = "class", data = data_train, cp=0.01, minsplit=2)
tree

# plotting of the decision tree
rpart.plot(tree, extra=101, cex=0.6, box.palette="YlGnBl",
           main="Is the transaction a fraud?")
rpart.rules(tree)  # rules of tree



###### PREDICTION TESTING using the test dataset ###############################
prediction <- predict(tree, data_test, type = "class")
con_matrix <- table(data_test$isFraud, prediction); con_matrix
con_matrix_prop <- round(100*prop.table(con_matrix, 1), 1); con_matrix_prop

round(prop.table(table(data$isFraud)) * 100, digits = 1)
