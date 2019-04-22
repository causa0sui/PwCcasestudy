rm(list=ls())
setwd("C:/Users/BS/Desktop/SLM/Projekt SLM/22.04")
source('Gini function.R')

df <- read.csv("final_data.csv", sep = ',')

variables <- data.frame(variable = names(df)[2:89], used = FALSE)
df <- na.omit(df)

smp_size <- floor(0.8 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

bestGini <- 0
iter <- 0
startTime <- Sys.time()
while(difftime(currTime,startTime,units="secs")<600){
  
  changedRow <- sample(nrow(variables),
                       sample(1:2,1))
  variables[changedRow,2] <- !variables[changedRow,2]
  
  formula <- paste("DefFlag ~ ", 
                   paste(variables[variables$used==TRUE,1], collapse = " + ")
  )
  model <- glm(formula = formula, data = train, family = binomial())
  pred <- predict(model, type = 'response', newdata = test)
  
  newGini <- Gini_value(pred, test$DefFlag)
  if(newGini > bestGini & 
     sum(variables$used)!=0 |
     runif(1) < 0.05){
    bestGini <- newGini
  }else{
    variables[changedRow,2] <- !variables[changedRow,2]
  }
  currTime <- Sys.time()
  iter = iter + 1
}
bestGini
formula
