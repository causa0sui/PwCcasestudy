rm(list=ls())
setwd("C:/Users/BS/Desktop/SLM/Projekt SLM/22.04")
source('Gini function.R')

df <- read.csv("final_data.csv", sep = ',')

variables <- data.frame(variable = names(df)[2:89], used = FALSE)
df <- na.omit(df)

smp_size <- floor(0.8 * nrow(df))

pool <- 1:nrow(variables)


train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

for (i in pool){
  
  variables[i,2] <- !variables[i,2]
 
  formula <- paste("DefFlag ~ ", 
                   paste(variables[variables$used==TRUE,1], collapse = " + "))
  model <- glm(formula = formula, data = train, family = binomial())
  pred <- predict(model, type = 'response', newdata = test)
  variables$score[i] <- Gini_value(pred, test$DefFlag)
  
  variables[i,2] <- !variables[i,2]
}
  
bestVariable <- which.max(variables$score)
variables[bestVariable,2] <- !variables[bestVariable,2]
pool <- c(1:(bestVariable-1),(bestVariable+1):nrow(variables))
variables[variables$used==TRUE,]
