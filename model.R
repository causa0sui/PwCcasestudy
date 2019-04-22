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
    bestFormula <- formula
  }else{
    variables[changedRow,2] <- !variables[changedRow,2]
  }
  currTime <- Sys.time()
  iter = iter + 1
}
bestGini
bestFormula

#DefFlag ~  GEO_region + Job_type + Marital_status + Car_status + Household_children + Monthly_Income + Credit_amount + Agricultural_tractor_number + Building_project_subbmission_number + Employed_number_women + Employed_agricultural_number + Employed_industry_number + Emplyed_trade_transport_number + Employed_other_number + Total_population_age_0_14_years + Total_population_age_15_29_years + Spending_food + Spending_clothing + Spending_catering + Unemployed_total + Unemployed_highschool_and_lower_number + NotionalValue_lag1 + NotionalValue_lag2 + NotionalValue_lag5 + NotionalValue_lag8 + NotionalValue_lag12 + DPD_t0 + DPD_lag1 + DPD_lag2 + DPD_lag3 + DPD_lag4 + DPD_lag6 + DPD_lag7 + DPD_lag10 + NotionalOverdue_lag3 + NotionalOverdue_lag6 + NotionalOverdue_lag7 + NotionalOverdue_lag10
#Najlepsze do tej pory, okolo 0.45
