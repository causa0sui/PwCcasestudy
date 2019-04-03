rm(list=ls())
source('C:\\Users\\melnyk.p\\Desktop\\studia\\2 semestr\\Statistical Learning Methods\\PwC case study\\Gini_function.R')


df <- read.csv("C:\\Users\\melnyk.p\\Desktop\\studia\\2 semestr\\Statistical Learning Methods\\PwC case study\\data.csv", sep = ',')
df$DefFlag <- factor(df$DefFlag)
df$Job_type_fulltime <- ifelse(df$Job_type=='Full-time', 1, 0) #zmienne 0/1 dla Job full-time

df <- na.omit(df)

df$income_ratio <- df$Monthly_Spendings/df$Monthly_Income #income i spendings sa skorelowane, wiec lepiej utworzyc zmienna stosunku wydatkow do tego, co zarabiaja, jesli jest >=1 to oznacza, ze nie maja hajsu wgl na splate kredytu

summary(df)

summary(df$DefFlag)



## 70% of the sample size
smp_size <- floor(0.8 * nrow(df))


## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]


#Step 1 - Univariate analysis

colname <- c("GEO_region","Age","Job_type","Home_status","Car_status","Household_children","Monthly_Income","Monthly_Spendings",
             "Credit_purpose","Credit_amount","Number_of_installments","NotionalValue_t0","NotionalValue_lag1",
             "NotionalValue_lag2","NotionalValue_lag3","NotionalValue_lag4","NotionalValue_lag5","NotionalValue_lag6",
             "NotionalValue_lag7","NotionalValue_lag8","NotionalValue_lag9","NotionalValue_lag10","NotionalValue_lag11",
             "NotionalValue_lag12","DPD_t0","DPD_lag1","DPD_lag2","DPD_lag3",
             "DPD_lag4","DPD_lag5","DPD_lag6","DPD_lag7","DPD_lag8",
             "DPD_lag9","DPD_lag10","DPD_lag11","DPD_lag12","NotionalOverdue_t0",
             "NotionalOverdue_lag1","NotionalOverdue_lag2","NotionalOverdue_lag3","NotionalOverdue_lag4","NotionalOverdue_lag5",
             "NotionalOverdue_lag6","NotionalOverdue_lag7","NotionalOverdue_lag8","NotionalOverdue_lag9","NotionalOverdue_lag10",
             "NotionalOverdue_lag11","NotionalOverdue_lag12")


#plot(df$Marital_status, df$DefFlag)
#plot(df$Car_status, df$DefFlag)
#plot(df$Household_children, df$DefFlag)
#plot(df$NotionalValue_t0, df$DefFlag)


m1 <- glm(DefFlag ~ GEO_region+Monthly_Income+Age+Job_type_fulltime+Home_status+Credit_amount+Number_of_installments+DPD_t0                +DPD_lag1               +DPD_lag2             +DPD_lag3 +             
          +DPD_lag7+DPD_lag9+DPD_lag10+DPD_lag12+income_ratio  , data = train, family = binomial())
summary(m1)
pred.m1 <- predict(m1, type = 'response', newdata = test)
test$score <- pred.m1
#test$score <- score
result <- test[,c("Application_ID", 'score', "DefFlag")]


m2 <- glm(DefFlag ~ Monthly_Income + NotionalValue_t0 + Credit_amount + GEO_region + Age + NotionalOverdue_t0+
            +DPD_lag1 + DPD_lag3 + Credit_purpose + DPD_lag2 +DPD_t0 + Job_type + Household_children + Number_of_installments+
            Home_status + Car_status, data = df, family = binomial())




m <- glm(DefFlag ~ GEO_region       +      Age        +            Job_type         +            
           Home_status     +      Car_status     +        Household_children +    Monthly_Income     +    Monthly_Spendings  +   
           Credit_purpose         +Credit_amount         +Number_of_installments + NotionalValue_t0     +NotionalValue_lag1  +  
           NotionalValue_lag2     +NotionalValue_lag3     +NotionalValue_lag4     +NotionalValue_lag5    +NotionalValue_lag6  +  
           NotionalValue_lag7     +NotionalValue_lag8     +NotionalValue_lag9     +NotionalValue_lag10    +NotionalValue_lag11  +   
           NotionalValue_lag12    +DPD_t0                +DPD_lag1               +DPD_lag2             +DPD_lag3 +             
           DPD_lag4               +DPD_lag5               +DPD_lag6               +DPD_lag7              +DPD_lag8 +             
           DPD_lag9               +DPD_lag10              +DPD_lag11              +DPD_lag12              +NotionalOverdue_t0+   
           NotionalOverdue_lag1   +NotionalOverdue_lag2   +NotionalOverdue_lag3   +NotionalOverdue_lag4   +NotionalOverdue_lag5+  
           NotionalOverdue_lag6   +NotionalOverdue_lag7   +NotionalOverdue_lag8   +NotionalOverdue_lag9   +NotionalOverdue_lag10 +
           NotionalOverdue_lag11  +NotionalOverdue_lag12 + income_ratio  , data = train, family = binomial())




summary(m)

score <- predict(m, type = 'response', newdata = test)


#randomForest

colnames(df)

str(train)

library(randomForest)
arf <- randomForest(DefFlag ~       GEO_region       +      Age        +            Job_type         +            
                    Home_status     +      Car_status     +        Household_children +    Monthly_Income     +       
                     Credit_purpose         +Credit_amount         +Number_of_installments + NotionalValue_t0        +DPD_t0                +DPD_lag1               +DPD_lag2             +DPD_lag3 +             
                           +NotionalOverdue_t0   
                     
                    , data = na.omit(test), importance = TRUE,
                    proximity = TRUE, do.trace = T, ntree = 100, keep.forest = TRUE)

varImpPlot(arf)
importance(arf)

#Bayesian Network

library(deal)
ksl <- test

ksl <- na.omit(ksl, 0)
ksl.nw <- network(ksl)
ksl.prior <- jointprior(ksl.nw)

ksl.nw <- learn(ksl.nw,ksl,ksl.prior, 2)$nw
result <-  heuristic(ksl.nw,ksl,ksl.prior,restart=1,degree=1,trace=TRUE)
thebest <- result$nw[[1]]
savenet(thebest, "ksl.net")
print(ksl.nw,condposterior=TRUE)

#Recursive Partitioning


library(corrgram)

corrgram(na.omit(test[,1:10]), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)



