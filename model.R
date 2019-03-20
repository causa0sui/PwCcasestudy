rm(list=ls())

dir <- getwd()

df <- read.csv("C:\\Users\\melnyk.p\\Desktop\\studia\\2 semestr\\Statistical Learning Methods\\PwC case study\\data.csv", sep = ',')
df$DefFlag <- factor(df$DefFlag)

head(df)

nrow(subset(df, df$DefFlag>0))

colnames(df)

var <- c('Home_status', 'Age', 'Credit_amount')



summary(m)

confint(m)

## 70% of the sample size
smp_size <- floor(0.8 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

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
           NotionalOverdue_lag11  +NotionalOverdue_lag12   , data = train, family = binomial())

score <- predict(m, type = 'response', newdata = test)


score[1:5]

m.res <- ifelse(score > 0.5, "Def", "NoDef")

head(m.res)

sum(m.res=='Def')

colnames(test)

test <- cbind(test, score)

result <- test[,c('Application_ID', 'score', 'DefFlag')]
head(result)

plot(head(result))

ftopk <- function(x, top = 3){
  res = names(x)[order(x, descreasing = TRUE)][1:top]
  paste(res, collapse = ';', sep = '')
}

topk <- apply(g, 1, ftopk, top=3)
test <- cbind(test, topk)

#randomForest

colnames(df)

str(train)

library(randomForest)
arf <- randomForest(DefFlag ~       GEO_region       +      Age        +            Job_type         +            
                    Home_status     +      Car_status     +        Household_children +    Monthly_Income     +    Monthly_Spendings  +   
                     Credit_purpose         +Credit_amount         +Number_of_installments + NotionalValue_t0     +NotionalValue_lag1  +  
                 NotionalValue_lag2     +NotionalValue_lag3     +NotionalValue_lag4     +NotionalValue_lag5    +NotionalValue_lag6  +  
                     NotionalValue_lag7     +NotionalValue_lag8     +NotionalValue_lag9     +NotionalValue_lag10    +NotionalValue_lag11  +   
                    NotionalValue_lag12    +DPD_t0                +DPD_lag1               +DPD_lag2             +DPD_lag3 +             
                    DPD_lag4               +DPD_lag5               +DPD_lag6               +DPD_lag7              +DPD_lag8 +             
                     DPD_lag9               +DPD_lag10              +DPD_lag11              +DPD_lag12              +NotionalOverdue_t0+   
                     NotionalOverdue_lag1   +NotionalOverdue_lag2   +NotionalOverdue_lag3   +NotionalOverdue_lag4   +NotionalOverdue_lag5+  
                    NotionalOverdue_lag6   +NotionalOverdue_lag7   +NotionalOverdue_lag8   +NotionalOverdue_lag9   +NotionalOverdue_lag10 +
                     NotionalOverdue_lag11  +NotionalOverdue_lag12        
                    , data = na.omit(test), importance = TRUE,
                    proximity = TRUE, ntree = 1000, keep.forest = TRUE)
varImpPlot(arf)

summary(test)
colnames(df)

rcorr(as.matrix(df, type = 'pearson'))

library(corrgram)

corrgram(na.omit(test), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)



cols.num <- c("a","b")
DF[cols.num] <- sapply(DF[cols.num],as.numeric)
sapply(DF, class)



