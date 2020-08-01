###Machine learning basic###
library(tidyverse)
df <- read_csv("C:/Users/akihi/Downloads/lecture用/pneumocopd_analysis_complete.csv") 
df <- df %>% 
  mutate_all(.funs = ~{as.numeric(.)}) #Just for simplicity. Actually don't do this.
####SLR
df = na.omit(df)
result0 = lm(hospitalterm ~ age, data = df)
summary(result0)
lmpredict = predict(result0)
lmresiduals = residuals(result0)
plot(lmpredict, df$age)
hist(lmpredict)
hist(lmresiduals)
plot(result0)
cooks.distance(result0)
ck.d = data.frame(df$age, df$hospitalterm, cooks.distance(result0))
subset(ck.d, ck.d$cooks.distance.result>0.02)
lm.predict = predict(result0, interval = "prediction")
summary(lm.predict)
lm.confidence = predict(result0, interval = "confidence")
summary(lm.confidence)
####MLR
result1 = lm(hospitalterm ~ age + bun, data = df)
summary(result1)
install.packages("car")
library(car)
vif(result1)
install.packages("MASS")
library(MASS)
AIC = stepAIC(result1) #Pick up the model with lowest AIC
####GLM
result2 = glm(death ~ age + bun, family = binomial, data = df)
summary(result2)
-2 * logLik(result2)
anova(result2, test="Chisq")
exp(result2$coefficients)
dim(df)
set.seed(1234)
tr.num = sample(756, 500)
train = df[tr.num,]
test = df[-tr.num,]
dim(train)
dim(test)
result3 = glm(death ~ age + bun, train, family = binomial)
summary(result3)
pre = ifelse(predict(result3, test) == 1, "Death", "Survive")
tab = table(test$death, pre)
tab
