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
model3 = glm(death ~ age + bun, train, family = binomial)
summary(model3)
pre.data <- test %>% select(age, bun)
pre = ifelse(predict(model3, pre.data) > 0, "Death", "Survive") 
tab = table(test$death, pre)
tab
1-(sum(diag(tab))/sum(tab))
####Hierarchical clustering
##R code below is written just for clarify (ex, including "id" variable etc)
df_d = dist(df)
round(df_d, 2)
sngl = hclust(df_d, "single")
sngl$merge
plot(sngl)
####K-means
set.seed(1234)
mx = as.matrix(df)
result = kmeans(mx, 3)
result
result$withinss
result$tot.withinss
result$totss
result$betweenss
install.packages("maptools")
library(maptools)
plot(mx, col = result$cluster)
points(result$centers, col = 1:5, pch = 8)
install.packages("cluster")
library(cluster)
result = clusGap(df, kmeans, K.max = 10, B = 100, verbose = interactive())
result
plot(result)
result_i = kmeans(mx, 3, iter.max = 500)
install.packages("useful")
library(useful)
plot(result_i, data = mx)
w.dist = dist(mx)^2
sil = silhouette(result_i$cluster, w.dist)
plot(sil)
####EM algorithm
install.packages("mclust")
library(mclust)
mclust = Mclust(df)
summary(mclust)
mclust$BIC
plot(mclust)
#####Principal component analysis
df_p <- df %>% select(age, bun, gender)
R = cor(df_p)
E = eigen(R)
contribution=NULL
accumulate=0
i=0
for (i in 1:3){
  accumulate= E$values[i]/sum(E$values)+accumulate
  contribution[i] =　paste ("第",i,"主成分","固有値",E$values[i]
                           ,"寄与率",E$values[i]/sum(E$values)
                           ,"累積寄与率",accumulate)
}
contribution
plot(E$vectors[1:3, 1:2], xlab="First PC", ylab = "Second PC")
result = prcomp(df_p, scale = TRUE)
result$x
biplot(result)
