install.packages("rpart")
install.packages("tree") #bulit in package in R
install.packages("naivebayes")

library(naivebayes)
library(rpart)
library(tree)
library(rpart)
A=data.frame(iris)
psych::pairs.panels(A[,1:4])
sf=sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd=A[sf == 1,]
tsd=A[sf == 2,]

model_tree = tree(Species ~ .,data = trd)
model_nb = naive_bayes(Species ~ .,data = trd)
model_rpart = rpart(Species ~ .,data = trd)
plot(model_tree)
text(model_tree)


plot(model_nb)

pred_tree =predict(model_rpart,tsd)
pred_nb =predict(model_nb,tsd)

pred_t = ifelse(pred_tree[,1]>0.5,"setosa",ifelse(pred_tree[,2]>0.5,"versicolor","virginica"))

cf_tree = table(pred_t,tsd$Species)
cf_nb = table(pred_nb,tsd$Species)

#################################################################################################
#rpart - database car90 -Reliabilty &tranm -Classifiation






