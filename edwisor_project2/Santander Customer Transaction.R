# train.csv - the training set.
# test.csv - the test set. The test set contains some rows which are not included in scoring
# removing list of objects previously saved
rm(list=ls(all=T))
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
#getting current working directory
getwd()
setwd("C:/Users/jagadeesh/Desktop/Santander Customer Transaction")
# Normalizing train and test data
down_test1=read.csv("downtest.csv")
down_test1$Class->tar
downscaledtest<-down_test1[-c(1,2)]
downscaledtest$Class<-0
downscaledtest<-as.data.frame(scale(downscaledtest))
for (i in 1:length(rownames(downscaledtest)))
{
  downscaledtest$Class[i]<-tar[i]
}
write.csv(downscaled1,"downscaled1.csv")
write.csv(downscaledtest,"downtest.csv")
customer_train=read.csv("train.csv")
customer_test=read.csv("test.csv")
# reading top values
nrow(customer_train)
str(customer_train)
ncol(customer_train)
summary(customer_train)
colnames(customer_train)
summary(customer_test)
#Analysis of Training data
# Checking for Missing values by creating a dataframe 
df=data.frame(apply(customer_train,2,function(x){sum(is.na(x))}))
tmp<-df
rownames(tmp)->tmp1
tmp2<-tmp$values
#Creating a dataframe containing number of missing values
tmp<-data.frame(tmp1,tmp2)
head(tmp)
missing_values<-sum(is.na(customer_train))
missing_values
#Calculating percentage ofmissing values in the r
percent<-data.frame(apply(customer_train,2,function(x){sum(is.na(x))})/nrow(customer_train))
rownames(percent)->percent1
percent$Percent->percent2
percent[order(-percent$Percent),]->percent1
data.frame(percent1,percent2)->percent
head(percent)
percent[order(-percent$percent2),]->percent
head(percent)
#Visualizing target variable using pie plot
par(mfrow=c(1,2))
pie(customer_train$target,labels=as.character(customer_train$target),
    main="Target Set Target Distribution",
    col=c("red","orange","yellow","blue","green"),
    border="brown",
    clockwise=TRUE
)
sample1<-customer_train$target
# plotting a histogram
barplot(sample2)
factor(sample1)->sample2
table(sample2)->sample2
install.packages("tidyverse")
library(tidyr)
#set the defulttheme to theme minimal
theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)
#plotting graphs for variables from var1 to var50
sample1<-customer_train[,c(3:5)]
library(dplyr)
tmp2<- subset(customer_train, target == 0)
tmp1<- subset(customer_train, target == 1)
col.names<-colnames(tmp2)
col.names<- col.names[2:53]
par(mfrow=c(3, 3))
length(col.names)
for (i in 2:length(col.names)) {
  hist(tmp2[,i], main=col.names[i], probability=TRUE, col="gray", border="white")
  d <- density(tmp1[,i])
  lines(d, col="red")
}
#plotting graphs separately for each variable
sample1.gathered<-sample1 %>% as.data.frame() %>% gather(key="variable",value="value")
ggplot(sample1.gathered,aes(value)) + geom_density() + facet_wrap(~variable)

#checking for unique values 
for (i in 2:length(col.names)){
  print(paste(paste(col.names[i-1],":"),length(unique(customer_train[,i]))))
  #print(length(unique(customer_train[,i])))
}
#Correlation graph
library(corrplot)
library(RColorBrewer)
cust<-customer_train[-c(1)]
correlation<-cor(cust)
head(correlation)
corrplot(correlation, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
require("corrplot")
corrgram(cust, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Outlier analysis
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(customer_train,is.numeric) #selecting only numeric
numeric_data = customer_train[,numeric_index]
numeric_data1<-numeric_data[-c(1)]
colnames(numeric_data1)
cnames = colnames(numeric_data)
cnames<-cnames[-c(1)]
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = subset(customer_train))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="target")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

# Finding Outliers

for(i in cnames){
  print(i)
  val = customer_train[,i][customer_train[,i] %in% boxplot.stats(customer_train[,i])$out]
  customer_train1 = customer_train[which(!customer_train[,i] %in% val),]
}

customer2<-customer_train[which(customer_train %in% customer_train[,1])]
customer3<-customer_train[which(!customer_train %in% customer_train[,1])]

tmp2<- nrow(subset(customer3, target == 0))
tmp3<- nrow(subset(customer3, target == 1))


customer_train1<-customer_train1[-c(1)]

#Principal Component Analysis
scaled_df[,-17]->scale_df
head(scaled_df,1)
scaled_df<-t(scaled_df)
pca<-prcomp(scaled_df,scale=FALSE)
pca$center[1:5]
summary(pca)
names(pca)
#training data and test data dividing
training_data_size <- floor(0.75 * nrow(numeric_data))
training_data_size
#index numbers
set.seed(142)
train_index <- sample(1:nrow(numeric_data),training_data_size)
#Training data
train_data <- numeric_data[train_index,]
head(train_data)
# Test data
test_data <- numeric_data[-train_index,]
test_data<-read.csv("downtest.csv")
library(dplyr)
# Logistic Regression
str(train_data)
train_data$target<-factor(train_data$target,levels=c(1,0))
table(train_data$target)
# Down Sampling
set.seed(100)
down_train <- downSample(x = train_data[,colnames(numeric_data1)],y=train_data$target)

up_train<- upSample(x = customer3[,colnames(customer_train1)],y=customer3$target)
library(magrittr)
table(down_train$Class)
colnames(down_train)
head(down_train)
colnames(down_train)
# Logistic regression model
logitmodel<-glm(Class~., family=binomial(link='logit'), data=down_train)
summary(logitmodel)
pred <- predict(logitmodel, newdata = test_data, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test_data$target
mean(y_pred==y_act)
table(test_data$target,y_act)
# PR and ROC curves

require(PRROC)
fg <- pred[test_data$target == 1]
bg <- pred[test_data$target == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(pr)

# Finding output using  test data
library(ROCR)
p<-predict(logitmodel,newdata = customer_test, type = "response")
fitted.results<-ifelse(p>0.5,1,0)
head(p)
pr<-prediction(p,customer_test$target)
numeric_data1$target=0
for (i in 1:length(rownames(customer_test)))
{
  customer_test$target[i]=fitted.results[i]
}

# Model 2
#K nearest Neighbours

#Implementing kNN
library(class)
# Dividing
train_target<-downscaled1$Class
test_target<-downscaledtest$Class
m1<- knn(downscaled1,downscaledtest,cl=train_target,k = 3)

# Calculate Accuracy 
table(test_target,m1)

fg <- m1[down_test1$target == 1]
bg <- m1[down_test1$target == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(pr)

# Decision Trees

install.packages("rpart")
library(rpart)
library(rpart.plot)
dt<-rpart(Class~.,data=downscaled1)
rpart.plot(dt, type = 3,extra = 1, tweak=1.1)
downscaledtest1<-downscaledtest[,-200]
# Prediction 
predict(dt,downscaledtest1)-> predicted_species_train
ifelse(predicted_species_train>0.5,1,0)->predicted_species_train
table(downscaledtest$Class,predicted_species_train)

fg <- dt[downscaledtest$Class == 1]
bg <- dt[downscaledtest$Class == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = bg, scores.class1 = fg, curve = T)
plot(pr)

# BayesianOptimization using R
# train options
kfolds = 3L
early_stopping_rounds = 10L
iterations = 100L
num_threads = 8
learning_rate = 0.02
init_points = 5L      
n_iter = 2L  
data=downscaled
# params
params = list(
  max_depth = c(3L, 9L),
  subsample = c(0.6, 1),
  colsample_bytree = c(0.6, 1),
  num_leaves = c(15L, 100L) 
)
start = Sys.time()
best_params2 <- rBayesianOptimization::BayesianOptimization(
  FUN = function(...){bayesTuneLGB(data=data, k=kfolds, ...)},
  bounds = params, 
  init_points = init_points, 
  n_iter = n_iter,
  acq = "ucb", 
  kappa = 2.576, 
  eps = 0.0, 
  verbose = TRUE
)
Sys.time() - start