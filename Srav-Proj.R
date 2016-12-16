library('caret')
#Visualizing 
setwd("E:/Data Science/670-Capstone/R Proj")
vis2015 <- read.table("2015F.txt", header = T, stringsAsFactors = F)
vis2014 <- read.table("2014F.txt", header = T, stringsAsFactors = F)
vis2013 <- read.table("2013.txt", header = T, stringsAsFactors = F)
vis2012 <- read.table("2012.txt", header = T, stringsAsFactors = F)



trainsub <- subset(vis2015, select=c("atmcond", "county","dayofweek","lightcond","accmon",
                                     "injury","acchr"))
trainsub1 <- subset(vis2014, select=c("atmcond", "county","dayofweek","lightcond","accmon",
                                     "injury","acchr"))
trainsub2 <- subset(vis2013, select=c("atmcond", "county","dayofweek","lightcond","accmon",
                                     "injury","acchr"))
trainsub3 <- subset(vis2012, select=c("atmcond", "county","dayofweek","lightcond","accmon",
                                     "injury","acchr"))
tx <- rbind(trainsub,trainsub1,trainsub2,trainsub3)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(tx))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(tx)), size = smp_size)
train <- tx[train_ind, ]
test <- tx[-train_ind, ]

train[,"injury"] <- as.character(train[,"injury"])
train[,"injury"][train[,"injury"] == 9] <- 0
train[,"injury"][train[,"injury"] > 0] <- 1
train$injury <- as.numeric(train$injury)

test[,"injury"] <- as.character(test[,"injury"])
test[,"injury"][test[,"injury"] == 9] <- 0
test[,"injury"][test[,"injury"] > 0] <- 1
test$injury <- as.numeric(test$injury)

cols <- colnames(train)
train[cols] <- lapply(train[cols], as.factor)
cols <- colnames(test)
test[cols] <- lapply(test[cols], as.factor)

model <- glm(injury ~ atmcond + county + dayofweek + lightcond + accmon + acchr,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$injury)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$injury)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Test data for a particular route
testroute <- read.csv("/Users/gowthambharath/Documents/Capstone/MA-Data/Testroute.csv")
routeprop <- predict(model, testroute, type="response") 
TotalProb_Fatal<-sum(routeprop)/length(routeprop)
TotalProb_Fatal

# library(rpart)
# # grow tree 
# Tfit <- rpart(injury ~ atmcond + county + dayofweek + lightcond + accmon + acchr,
#              method="class", data=train)
# 
# 
# printcp(Tfit) # display the results 
# text(Tfit, pretty = 0)
# plotcp(fit) # visualize cross-validation results 
# summary(fit) # detailed summary of splits
# 
# # plot tree 
# plot(fit, uniform=TRUE, 
#      main="Classification Tree for accidents")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)
# 
# # create attractive postscript plot of tree 
# post(fit, file = "c:/tree.ps", 
#      title = "Classification Tree for Kyphosis")
# # prune the tree 
# pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# 
# # plot the pruned tree 
# plot(pfit, uniform=TRUE, 
#      main="Pruned Classification Tree for Kyphosis")
# text(pfit, use.n=TRUE, all=TRUE, cex=.8)
# post(pfit, file = "c:/ptree.ps", 
#      title = "Pruned Classification Tree for Kyphosis")