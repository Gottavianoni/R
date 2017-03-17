#Kaggle
ppl <- read.csv("C:/Users/ottavig/Desktop/kaggle/Redhat/people.csv")
train <- read.csv("C:/Users/ottavig/Desktop/kaggle/Redhat/act_train.csv")
test <- read.csv("C:/Users/ottavig/Desktop/kaggle/Redhat/act_test.csv")
sub <- read.csv("C:/Users/ottavig/Desktop/kaggle/Redhat/sample_submission.csv")


for (i in 10:37) ppl[,paste0("char_",i)] <- ifelse(ppl[,paste0("char_",i)] == "True",1,0)
for (i in 10:37) ppl[,paste0("char_",i)] <- as.factor(ppl[,paste0("char_",i)])
ppl[,41] <- as.numeric(ppl[,41])
ind <- which(regexpr("char",names(ppl)) !=-1 )
names(ppl)[ind] <- paste0("char_ppl_",1:length(ind))
ind <- which(names(ppl) == "date")
names(ppl)[ind] <- "date_ppl"

train_other_typ <- train[train$activity_category != "type 1",]
train_typ1 <- train[train$activity_category == "type 1",]

##TYPE 1
train_typ1 <- train_typ1[,-14]
train_typ1 <- merge(train_typ1,ppl,by.x = "people_id",by.y = "people_id", all.x=T)

#MODELISATION
ind <- which(regexpr("char",names(train_typ1))!=-1)
train_t <- train_typ1[,c(names(train_typ1)[ind],"date","date_ppl","outcome")]
ind <- sample(length(train_t[,1]),10000)
train_sub <- train_t[-ind,]
train_t <- train_t[ind,]

attach(train_t)
glm1 <- glm(data = train_t, formula = outcome ~.,family = binomial(link = "logit") )
backwards <- step(glm1)

library(e1071)
svm1 <- svm(formula =  outcome ~. ,data = train_t,cost = 1000,gamma =1)

library(nnet)
ir.nn2 <- nnet(outcome ~ ., data = train_t, size = 2000, rang = 0.1,
               decay = 5e-4, maxit = 200)


ind <- sample(length(train_sub[,1]),10000)

x <- predict(glm1,train_sub)
x <- predict(svm1,train_sub)
x <- predict(ir.nn2,train_sub[ind,])

res <- NULL
ind <- sample(length(train_sub[,1]),50000)
x <- predict(ir.nn2,train_sub[ind,])
res <- data.frame(cbind(outcome = train_sub[ind,"outcome"],res_out = x))
res$V2 <- ifelse(res$V2 < 0.5,0,1)

conf1 <- length(which(res$V2 == 1 & res$outcome == 1))
conf2 <- length(which(res$V2 == 0 & res$outcome == 1))
conf3 <- length(which(res$V2 == 1 & res$outcome == 0))
conf4 <- length(which(res$V2 == 0 & res$outcome == 0))

conf <- cbind(c(conf4,conf2)/length(res$V2),c(conf3,conf1)/length(res$V2))

summary(glm1)

##############
train_other_typ <- train[train$activity_category != "type 1",]
