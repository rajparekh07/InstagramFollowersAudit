
test <- read.csv("InstagramChecker/test.csv")
train <- read.csv(("InstagramChecker/train.csv"))

dim(test)
dim(train)

summary(train)

colnames(test)
colnames(train)

table(train['fake'])
barplot(table(train['fake']))

barplot(table(train['profile.pic']))

barplot(table(train['profile.pic']), main  = "Profile Picture")

barplot(table(train['external.URL']), main  = "External URL in profile")

barplot(table(train['private']), main  = "Private Account")
   
barplot(table(train['name..username']), main  = "Name and Username are same")

install.packages("vcd")
library(vcd)

install.packages("ggplot2")
library(ggplot2)

plot(density(table(train["X.posts"])))

O18 = table(train["X.followers"])

O18.density <- density(O18)
hist(O18, breaks=40, probability=TRUE)
lines(O18.density)
rug(O18)


O18 = table(train["X.posts"])

O18.density <- density(O18)
hist(O18, breaks=40, probability=TRUE)
lines(O18.density)
rug(O18)





O18 = table(train["X.follows"])

O18.density <- density(O18)
hist(O18, breaks=40, probability=TRUE)
lines(O18.density)
rug(O18)

O18 = table(train["description.length"])

O18.density <- density(O18)
hist(O18, breaks=40, probability=TRUE)
lines(O18.density)
rug(O18)


#specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
#condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
#value <- abs(rnorm(12 , 0 , 15))
#data <- data.frame(table(train["profile.pic"]), table(train["fake"]))

# Stacked
#ggplot(data, aes(fill=condition, y=value, x=specie)) + geom_bar(position="stack", stat="identity")

profile_pic <- c(rep("profile pic" , 2) , rep("no profile pic" , 2) )
condition <- rep(c("fake" , "real") , 2)
value <- c(nrow(train[train$profile.pic != "0"& train$fake != "0",]), nrow(train[train$profile.pic != "0"& train$fake != "1",]), nrow(train[train$profile.pic != "1"& train$fake != "0",]), nrow(train[train$profile.pic != "1"& train$fake != "1",]))
data <- data.frame(profile_pic,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=profile_pic)) + 
  geom_bar(position="fill", stat="identity")


ExternalURL <- c(rep("external link" , 2) , rep("no external link" , 2) )
condition <- rep(c("fake" , "real") , 2)
value <- c(nrow(train[train$external.URL != "0"& train$fake != "0",]), nrow(train[train$external.URL != "0"& train$fake != "1",]), nrow(train[train$external.URL != "1"& train$fake != "0",]), nrow(train[train$external.URL != "1"& train$fake != "1",]))
data <- data.frame(ExternalURL,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=ExternalURL)) + 
  geom_bar(position="fill", stat="identity")

private <- c(rep("private" , 2) , rep("public" , 2) )
condition <- rep(c("fake" , "real") , 2)
value <- c(nrow(train[train$private != "0"& train$fake != "0",]), nrow(train[train$private != "0"& train$fake != "1",]), nrow(train[train$private != "1"& train$fake != "0",]), nrow(train[train$private != "1"& train$fake != "1",]))
data <- data.frame(private,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=private)) + 
  geom_bar(position="fill", stat="identity")


nameisusername <- c(rep("name==username" , 2) , rep("name!=username" , 2) )
condition <- rep(c("fake" , "real") , 2)
value <- c(nrow(train[train$name..username != "0"& train$fake != "0",]), nrow(train[train$name..username != "0"& train$fake != "1",]), nrow(train[train$name..username != "1"& train$fake != "0",]), nrow(train[train$name..username != "1"& train$fake != "1",]))
data <- data.frame(nameisusername,condition,value)

# Stacked
ggplot(data, aes(fill=condition, y=value, x=nameisusername)) + 
  geom_bar(position="fill", stat="identity")


install.packages("caret")
install.packages("e1071")
install.packages("rpart.plot")
library(caret)
library(rpart.plot)
library(e1071)

# Save the file.
train$fake = as.factor(train$fake)
intrain <- createDataPartition(y = train$fake, p= 0.7, list = FALSE)
training <- train[intrain,]
testing <- train[-intrain,]
dim(training); dim(testing);
anyNA(train)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(fake  ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
predict(dtree_fit, newdata = testing[1,])
testing[1,]
predict(dtree_fit, newdata = testing[1,])

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(factor(test_pred, levels=0:1), factor(testing$fake, levels=0:1) )  #check accuracy

