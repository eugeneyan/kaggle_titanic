### import necessary packages
library(data.table)
library(dplyr)

# read training dataset
train <- read.csv('train.csv')
setDT(train)
# str(train)
# View(train)

# read test dataset
test <- read.csv('test.csv')
setDT(test)

# exploring the data
table(train$Survived)
prop.table(table(train$Survived))

# everyone dies prediction
test$Survived <- 0
# save as csv
submit <- data.table(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "version1.csv", row.names = FALSE)

### gender-class model
summary(train$Sex)
prop.table(table(train$Sex, train$Survived), margin = 1)

# update model such that females survive
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.table(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "version2.csv", row.names = FALSE)

# examining age
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
View(train)

# using aggregate
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

# using dplyr
train %>% 
    group_by(Child, Sex) %>%
    summarize(n = n(), counts = sum(Survived)) %>%
    mutate(prob = counts/n)

# examining fare
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

train %>% 
    group_by(Fare2, Pclass, Sex) %>%
    summarize(n = n(), counts = sum(Survived)) %>%
    mutate(prob = counts/n)

# main effect of fare
train %>%
    group_by(Fare2) %>%
    summarize(n = n(), counts = sum(Survived)) %>%
    mutate(prob = counts/n)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] <- 0
submit <- data.table(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "version3.csv", row.names = FALSE)

### Decision Trees
install.packages('C50')
install.packages('randomForest')
library(rpart)
model.DT <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = 'class')
plot(model.DT)
text(model.DT)

# for better graphics
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(model.DT)

prediction <- predict(model.DT, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'myfirstdtree.csv', row.names = F)

?rpart.control
model.DT2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = 'class', control = rpart.control(minsplit = 2, cp = 0))  # this leads to overfitting
fancyRpartPlot(model.DT2)

# manually trim trees
model.DT3 <- prp(model.DT2, snip = T)$obj

### Feature Engineering
str(train)
str(test)
test$Survived <- NA

# combine training and testing data
combi <- rbind_list(train, test)  # coerces factors to char 
str(combi)

combi$Name[1]

# regular expressions
strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]

strsplit(combi$Name[1], split = '[,.]')[[1]][2]

# sapply data cleaning to all Titles
combi$Title <- sapply(combi$Name, FUN = function(x) {
  strsplit(x, split = '[,.]')[[1]][2]
})
str(combi)  # but there's still a ' ' in front of all the titles

# using dplyr (doesn't work correctly)
combi$Title <- NA
str(combi)
combi <- combi %>%
  mutate(Title = strsplit(Name, split = '[,.]')[[1]][2])  # doesn't work correctly
View(combi)

# removing the ' '
combi$Title <- sub(' ', '', combi$Title)
View(combi$Title)
str(combi)
table(combi$Title)

# combining titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

# using dplyr
combi <- combi %>%
  mutate(Title = ifelse(Title == 'Mme', 'Mlle', Title))

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Jonkheer', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
table(combi$Title)

# change title back to factor
combi$Title <- as.factor(combi$Title)
str(combi)

# derive FamilySize
combi <- combi %>%
  mutate(FamilySize = SibSp + Parch + 1)

# combine surname with familysize
combi$Surname <- sapply(combi$Name, FUN = function(x) {
  strsplit(x, split = '[,.]') [[1]][1]
})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = '')
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# split training and test set
train <- combi[1:891, ]
test <- combi[892:1309, ]

# create new decision tree
model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                 Embarked + Title + FamilySize + FamilyID, 
               data = train, method = 'class')
fancyRpartPlot(model)

# create submission
prediction <- predict(model, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'myseconddtree.csv', row.names = F)

# create new decision tree (with fewer models)
model <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title, 
               data = train, method = 'class')
fancyRpartPlot(model)

# create submission
prediction <- predict(model, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'mythirddtree.csv', row.names = F)

### Random Forests
# for random forests, we have to fill in the missing values
# it can only take in factors up to 32 levels
install.packages('rondomForest')
library(randomForest)

# filling in Age
summary(combi$Age)
age.avail <- combi[!is.na(combi$Age), ]
View(combi[!is.na(combi$Age), ])
model.age <- rpart(Age ~ Pclass + Sex + Fare + Title, 
               data = age.avail, method = 'anova')

combi$Age[is.na(combi$Age)] <- predict(model.age, combi[is.na(combi$Age), ])

summary(combi)

# filling in Embarked
i <- which(combi$Embarked == '')
combi$Embarked[i] <- 'S'
combi$Embarked <- factor(combi$Embarked)
summary(combi$Embarked)

# filling in Fare
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm = T)
summary(combi$Fare)

# adjusting the FamilyID
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
combi$Survived <- factor(combi$Survived)
str(combi)

# split training and test set
train <- combi[1:891, ]
test <- combi[892:1309, ]

set.seed(251186)
model2 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title, 
               data = train, importance = T, ntree = 2000)
model3 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + 
                         Parch + Fare + Embarked + Title + FamilySize +
                         FamilyID2, data = train, importance = T, ntree = 2000)

# check importance of variables
varImpPlot(model2)
varImpPlot(model3)

# create submission
prediction <- predict(model2, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'rforest1.csv', row.names = F)

prediction <- predict(model3, test, type = 'class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'rforest2.csv', row.names = F)

# forest of conditional inference trees
install.packages('party')
library(party)

set.seed(415)
model4 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title, 
           data = train, controls = cforest_unbiased(ntree=2000, mtry=3))
prediction <- predict(model4, test, OOB = T, type = 'response')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'cforest1.csv', row.names = F)

model5 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                    Fare + Embarked + Title + FamilySize + FamilyID, 
                  data = train, controls = cforest_unbiased(ntree = 2000, 
                                                            mtry = 3))
prediction <- predict(model5, test, OOB = T, type = 'response')
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = 'cforest3.csv', row.names = F)
