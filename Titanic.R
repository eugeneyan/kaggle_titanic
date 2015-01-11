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

# using dplyr pipes
train %>% group_by(Child, Sex) %>%
    summarize(counts = sum(Survived))
