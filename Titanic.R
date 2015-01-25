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
