library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)

train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

full <- bind_rows(train, test)

str(full)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

print(table(full$Sex, full$Title))

rare_title <- c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

print(table(full$Sex, full$Title))

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# This variable appears to have a lot of missing values
print(full$Cabin[1:28])

# The first character is the deck. For example:
print(strsplit(full$Cabin[2], NULL)[[1]])

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Passengers 62 and 830 are missing Embarkment
print(full[c(62, 830), 'Embarked'])

print(cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]], '</b>and<b> $', full[c(62, 830), 'Fare'][[2]], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]], '</b>and<b>', full[c(62, 830), 'Pclass'][[2]], '</b>. So from where did they embark?')))

# Get rid of our missing passenger IDs
embark_fare <- full %>%
    filter(PassengerId != 62 & PassengerId != 830)

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
print(sum(is.na(full$Age)))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked', 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
print(sum(is.na(full$Age)))

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
print(table(full$Child, full$Survived))

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
print(table(full$Mother, full$Survived))

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
