# Trabajo de Data Science Dojo, tutorial de youtube
# https://bit.ly/3dKwURV


setwd("C:/Users/admin/Desktop/Kaggle Titanic")

titanic.train <- read.csv(file = "train.csv", 
                          stringsAsFactors = FALSE, header = TRUE)

titanic.test <- read.csv(file = "test.csv", 
                          stringsAsFactors = FALSE, header = TRUE)
# Clean the data

## Crea una nueva variable para los 2 tablas

titanic.train$IsTraineSet <- TRUE
titanic.test$IsTraineSet <- FALSE

## Crea una variable que no posee la tabla titanic.train

titanic.test$Survived <- NA

# Ahora combinas las dos tablas en una sola

titanic.full <- rbind(titanic.train, titanic.test)

# Eliminamos algunos valores vacios

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

# Seguimos eliminando NA values y vacios

# limpiamos missing values de la variable Age

age.median <- median(titanic.full$Age, na.rm = TRUE)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median


# limpiamos missing values de la variable Fare

fare.median <- median(titanic.full$Fare, na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

# Categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)




# volvemos a picar la tabla en los dos tablas originales 
#  pero limpias
titanic.train <- titanic.full[titanic.full$IsTraineSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTraineSet==FALSE,]


titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)



titanic.model <- randomForest(formula = survived.formula,
                              data = titanic.train, 
                              ntree = 500, mtry = 3, 
                              nodesize = 0.01 * 
                                nrow(titanic.test))

features.equation <- "Pclass + Sex + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)


PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived



write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)
