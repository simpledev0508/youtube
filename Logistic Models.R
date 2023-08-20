# Reset workspace
rm(list = ls())

#Read the dataset in loan.csv 
poverty <- read.csv("C:/School/MyDataSets/PovertyProbability.csv")
View(poverty)

#convert literacy to be a factor
is.factor(poverty$literacy)
poverty$literacy <- as.factor(poverty$literacy)

#Build model to predict literacy
glm.literacy <- glm(literacy ~ education_level + age + is_urban, data = poverty, family = "binomial")

#predict literacy using model
glm.literacy.predict <- predict(glm.literacy, poverty, type = 'response' )

#convert predicted values back to True/False
poverty$predict.literacy <- ifelse(glm.literacy.predict >= .5, "True","False")


#Determine accuracy of model
accuracy <- mean(head(poverty$predict.literacy,35) == head(poverty$literacy,35))
print(accuracy)







