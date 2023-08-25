# Reset workspace
rm(list = ls())

######### 1. LOAD DATASET #############
poverty <- read.csv("C:/School/PovertyProbability.csv")
nrow(poverty)


######### 2. CLEAN DATASET ##############
#rows with na values
sum(apply(is.na(poverty), 1, any))

# Remove rows with NA values
poverty <- na.omit(poverty)

#check one more time for na values
sum(apply(is.na(poverty), 1, any))
nrow(poverty)

#convert literacy to be a factor
is.factor(poverty$literacy)
poverty$literacy <- as.factor(poverty$literacy)


########## 3. SPLIT INTO TESTING AND TRAINING SETS ##########
# Set a seed for reproducibility
set.seed(123)

# Generate random indices for the training and testing sets
train_indices <- sample(nrow(poverty), 0.7 * nrow(poverty))  # 70% for training
test_indices <- setdiff(1:nrow(poverty), train_indices) # 30% testing
head(train_indices)

# Create training and testing sets using indices
train_data <- poverty[train_indices, ]
test_data <- poverty[test_indices, ]

# check size of testing and training datasets
nrow(train_data)
nrow(test_data)


########### 4. TRAIN MODEL TO PREDICT LITERACY ############
glm.literacy <- glm(literacy ~ education_level + age + is_urban + employment_category_last_year
, data = train_data, family = "binomial")


########### 5. PREDICT LITERACY USING MODEL ##############
glm.literacy.predict <- predict(glm.literacy, test_data, type = 'response')

#print first few results
head(glm.literacy.predict,3)

########### 6. CHECK MODEL PERFORMANCE ###############
#convert predicted values back to True/False
test_data$predict.literacy <- ifelse(glm.literacy.predict >= .5, "True","False")

#convert string "true" and "false" values to booleans
test_data$predict.literacy <- as.logical(test_data$predict.literacy)

head(test_data$predict.literacy,5)
head(test_data$literacy,5)

#Determine accuracy of model
accuracy <- mean(test_data$predict.literacy == test_data$literacy)
print(accuracy)
