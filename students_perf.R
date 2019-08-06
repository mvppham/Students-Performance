##### Students Performance #####

## Import library
library(readr)
library(tidyverse)
library(caTools)
library(randomForest)

## Open dataset
StudentsPerformance <- read_csv("~/StudentsPerformance.csv")

### Data Exploration

## First look
View(StudentsPerformance)
df <- StudentsPerformance
head(df)
summary(df)

# There are 1000 entries of students with information on gender, ethnicity, parental level of education, lunch,
# test preparation, math score, reading score writing score.

# Let's check for any missing values

df[!complete.cases(df),]

# No missing values.

table(df$gender)

# 518 students are female, 482 are male.

ggplot(data = df, aes(x = df$`race/ethnicity`)) +
  geom_bar()

table(df$`race/ethnicity`)

# Five different race groups, denoted by A to E. Group C is the biggest group containing 319 students.

table(df$`parental level of education`)

# Five different parental education level: Associate's degree, Bachelor's degree, high school, Master's degree,
# some college and some high school. "Some college" and Associate's degree have the most occurences with 226
# and 222.

table(df$lunch)

# The variable 'lunch' has two characteristics: standard lunch and free/reduced lunch. Most students (645) have 
# standard lunch.

table(df$`test preparation course`)

# Binary variable. 642 students did not do the test preparation course.

ggplot(data = df, aes(x = factor(0), df$`math score`)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = NULL) +
  xlab(NULL)

ggplot(data = df, aes(x = factor(0), df$`reading score`)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = NULL) +
  xlab(NULL)

ggplot(data = df, aes(x = factor(0), df$`writing score`)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = NULL) +
  xlab(NULL)

# Scores for each test are centered around 65-70.

# Creating a total score variable.

df$totalScore <- round((df$`math score` + df$`reading score` + df$`writing score`)/3, digits = 2)

# Let's see if there is a difference between gender.

ggplot(data = df, aes(x = gender, y = df$`math score`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Math Score by Gender", y = "Math Score", x = "Gender")

ggplot(data = df, aes(x = gender, y = df$`reading score`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Reading Score by Gender", y = "Reading Score", x = "Gender")

ggplot(data = df, aes(x = gender, y = df$`writing score`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Writing Score by Gender", y = "Writing Score", x = "Gender")

ggplot(data = df, aes(x = gender, y = totalScore, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Score by Gender", y = "Total Score", x = "Gender")

# While male students are better at math in general, female students beat them at reading and writing. Female
# students are overall better.

# Difference between ethnicities?

ggplot(df, aes(x = `race/ethnicity`, y = `math score`, fill = `race/ethnicity`)) +
  geom_boxplot() +
  labs(title = "Math Score by Ethnicity", x = "Ethnicity", y = "Math Score")

ggplot(df, aes(x = `race/ethnicity`, y = `reading score`, fill = `race/ethnicity`)) +
  geom_boxplot() +
  labs(title = "Reading Score by Ethnicity", x = "Ethnicity", y = "Reading Score")

ggplot(df, aes(x = `race/ethnicity`, y = `writing score`, fill = `race/ethnicity`)) +
  geom_boxplot() +
  labs(title = "Writing Score by Ethnicity", x = "Ethnicity", y = "Writing Score")

ggplot(df, aes(x = `race/ethnicity`, y = totalScore, fill = `race/ethnicity`)) +
  geom_boxplot() +
  labs(title = "Total Score by Ethnicity", x = "Ethnicity", y = "Total Score")

# From worst to best in each test: A < B < C < D < E

# What about parental education level?

ggplot(df, aes(x = `parental level of education`, y = `math score`, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Math Score by Parental Level of Education", x = "Parental Education Level", y = "Math Score")

ggplot(df, aes(x = `parental level of education`, y = `reading score`, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Reading Score by Parental Level of Education", x = "Parental Education Level", y = "Reading Score")

ggplot(df, aes(x = `parental level of education`, y = `writing score`, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Writing Score by Parental Level of Education", x = "Parental Education Level", y = "Writing Score")

ggplot(df, aes(x = `parental level of education`, y = totalScore, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Total Score by Parental Level of Education", x = "Parental Education Level", y = "Total Score")

# It seems like there is an intergenerational transmission of educational performance. Students whose parents have a
# master's degree tend to have higher scores than the rest. On the contrary, students whose parents only have
# high school degree are the worst. This is the case for every test.

# Lunch?

ggplot(df, aes(x = lunch, y = totalScore, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Score by lunch", x = "Lunch")

# Students having standard lunch perform better in general.

# Does the test preparation course help to achieve a higher score?

ggplot(df, aes(x = `test preparation course`, y = totalScore, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Score by completing the Test Preparation Course or not", x = "Test Preparation")

# It does help both for female and male students.


### Classification models

# Can we find a classification model that is able to predict whether the student is male oder female given
# the other variables?

# Creating binary variables

df$gender0 <- factor(df$gender, levels = c("male", "female"), labels = c(0, 1)) 
df$ethnicity0 <- factor(df$`race/ethnicity`, levels = c("group A", "group B", "group C", "group D", "group E"),
                        labels = c(1, 2, 3, 4, 5))
df$parentEd0 <- factor(df$`parental level of education`, 
                       levels = c("some high school", "high school", "some college", "associate's degree",
                                  "bachelor's degree", "master's degree"), 
                       labels = c(1, 2, 3, 4, 5, 6))
df$lunch0 <- factor(df$lunch, levels = c("free/reduced", "standard"), labels = c(0, 1))
df$testPrep0 <- factor(df$`test preparation course`, levels = c("none", "completed"), labels = c(0, 1))

# Creating training and test set

set.seed(123)
split <- sample.split(df$gender0, SplitRatio = 0.75)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

training_set <- training_set[, -c(1:5, 9)]
test_set <- test_set[, -c(1:5, 9)]

# Fitting Logistic Regression to the training set

classifier <- glm(formula = gender0 ~ parentEd0 + lunch0 + testPrep0 + `math score` + `reading score`
                  + `writing score`,
                  family = binomial,
                  data = training_set)

summary(classifier)

# Parental Education and Writing Score are not significant. But removing them from the model would give us
# a worse one (Residual deviance and AIC higher)

# Predicting test set results

prob_pred <- predict(classifier, type = "response", newdata = test_set[, -4])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Making Confusion Matrix

cm <- table(test_set[, 4], y_pred)

# The gender of 219 (out of 250) students are predicted properly.
# Accuracy: (107+112)/250 = 0.876
# Sensitivity/Recall: 112/(112+18) = 0.862
# Specificity: 107/(107+13) = 0.892
# Precision: 112/(112+13) = 0.896
# Model looks solid.


# Can we also predict if a student did the test preparation?

classifier2 <- glm(formula = testPrep0 ~ ., family = binomial, data = training_set)
summary(classifier2)

# All variables are significant.

# Predicting test set results

prob_pred2 <- predict(classifier2, type = "response", newdata = test_set[, -8])
y_pred2 <- ifelse(prob_pred2 > 0.5, 1, 0)

# Confusion Matrix

cm2 <- table(unlist(test_set[, 8]), y_pred2)

# Accuracy is only 0.736 this time. In addition to that: If we used a model that only predicts "0", the accuracy
# would be 0.64. So our model is not much better than that.

# Let's try a nonlinear classification model. Random Forest.

classifier3 <- randomForest(x = training_set[, -8], y = training_set$testPrep0, ntree = 10, importance = TRUE)

# Predicting test set results

y_pred3 <- predict(classifier3, newdata = test_set[, -8])

# Confusion Matrix

cm3 <- table(unlist(test_set[, 8]), y_pred3)

# CM looks worse than the one for the logistic regression.

importance(classifier3)

# Looking at the Mean Decrease Accuracy and the Mean Decrease Gini, we can try a different Random Forest model
# without Ethnicity, parental Education and lunch.

training_set2 <- training_set[, -c(5,6,7)]
test_set2 <- test_set[, -c(5,6,7)]

classifier4 <- randomForest(x = training_set2[, -5], y = training_set2$testPrep0, ntree = 10)

y_pred4 <- predict(classifier4, newdata = test_set2[, -5])

cm4 <- table(unlist(test_set2[, 5]), y_pred4)

# Model accuracy does not improve.
