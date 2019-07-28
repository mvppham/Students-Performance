##### Students Performance #####

## Import library
library(readr)
library(tidyverse)

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

# I do not consider the variable lunch as I do not really understand it.

# Does the test preparation course help to achieve a higher score?

ggplot(df, aes(x = `test preparation course`, y = totalScore, fill = gender)) +
  geom_boxplot() +
  labs(title = "Total Score by completing the Test Preparation Course or not", x = "Test Preparation Score")

# It does help both for female and male students.