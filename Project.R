
############################################################
#Ryan Stone's Code Starts here:
############################################################

#' We need to install packages to read csv (Only run if not done yet), 
#' and also import the libraries first:
library(ggplot2)

#Check the working directory
this_wd <- getwd()
this_wd

#' Read insurance.csv from the project folder
#' WARNING: You need to ensure insurance.csv is in your working directory
insurance <- read.csv(
  "insurance.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# change the smoker column into a boolean vector
smoker_boolean_vector <- insurance$smoker == "yes"
smoker_boolean_vector

insurance$smoker <- smoker_boolean_vector

# view the head of the new dataframe i just edited
head(insurance)

#divide the dataframe by subcategories
smokers <- insurance[insurance$smoker,]
head(smokers)

nonsmokers <- insurance[insurance$smoker,]
head(nonsmokers)

males <- insurance[insurance$sex == "male",]
head(males)

females <- insurance[insurance$sex == "female",]
head(females)


# compare males and females bmi and medical costs
ggplot(data = insurance, 
       aes(x = bmi,
           y = charges,
           color = sex)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("female" = "pink", "male" = "blue"))


# compare males and females age and medical costs
ggplot(data = insurance, 
       aes(x = age,
           y = charges,
           color = sex)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("female" = "pink", "male" = "blue"))





############################################################
# Ryan Hannon-Hippe's Code Starts here:
############################################################

#changing them to factors to make a linear regression model
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

#create a standard model of all the variables to base the accuracy of the other models on
model_1 <- lm(log(charges) ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(model_1)
#when doing plot it gives 4 plots so having all 4 when loaded is ideal.
par(mfrow = c(2, 2))
plot(model_1)

#create a second model to see if the change (bmi smoker interaction) improves the model or not.
model_2 <- lm(log(charges) ~ age + sex + bmi * smoker + children + region, data = insurance)
par(mfrow = c(2, 2))
plot(model_2)

#anova(model_1, model_2)
#comparing the two models to see which one is more accurate
AIC(model_1, model_2)

#to see if the data is multicollinearity (if other variables inflates variance) 
library(car)
vif(model_1)

#creating a third model to see if a smoker age interaction improves the model more or not
model_3 <- lm(log(charges) ~  smoker * age + bmi + children + region + sex, data = insurance)
par(mfrow = c(2, 2))
plot(model_3)

#testing if it does
#anova(model_1, model_3)
AIC(model_1, model_3)

#plot seeing where smokers land because of their influence on the data.
plot(model_1, col = insurance$smoker)

#using different values to see what a predicted charge would be.
testing_data <- data.frame( 
  age = c(20, 40, 60),
  sex = factor(c("female", "male", "female"),
               levels = levels(insurance$sex)),
  bmi = c(23, 30, 20),
  children = c(0, 1, 3),
  smoker = factor(c("yes", "no", "no"),
                  levels = levels(insurance$smoker)),
  region = factor(c("southeast", "northwest", "northeast"),
                  levels = levels(insurance$region))
)
#using confidence interval so it is a range and not just one strict value. 
predictions <- predict(
  model_1,
  newdata = testing_data,
  interval = "confidence",
  level = 0.95
)

#separating smokers and non smokers to see if it changes anything in the data. 
non_smokers <- insurance[insurance$smoker == "no",]
smokers <- insurance[insurance$smoker == "yes",]

#plots of different data when the y-axis is charges
par(mfrow = c(2, 2))
plot(insurance$sex, insurance$charges, main = "Sex", ylab = "charges")
plot(non_smokers$sex, non_smokers$charges, main = "Sex of Non-Smokers", ylab = "charges")
plot(insurance$smoker, insurance$charges, main = "Does the Person Smoke", ylab = "charges")
plot(insurance$region, insurance$charges, main = "Region", ylab = "charges")

#more plots that highlights the smokers or data is split up by smokers and non-smokers
plot(insurance$age, insurance$charges, col = insurance$smoker, xlab = "age", ylab = "charges") 
plot(insurance$bmi, insurance$charges, col = insurance$smoker, xlab = "bmi", ylab = "charges")
boxplot(non_smokers$charges~non_smokers$children, data = non_smokers, main = "non smoker children", xlab = "# of children", ylab = "charges")
boxplot(smokers$charges~smokers$children, data = smokers, main = "smoker children", xlab = "children", ylab = "charges")


#testing the data, mainly looking at the r-squared value to see what all "explains" the data. 
test <- lm(charges ~ age, data = non_smokers)
summary(test)
test2 <- t.test(charges ~ smoker, data = insurance)
test3 <- lm(bmi ~ charges, data = non_smokers)
summary(test3)
test4 <- lm(charges ~ smoker * bmi, data = insurance)
summary(test4)
test5 <- lm(charges ~ smoker * age, data = insurance)
summary(test5)

#other plots
plot(test)
plot(test5)




############################################################
#Benjamin Wang's Code Starts here:
############################################################

# Turn smoker from "yes"/"no" into TRUE/FALSE
insurance$smoker <- insurance$smoker == "yes"

############################################################
# Descriptive statistics for the numeric variables
############################################################

# min, Q1, median, mean, Q3, max for each numeric variable
summary(insurance$age)
summary(insurance$bmi)
summary(insurance$children)
summary(insurance$charges)

# standard deviations
sd(insurance$age)
sd(insurance$bmi)
sd(insurance$children)
sd(insurance$charges)

# IQR and range of charges
IQR(insurance$charges)
max(insurance$charges) - min(insurance$charges)

# charges are skewed right: the mean is much bigger than the median
mean(insurance$charges)
median(insurance$charges)

# so we also look at log(charges)
insurance$log_charges <- log(insurance$charges)
summary(insurance$log_charges)
sd(insurance$log_charges)


############################################################
# Histograms of the numeric variables
############################################################

ggplot(data = insurance, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of age", x = "Age", y = "Count")

ggplot(data = insurance, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count")

ggplot(data = insurance, aes(x = charges)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "white") +
  labs(title = "Distribution of charges", x = "Charges (USD)", y = "Count")

ggplot(data = insurance, aes(x = log_charges)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of log(charges)", x = "log(charges)", y = "Count")


############################################################
# Frequency tables and bar charts for the categorical variables
############################################################

# counts
table(insurance$sex)
table(insurance$smoker)
table(insurance$region)
table(insurance$children)

# percentages
prop.table(table(insurance$sex)) * 100
prop.table(table(insurance$smoker)) * 100
prop.table(table(insurance$region)) * 100

ggplot(data = insurance, aes(x = sex)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Sex", y = "Count")

ggplot(data = insurance, aes(x = smoker)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Smoker", y = "Count")

ggplot(data = insurance, aes(x = region)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Region", y = "Count")

ggplot(data = insurance, aes(x = factor(children))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of children", x = "Children", y = "Count")


############################################################
# Charges compared across groups
############################################################

# mean charges in each group
tapply(insurance$charges, insurance$smoker, mean)
tapply(insurance$charges, insurance$sex, mean)
tapply(insurance$charges, insurance$region, mean)
tapply(insurance$charges, insurance$children, mean)

# median charges in each group
tapply(insurance$charges, insurance$smoker, median)
tapply(insurance$charges, insurance$sex, median)
tapply(insurance$charges, insurance$region, median)

# boxplots of charges by group
ggplot(data = insurance, aes(x = smoker, y = charges)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Charges by smoking status", x = "Smoker", y = "Charges (USD)")

ggplot(data = insurance, aes(x = sex, y = charges)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Charges by sex", x = "Sex", y = "Charges (USD)")

ggplot(data = insurance, aes(x = region, y = charges)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Charges by region", x = "Region", y = "Charges (USD)")


############################################################
# Correlations and scatterplots
############################################################

# correlation matrix of the numeric variables
cor(insurance[, c("age", "bmi", "children", "charges")])

# charges vs age, colored by smoking status
ggplot(data = insurance, aes(x = age, y = charges, color = smoker)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  labs(title = "Charges vs age", x = "Age", y = "Charges (USD)")

# charges vs BMI, colored by smoking status, with a line for each group
ggplot(data = insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  labs(title = "Charges vs BMI", x = "BMI", y = "Charges (USD)")


############################################################
# Chi-square test: is smoking related to sex?
############################################################

sex_smoker_table <- table(insurance$sex, insurance$smoker)
sex_smoker_table
chisq.test(sex_smoker_table)


############################################################
# Multiple linear regression on log(charges)
############################################################

# Model 1: all six variables
model1 <- lm(log_charges ~ age + sex + bmi + children + smoker + region,
             data = insurance)
summary(model1)

# Model 2: add smoker x BMI interaction (does BMI matter more for smokers?)
model2 <- lm(log_charges ~ age + sex + bmi + children + smoker + region +
               smoker:bmi,
             data = insurance)
summary(model2)

# Compare the two models (small p-value / lower AIC means model 2 is better)
anova(model1, model2)
AIC(model1)
AIC(model2)

# Check multicollinearity with VIF = 1 / (1 - R^2)
# (VIF above 5 or 10 would be a problem)
1 / (1 - summary(lm(age ~ bmi + children, data = insurance))$r.squared)
1 / (1 - summary(lm(bmi ~ age + children, data = insurance))$r.squared)
1 / (1 - summary(lm(children ~ age + bmi, data = insurance))$r.squared)

# Residual plots to check the assumptions
plot(model2, which = 1)   # residuals vs fitted
plot(model2, which = 2)   # normal Q-Q plot

# Because we used log(charges), turn each coefficient into a percent change:
# percent change = (exp(coefficient) - 1) * 100
(exp(coef(model2)) - 1) * 100

# 95% confidence intervals as percent changes
(exp(confint(model2)) - 1) * 100

# Effect of 1 more BMI unit for smokers = bmi coefficient + interaction coefficient
smoker_bmi <- coef(model2)["bmi"] + coef(model2)["bmi:smokerTRUE"]
(exp(smoker_bmi) - 1) * 100


############################################################
# How well does the model predict charges?
############################################################

# R-squared of model 2
summary(model2)$r.squared

# Split the data: 80% to fit the model, 20% to test it
set.seed(167)
train_rows <- sample(1:nrow(insurance), 0.8 * nrow(insurance))
train <- insurance[train_rows, ]
test <- insurance[-train_rows, ]

# Fit model 2 on the training data and predict the test data
train_model <- lm(log_charges ~ age + sex + bmi + children + smoker + region +
                    smoker:bmi,
                  data = train)
test_pred <- exp(predict(train_model, newdata = test))   # back to dollars

# Average size of the prediction errors in dollars
sqrt(mean((test$charges - test_pred)^2))   # RMSE
mean(abs(test$charges - test_pred))        # mean absolute error

# Predicted vs actual (points on the dashed line are perfect predictions)
ggplot(data = test, aes(x = charges, y = test_pred, color = smoker)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
  labs(title = "Predicted vs actual charges (test data)",
       x = "Actual charges", y = "Predicted charges")

# Example predictions for two made-up people (with 95% prediction intervals)
new_people <- data.frame(
  age = c(30, 50),
  sex = c("female", "male"),
  bmi = c(25, 35),
  children = c(1, 2),
  smoker = c(FALSE, TRUE),
  region = c("southeast", "southeast")
)
exp(predict(model2, newdata = new_people, interval = "prediction"))
new_people
