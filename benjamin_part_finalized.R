# Load packages and data
library(ggplot2)

# A window opens: pick insurance.csv
insurance <- read.csv(file.choose())

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