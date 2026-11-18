#assigning the data to insurance which is easier to remember and to write
insurance <- insurance_1_
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
