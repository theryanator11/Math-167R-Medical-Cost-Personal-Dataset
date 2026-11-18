
#' We need to install packages (Only run if not done yet),
#' and also import the libraries first:
library(ggplot2)


#Check the working directory
this_wd <- getwd()
this_wd


#' Read insurance.csv from the project folder
#' WARNING: You need to ensure insurance.csv is in your working directory
insur <- read.csv(
  "insurance.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)


# change the smoker column into a boolean vector
smoker_boolean_vector <- insur$smoker == "yes"
smoker_boolean_vector


insur$smoker <- smoker_boolean_vector


# view the head of the new dataframe i just edited
head(insur)


#divide the dataframe by subcategories
smokers <- insur[insur$smoker,]
head(smokers)


nonsmokers <- insur[insur$smoker,]
head(nonsmokers)


males <- insur[insur$sex == "male",]
head(males)


females <- insur[insur$sex == "female",]
head(females)




# compare males and females bmi and medical costs
ggplot(data = insur,
       aes(x = bmi,
           y = charges,
           color = sex)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("female" = "pink", "male" = "blue"))




# compare males and females age and medical costs
ggplot(data = insur,
       aes(x = age,
           y = charges,
           color = sex)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("female" = "pink", "male" = "blue"))




# look at medical costs by filtering based on number of children
# I want to save a new graph for every number of children there are
# I am still figuring this part out. So don’t run it yet
for (i in 1:length(insur$age)){
 
  ggplot(data = insur,
         aes(x = age,
             y = charges,
             color = sex)) +
    geom_point(size = 1) +
    scale_color_manual(values = c("female" = "pink", "male" = "blue"))
 
  plot <- paste0("Age vs Costs for children amount", i)
  ggsave(plot)
}


insurance <- insurance_1
insurancesex<-as.factor(insurancesex)
insurancesmoker<-as.factor(insurancesmoker)
insuranceregion<-as.factor(insuranceregion)
model_1 <- lm(log(charges) ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(model_1)

par(mfrow = c(2, 2))
plot(model_1)

model_2 <- lm(log(charges) ~ age + sex + bmi ~ smoker + children + region, data = insurance)
par(mfrow = c(2, 2))
plot(model_2)

anova(model_1, model_2)
AIC(model_1, model_2)

library(car)
vif(model_1)


model_3 <- lm(log(charges) ~ smoker * age + bmi + children + region + sex, data = insurance)
par(mfrow = c(2, 2))
plot(model_3)

anova(model_1, model_3)
AIC(model_1, model_3)


plot(model_1, which = 1, col = insurance$smoker)

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

predictions <- predict(
  model_1,
  newdata = testing_data,
  interval = "confidence",
  level = 0.95
)


non_smokers <- insurance[insurance$smoker == "no",]
smokers <- insurance[insurance$smoker == "yes",]


non_smokers <- insurance[insurance$smoker == "no",]
smokers <- insurance[insurance$smoker == "yes",]


par(mfrow = c(2, 2))
plot(insurancesex,insurancecharges)
plot(non_smokerssex,nonsmokerscharges)
plot(insurancesmoker,insurancecharges)
plot(insuranceregion,insurancecharges)


plot(insuranceage,insurancecharges, col = insurance$smoker) #r-squared code for the non-smoker part?
plot(insurancebmi,insurancecharges, col = insurance$smoker)
boxplot(non_smokersnonsmokerscharges)
boxplot(smokerscharges~smokerschildren, data = smokers)

library("ggplot2")
insur <- ggplot(data = non_smokers, aes(x = bmi, y = charges, color = age))
insur + geom_point(size = 3) +
  labs(
    title = "bmi vs cost",
    x = "bmi",
    y = "charges",
    color = "age",
  )

test <- lm(age ~ charges, data = non_smokers)
summary(test)
test2 <- lm(test$charges ~ smoker, data = insurance)
test3 <- lm(bmi ~ charges, data = non_smokers)
summary(test3)
test4 <- lm(charges ~ smoker * bmi, data = insurance)
