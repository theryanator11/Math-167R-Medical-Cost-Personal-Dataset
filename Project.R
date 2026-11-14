
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
