#' We need to install packages (Only run if not done yet), 
#' and also import the libraries first:
library(ggplot2)

#Check the working directory
this_wd <- getwd()
this_wd

# Read insurance.csv from the project folder
insurance_df <- read.csv(
  "insurance.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Display the data frame
View(insurance_df)

