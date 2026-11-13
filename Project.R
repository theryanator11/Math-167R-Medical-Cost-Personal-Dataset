#' We need to install packages (Only run if not done yet), 
#' and also import the libraries first:
install.packages("ggplot2")
library(ggplot2)

# Read insurance.csv from the project folder
insurance_df <- read.csv(
  "insurance.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Display the data frame
print(insurance_df)

# Optional: open it in a spreadsheet-style viewer
View(insurance_df)
theDirectory <- getwd()
theDirectory

#Create a Dataframe to do the analysis and make graphs with
insurance_df <- read.csv("insurance.csv")
head(insurance_df)

