# How many unique models were selected by elastic net in 200 iterations
library(pROC)
library(writexl)
library(readxl)
install.packages("here")
library(here)

# This function finds the pathway for all the files within the directory inside new R code 
find_file <- function(filename) {
  path <- list.files(path = here::here(), 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])}

## Part I: Convert Features and Coefficients as another list
# Logic: list of all features and coeff, checking how many unique models from there, then for each model, coeff and features as another column for 200. 
All_ModelFeatures <- list()
# Model[i] = paste("Model_", i, sep = "")
for (num in 1:200){
  Model_Output = read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = num)
  Model_Features = read_excel(find_file("FeatureandCoefficients_200iterations.xlsx"), sheet = num)
  All_ModelFeatures[[paste0("Model_", num)]] = data.frame(
    Feature = Model_Features$Features, Coefficient = Model_Features$Coefficients)
}

# Get the models in the format so as to check how many of the models are exact match with one another 
Model_Signatures <- lapply(All_ModelFeatures, function(Models) {
  Models <- Models[order(Models$Feature), ]  # sort alphabetically by feature
  paste(paste(Models$Feature, Models$Coefficient, sep = "="), collapse = ",")
})

# Checking uniques and duplicates
Model_Signatures_vec <- unlist(Model_Signatures) # get them all as a character vector
length(unique(Model_Signatures_vec)) # total different models
which(duplicated(Model_Signatures_vec)) ## check which models are exact duplicates. 
unique_signatures = unique(Model_Signatures_vec)

# Create a data frame with Model Names and their Signatures
Model_Signatures_df <- data.frame(
  Model = names(Model_Signatures),
  Signature = as.character(Model_Signatures),
  stringsAsFactors = FALSE
)
write_xlsx(as.data.frame(Model_Signatures_df), "All_Models_Info_without_Rounding.xlsx")
