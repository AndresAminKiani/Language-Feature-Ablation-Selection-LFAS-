# How many unique models were selected by elastic net in 200 iterations
library(pROC)
library(writexl)
library(readxl)
#install.packages("here")
library(here)

find_file <- function(filename) {
  path <- list.files(path = here::here(), 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])
}
## Convert and probability score as another list
# Logic: each time order the data in the ascending order by id and give the probability score and id for each. if id match, put as the next column.
All_ProbabilityScore <- list()
for (num in 1:200){
  Model_Output = read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = num)
  All_ProbabilityScore[[paste0("Model_", num)]] = Model_Output$Exp_Model_ProbScore
}

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
unique_signatures = unique(Model_Signatures_ve)

# # Rounding features to 10 and organizing each model as a row 
# Model_Signatures_Rounding <- lapply(All_ModelFeatures, function(df) {
#   df <- df[order(df$Feature), ]
#   df$Coefficient <- round(df$Coefficient, 10)  
#   paste(paste(df$Feature, df$Coefficient, sep = "="), collapse = "|")
# })

# Check how many of the models are exact match with one another if we round them up to 10 decimals.
Model_Signatures_vec_round <- unlist(Model_Signatures_Rounding)
length(unique(Model_Signatures_vec_round))
unique_signatures_after_Rounding_to_10 <- unique(Model_Signatures_vec_round)

# Track the rounded model and how many times each of the unique model occurred. 
Model_Iteration = list()
Model_Occurence = c()
for (index in 1:length(unique_signatures)){
  Model_Iteration[[index]] = as.numeric(which(Model_Signatures_vec_round == unique_signatures_after_Rounding_to_10[index]))
  Model_Occurence[index] = length(which(Model_Signatures_vec_round == unique_signatures_after_Rounding_to_10[index]))
}


library(tibble)

# Convert the models into usable formats for R and save them in excel
All_Models_Info_Rounding <- tibble::tibble(
  "All_Unique_Models_Roundings_10" = unique_signatures_after_Rounding_to_10,
  "Model_Iteration" = Model_Iteration,
  "Repeated_Number_ofTimes" = Model_Occurence) 

library(tidyr)
All_Models_Info_Rounding_flattened <- unnest(All_Models_Info_Rounding, cols = c("Model_Iteration"))  
#write_xlsx(All_Models_Info_Rounding_flattened, "All_Models_Info_Rounding.xlsx")

