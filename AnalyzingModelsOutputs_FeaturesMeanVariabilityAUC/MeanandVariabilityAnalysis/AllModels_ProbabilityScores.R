# How many unique models were selected by elastic net in 200 iterations
library(pROC)
library(writexl)
library(dplyr)
library(tidyr)
library(readxl)
install.packages("here")
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
#Participant_ID <- list()
for (num in 1:200){
  Model_Output = read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = num)
  All_ProbabilityScore[[paste0("Model_", num)]] = data.frame("Participant_ID" = Model_Output$ID,
                                                             "Model_Scores" = Model_Output$Exp_Model_ProbScore)
  #Participant_ID[[paste0("Model_", num)]] = Model_Output$ID
}

# Step 1: Get sorted unique ID order from first model
sorted_ids <- All_ProbabilityScore[[1]] %>% arrange(Participant_ID) %>% pull(Participant_ID)
Comp_zscore_ID_OhioLabel <- Model_Output %>% arrange(ID) %>% select(ID, Comp_zscore, Ohio_Label)

# Step 2: Create an empty list to store each model's sorted scores
Model_OhioData_matrix_list <- lapply(names(All_ProbabilityScore), function(model_name) {
  df <- All_ProbabilityScore[[model_name]] %>%
    arrange(Participant_ID) %>%
    select(Participant_ID, Model_Scores)
  return(df)
})

# Step 3: Verify the ID match in each model and convert list to data.frame
Match = c()
for (Model_iteration in 1: length(All_ProbabilityScore)){
  Match[Model_iteration] = sum(ID_score_matrix_list[[Model_iteration]]$Participant_ID == sorted_ids)
}
sum(Match == 223)

#Step 4: Convert list to data.frame with Models as rows and Participant ID as columns
score_matrix <- lapply(Model_OhioData_matrix_list, function(df){
 df$Model_Scores
})
score_matrix <- do.call(rbind, score_matrix)
colnames(score_matrix) <- paste("ID_", sorted_ids, sep = '')
rownames(score_matrix) <- names(All_ProbabilityScore)

# Step 5: Convert to tibble and add model names as column
score_dataframe <- as.data.frame(score_matrix)
score_dataframe <- tibble::rownames_to_column(score_dataframe, var = "Model")
CompZScore_row <- c("CompZScore", as.numeric(Comp_zscore_ID_OhioLabel$Comp_zscore))
OhioLabel_row <- c("Ohio_Label", as.numeric(Comp_zscore_ID_OhioLabel$Ohio_Label))

# Step 6: Bind Comp z score to the existing probability score tibble
score_dataframe_final <- rbind(score_dataframe, CompZScore_row, OhioLabel_row)

# Step 7: Save as an Excel file
write_xlsx(score_dataframe_final, "EachModel_ProbabilityScores_forAllParticipants.xlsx")
