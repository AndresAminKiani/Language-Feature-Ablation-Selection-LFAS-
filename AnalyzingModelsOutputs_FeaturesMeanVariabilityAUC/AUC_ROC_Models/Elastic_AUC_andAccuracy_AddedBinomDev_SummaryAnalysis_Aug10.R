# Load data and packages
library(easypackages)
libraries("readxl", "ggplot2", "ggalt", "openxlsx", "writexl", "rio", 
          "tidyverse", "pROC")
#Cont_CVE_record <- read_excel("MSE_record_Continuous_200iterations.xlsx");

# Which model has the smallest Cross-validation error
#Best_Training_ModelNum <- which(Cont_CVE_record$cvm == min(Cont_CVE_record$cvm))
##________________________________________________________________________________

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

## Saving the model with the least cross-validation error

# Ohio_data <<- 
#   read_excel(find_file("Master9_Mtch_117x2_v13_with PSs_1-1-18_Corrected634ID_zscore.xlsx"))
# Experimental_Measures <- 
#   data.frame(Ohio_data[,36:68], Ohio_data[,70:76],
#              Ohio_data[,78:85], Ohio_data[,88:94],
#              Ohio_data[,96:107], Ohio_data[,109: 112])
# Missing_Mat <- which(is.na(Experimental_Measures), arr.ind = T)

AUC_All200Models <- c();
for (i in 1:200){
## Let's pull out the model that performed the best for the training set
BestTrainingModel_Train <- 
  read_excel(find_file("ModelsOutputTrain_200iterations_Aug18.xlsx"), sheet = i);

## ROC on the best and worst models
AUC_All200Models[i] <- auc(roc(BestTrainingModel_Train$Ohio_Label, 
                     BestTrainingModel_Train$Exp_Model_ProbScore)) }
paste(which(AUC_All200Models == max(AUC_All200Models))[1], "is the best model with", max(AUC_All200Models), "AUC.")
paste(which(AUC_All200Models == min(AUC_All200Models))[1], "is the best model with", min(AUC_All200Models), "AUC.")
mean(AUC_All200Models)
sd(AUC_All200Models)

##
par(mfrow = c(1,1))
plot(hist(AUC_All200Models), main = "Density Plot of AUC: All 200 models",
     xlab = "AUC", ylab = "Density",  ylim = c(0, 80), xlim = c(0.85, .9),
     col = rgb(34/255, 139/255, 34/255, alpha = 0.5), lwd = 2)



Performance_Model <- c();
for (i in 1:200){
  BestTrainingModel_Train <- 
    read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = i);
    # store correct predictions
  ROC_Model = roc(BestTrainingModel_Train$Ohio_Label, 
                  BestTrainingModel_Train$Exp_Model_ProbScore)
  best_coords <- coords(ROC_Model, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
  best_threshold <- as.numeric(best_coords[1])
  predicted_class <- ifelse(BestTrainingModel_Train$Exp_Model_ProbScore >= best_threshold, 1, 0)
    Performance_Model[i] <- (sum(BestTrainingModel_Train$Ohio_Label == predicted_class)/223)*100}


paste(which(Performance_Model == max(Performance_Model))[1], "is the best model with", max(Performance_Model), "AUC.")
paste(which(Performance_Model == min(Performance_Model))[1], "is the best model with", min(Performance_Model), "AUC.")
mean(Performance_Model)
sd(Performance_Model)

par(mfrow = c(1,1))
plot(hist(Performance_Model), main = "Density Plot of Accuracy: All 200 models",
     xlab = "Accuracy in %", ylab = "Density",  ylim = c(0, 140), xlim = c(75, 85),
     col = rgb(153/255, 50/255, 204/255, alpha = 0.5), lwd = 2)

