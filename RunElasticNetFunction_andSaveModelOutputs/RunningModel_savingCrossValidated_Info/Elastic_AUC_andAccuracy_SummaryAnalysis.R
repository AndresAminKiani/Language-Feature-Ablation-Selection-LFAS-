# Load data and packages
library(easypackages)
libraries("readxl", "ggplot2", "ggalt", "openxlsx", "writexl", "rio", 
          "tidyverse", "pROC")

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


AUC_All200Models <- c();
for (i in 1:200){
## Let's pull out the model that performed the best for the training set
BestTrainingModel_Train <- 
  read_excel(find_file("ModelsOutputTrain_CV_200Iterations.xlsx"), sheet = i);

## ROC on the best and worst models
AUC_All200Models[i] <- auc(roc(BestTrainingModel_Train$Ohio_Label, 
                     BestTrainingModel_Train$ProbScore_CV)) }
paste(which(AUC_All200Models == max(AUC_All200Models))[1], "is the best model with", max(AUC_All200Models), "AUC.")
paste(which(AUC_All200Models == min(AUC_All200Models))[1], "is the best model with", min(AUC_All200Models), "AUC.")
Mean_AUC = mean(AUC_All200Models)
SD_AUC = sd(AUC_All200Models)

##
par(mfrow = c(1,1))
plot(hist(AUC_All200Models), main = "AUC Distribution across Models: Using CV Prob Scores",
     xlab = "AUC", ylab = "Density",  ylim = c(0, 80), xlim = c(0.80, .9),
     col = rgb(34/255, 139/255, 34/255, alpha = 0.5), lwd = 2)
# Add text with mean and SD
text(x = .86, y = 60,
     labels = paste0("Mean = ", round(Mean_AUC, 2), "%\nSD = ", round(SD_AUC, 2), "%"),
     adj = 0, cex = 0.9, font = 2)
#Accuracy_Distribution_onCV


Performance_Model <- c();
for (i in 1:200){
  BestTrainingModel_Train <- 
    read_excel(find_file("ModelsOutputTrain_CV_200Iterations.xlsx"), sheet = i);
    # store correct predictions
  ROC_Model = roc(BestTrainingModel_Train$Ohio_Label, 
                  BestTrainingModel_Train$ProbScore_CV)
  best_coords <- coords(ROC_Model, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
  best_threshold <- as.numeric(best_coords[1])
  predicted_class <- ifelse(BestTrainingModel_Train$ProbScore_CV >= best_threshold, 1, 0)
    Performance_Model[i] <- (sum(BestTrainingModel_Train$Ohio_Label == predicted_class)/223)*100}


paste(which(Performance_Model == max(Performance_Model))[1], "is the best model with", max(Performance_Model), "Accuracy %.")
paste(which(Performance_Model == min(Performance_Model))[1], "is the best model with", min(Performance_Model), "Accuracy %.")
Mean_Performance = mean(Performance_Model)
SD_Performance = sd(Performance_Model)

par(mfrow = c(1,1))
plot(hist(Performance_Model), main = "Accuracy % Distribution across Models: Using CV Prob Scores ",
     xlab = "Accuracy in %", ylab = "Density",  ylim = c(0, 120), xlim = c(74, 85),
     col = rgb(153/255, 50/255, 204/255, alpha = 0.5), lwd = 2)
text(x = 75, y = 90,
     labels = paste0("Mean = ", round(Mean_Performance, 4), "%\nSD = ", round(SD_Performance, 4), "%"),
     adj = 0, cex = 0.9, font = 2)

