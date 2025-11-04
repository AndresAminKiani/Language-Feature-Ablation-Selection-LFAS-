# How many unique models were selected by elastic net in 200 iterations
Models = read_excel("ModelsPerformance_200iterations.xlsx")
Unique_Models = table(Models$Performance_training)
library(pROC)
library(writexl)

AllModels_Report = data.frame()
AllModels_Report = rownames(c("Model", "HowManyTimes", "AUC", "Correlation"))
Occurance <- numeric(length(Unique_Models))
Model <- character(length(Unique_Models))
AUC <- numeric(length(Unique_Models))
CorrwithCompScore <- numeric(length(Unique_Models))
for (i in 1:length(rownames(Unique_Models))){
  Sheet_Num = which(abs(Models$Performance_training - as.numeric(names(Unique_Models)[i])) < 1e-6)[1]
  Model[i] = paste("Model_", i, sep = "")
  Occurance[i] = as.vector( Unique_Models[i])
  
  # What is the area under the curve of the most occuring model?
  Most_Occuring_Model = read_excel("ModelsOutputTrain_200iterations.xlsx", sheet = Sheet_Num)
  AUC[i] = auc(roc( Most_Occuring_Model$Ohio_Label, Most_Occuring_Model$Exp_Model_ProbScore))
  
  # What is the correlation between the model's probability score and the composite z score?
  CorrwithCompScore[i] = cor(Most_Occuring_Model$Exp_Model_ProbScore, Most_Occuring_Model$Comp_zscore)}

AllModels_Report = data.frame(Model, Occurance, AUC, CorrwithCompScore)
write_xlsx(AllModels_Report, "AllModels_Report.xlsx")