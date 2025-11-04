# PLot violin in R
library(readxl)
library(ggplot2)
library(tidyr)
list.files(path = "/Users/susmusharma/Desktop/Classes_Records/First_Year_Paper/R_Codes/New_R_Codes_for_AllChildren/AnalyzingModelsOutputs_FeaturesMeanVariabilityAUC/AUC_ROC_Models")
Summary_data <- read_excel("Sumary_performance_200Runs.xlsx")
Summary_data$Performance_Model <- Summary_data$Performance_Model/100
Summary_data$All_threshold <- NULL
SummaryData_longformat <- pivot_longer(Summary_data, cols = everything(),
                       names_to = "Variable",
                       values_to = "Value")
SummaryData_longformat$Variable <- factor(SummaryData_longformat$Variable,
                                          levels = c("All_sensitivity", "All_specificity", "AUC_All200Models", "Performance_Model"),
                                          labels = c("Sensitivity", "Specificity", "AUROC", "Accuracy") )

# Create a violin plot  
ggplot(SummaryData_longformat, aes(x = Variable, y = Value, fill = Variable)) +
  geom_violin(fill = "steelblue", color = "black", alpha = 0.7, trim = FALSE, width = 1.2, scale = "width") +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "grey50") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, 
                fill = "black") +
  labs(y = "Model Performance Across 200 runs", x = "") +   ylim(0.7, .9)+
  theme_classic(base_size = 14) + 
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), # box around plot
      axis.line.x  = element_blank(),   # remove x-axis line
      axis.ticks.x = element_blank(),    # remove x-axis ticks
      axis.title.y = element_text(face = "bold"),
    legend.position = "none")

# Create a violin plot  
ggplot(SummaryData_longformat, aes(x = Variable, y = Value, fill = Variable)) +
  geom_violin(fill = "firebrick4", color = "black", alpha = 0.5, trim = FALSE, width = 1.2, scale = "width") +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.4, color = "grey50") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, 
               fill = "black") +
  labs(y = "Model Performance Across 200 runs", x = "") +   ylim(0.7, .9)+
  theme_classic(base_size = 14) + 
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), # box around plot
    axis.line.x  = element_blank(),   # remove x-axis line
    axis.ticks.x = element_blank(),# remove x-axis ticks
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "none")
  
  
