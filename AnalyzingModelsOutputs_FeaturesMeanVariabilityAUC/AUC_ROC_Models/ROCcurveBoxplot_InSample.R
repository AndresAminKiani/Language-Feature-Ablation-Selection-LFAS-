## caret for confusionmatrix
install.packages(seTLCiff(c("caret", "pROC", "MLeval", "gridExtra", "readxl",
                           "ggplot2"), 
                         rownames(installed.packages())))
#install.packages("caret")
#install.packages("gridExtra")
library(caret) #
library(pROC) # for roc curve
library(MLeval) # to analyze the model
library(gridExtra)
library(readxl)
library(ggplot2)
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

## Data 
AUC_All200Models <- c();
for (i in 1:200){
  ## Let's pull out the model that performed the best for the training set
  BestTrainingModel_Train <- 
    read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = i);
  
  ## ROC on the best and worst models
  AUC_All200Models[i] <- auc(roc(BestTrainingModel_Train$Ohio_Label, 
                          BestTrainingModel_Train$Exp_Model_ProbScore))}
paste(which(AUC_All200Models == max(AUC_All200Models))[1], "is the best model with", max(AUC_All200Models), "AUC.")
paste(which(AUC_All200Models == min(AUC_All200Models))[1], "is the best model with", min(AUC_All200Models), "AUC.")

# Model that predicted the training set the best -----------------------

Best_AUCModelNum<- which(AUC_All200Models == max(AUC_All200Models))[1];

## For the ROC curves on the best training set
Best_AUC_model <- 
  data.frame(read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Best_AUCModelNum)$Exp_Model_ProbScore, 
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Best_AUCModelNum)$Group)
names(Best_AUC_model) <- c("Exp_Model_ProbScore", "Group")

#Worst_AUCModelNum<- 31;
Worst_AUCModelNum<- which(AUC_All200Models == min(AUC_All200Models))[1];
## For the ROC curves on the best training set
Worst_AUC_model <- 
  data.frame(read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Worst_AUCModelNum)$Exp_Model_ProbScore, 
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Worst_AUCModelNum)$Group)
names(Worst_AUC_model) <- c("Exp_Model_ProbScore", "Group")

# ROC and AUC curve -------------------------------------------------------
## Plotting ROC curve and AUC using ggplot for both training and testing set.
# I. AUC
Best_AUC <- 
  round(auc(ifelse(Best_AUC_model$Group == "SLI", 0, 1), 
            Best_AUC_model$Exp_Model_ProbScore),4)
Worst_AUC <- 
  round(auc(ifelse(Worst_AUC_model$Group == "SLI", 0, 1), 
            Worst_AUC_model$Exp_Model_ProbScore),4)

## ROC
Best_ROC <- 
  roc(ifelse(Best_AUC_model$Group == "SLI", 0, 1), 
      Best_AUC_model$Exp_Model_ProbScore)
coords(Best_ROC, x="best")
Worst_ROC <- 
  roc(ifelse(Worst_AUC_model$Group == "SLI", 0, 1), 
      Worst_AUC_model$Exp_Model_ProbScore)
coords(Worst_ROC, x="best")

## Plots
par(mfrow= c(1,1))

#scale_colour_manual(values = c("forestgreen", "royalblue4")) 
ROC_curve_BestandWorst <- ggroc(list(Best_Model = Best_ROC, Worst_Model = Worst_ROC), 
  size = 1.5) + scale_colour_manual(values = c("steelblue", "darkgrey")) +
    scale_linetype_discrete(labels=c("Best_Model", "Worst_Model")) + theme_minimal() +
 labs("ROC") + theme_classic() + ylab(substitute(paste(bold("Sensitivity")))) + 
  xlab(substitute(paste(bold("Specificity")))) +
  theme(legend.position = "none", legend.title=element_blank(),
  plot.title = element_text(face = "bold"), axis.text = element_text(face="bold"))+
  annotate("text",x = 0.65, y = 0.15,label = paste0("AUC (Best model): ", Best_AUC),
    color = "steelblue",fontface = "bold") +
  annotate("text", x = 0.65, y = 0.08, label = paste0("AUC (Worst model): ",Worst_AUC),
    color = "darkgrey",fontface = "bold")
ROC_curve_BestandWorst


# Models with Best and Worst AUC: Boxplot TLC-DLD ----------------------------------------

# Add Set indicator to each dataset
Best_AUC_model$Set <- "Model with highest AUC"
Worst_AUC_model$Set <- "Model with lowest AUC"

# Combine into one tidy frame
Combined_Data <- rbind(Best_AUC_model, Worst_AUC_model)

# Create one boxplot
Boxplot_combined <- ggplot(Combined_Data, aes(x = Set, y = Exp_Model_ProbScore,
                                              color = Group, fill = Group)) +
  geom_boxplot() + coord_cartesian(ylim = c(0, 1)) +
  xlab("") +
  ylab(expression(bold("Model's Probability Score"))) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("grey86", "lightgoldenrod3"),
                    labels = c("SLI" = "DLD", "TD" = "TLC")) +
  scale_colour_manual(values = c("black", "darkgoldenrod4"),
                      labels = c("SLI" = "DLD", "TD" = "TLC")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "right")
Boxplot_combined



