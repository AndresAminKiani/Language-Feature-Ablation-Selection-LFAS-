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
  path <- list.files(path = "~/Desktop/Classes_Records/First_Year_Paper/R_Codes/New_R_Codes_for_AllChildren", 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])
}

## Data 
CV_BinomDeviance_All200Models <- c();
  ## Let's pull out the model that performed the best for the training set
lambdainfo_All200Models <- read_excel(find_file("AllModels_Lambdas_Info_CV_200Iterations.xlsx"))
OptimalLambda_Info_200_Iterations = lambdainfo_All200Models[lambdainfo_All200Models$lambda_type == "lambda.1se", ]
Best_Fitted_Model = which(OptimalLambda_Info_200_Iterations$binomial_deviance == max(OptimalLambda_Info_200_Iterations$binomial_deviance))
Worst_Fitted_Model = which(OptimalLambda_Info_200_Iterations$binomial_deviance == min(OptimalLambda_Info_200_Iterations$binomial_deviance))
paste(which.max(OptimalLambda_Info_200_Iterations$binomial_deviance), "is the best model with", max(OptimalLambda_Info_200_Iterations$binomial_deviance), "deviance.")
paste(which.min(OptimalLambda_Info_200_Iterations$binomial_deviance), "is the worst model with", min(OptimalLambda_Info_200_Iterations$binomial_deviance), "deviance.")

mean(OptimalLambda_Info_200_Iterations$binomial_deviance)
sd(OptimalLambda_Info_200_Iterations$binomial_deviance)
# Model that predicted the training set the best -----------------------

Best_BinomModelNum<- Best_Fitted_Model;

## For the ROC curves on the best training set
Best_AUC_model <- 
  data.frame(read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Best_BinomModelNum)$Exp_Model_ProbScore, 
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Best_BinomModelNum)$Group,
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Best_BinomModelNum)$Comp_zscore)
names(Best_AUC_model) <- c("Exp_Model_ProbScore", "Group", "Comp_zscore")

#Worst_BinomModelNum<- 31;
Worst_BinomModelNum<- Worst_Fitted_Model;
## For the ROC curves on the best training set
Worst_AUC_model <- 
  data.frame(read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Worst_BinomModelNum)$Exp_Model_ProbScore, 
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Worst_BinomModelNum)$Group,
             read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), 
                        sheet = Worst_BinomModelNum)$Comp_zscore)
names(Worst_AUC_model) <- c("Exp_Model_ProbScore", "Group", "Comp_zscore")

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
        axis.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold"), axis.text = element_text(face="bold"))+
  annotate("text",x = 0.50, y = 0.15,label = paste0("AUROC (Best Fitting Model): ", Best_AUC),
    color = "steelblue",fontface = "bold") +
  annotate("text", x = 0.50, y = 0.08, label = paste0("AUROC ( Worst Fitting Model): ",Worst_AUC),
    color = "darkgrey",fontface = "bold")
ROC_curve_BestandWorst


# Models with Best and Worst AUC: Boxplot TLC-DLD ----------------------------------------

# Add Set indicator to each dataset
Best_AUC_model$Set <- "Best Fitting Model"
Worst_AUC_model$Set <- "Worst Fitting Model"

# Combine into one tidy frame
Combined_Data <- rbind(Best_AUC_model, Worst_AUC_model)

# Create one boxplot
Boxplot_combined <- ggplot(Combined_Data, aes(x = Set, y = Exp_Model_ProbScore,
                                              color = Group, fill = Group)) +
  geom_boxplot() + coord_cartesian(ylim = c(0, 1)) +
  xlab("") +
  ylab(expression(bold("Probability of DLD Presence"))) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("grey86", "lightgoldenrod3"),
                    labels = c("SLI" = "DLD", "TD" = "TD")) +
  scale_colour_manual(values = c("black", "darkgoldenrod4"),
                      labels = c("SLI" = "DLD", "TD" = "TD")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "right")
Boxplot_combined

## Step 7: Plotting the mean figure with ggplot 
## Step 7: Plotting the mean figure with ggplot 
library(ggplot2)
Best_AUC_model$Group = factor(Best_AUC_model$Group)
ggplot(Best_AUC_model, aes(x = Comp_zscore, y = Exp_Model_ProbScore, color = Group, group = Group)) +
  geom_point(shape = 16, size = 3, alpha = .5) +  # main curve
  #geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("SLI" = "forestgreen", "TD" = "steelblue"),
                     labels = c("TD" = "TD", "SLI" = "DLD"),
                     name = NULL) +
  labs(
    title = NULL,
    x = "Composite Language Z-scores",
    y = "Probability Score for DLD (Best Fitting Model)") +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(face = "bold"), 
        axis.title = element_text(face = "bold"))

library(ggplot2)
Worst_AUC_model$Group = factor(Worst_AUC_model$Group)
ggplot(Worst_AUC_model, aes(x = Comp_zscore, y = Exp_Model_ProbScore, color = Group, group = Group)) +
  geom_point(shape = 16, size = 3, alpha = .5) +  # main curve
  #geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("SLI" = "forestgreen", "TD" = "steelblue"),
                     labels = c("TD" = "TD", "SLI" = "DLD"),
                     name = NULL) +
  labs(
    title = NULL,
    x = "Composite Language Z-scores",
    y = "Probability Score for DLD (Worst Fitting Model)") +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

cor.test(Worst_AUC_model$Comp_zscore, Worst_AUC_model$Exp_Model_ProbScore)
cor.test(Best_AUC_model$Comp_zscore, Best_AUC_model$Exp_Model_ProbScore)

