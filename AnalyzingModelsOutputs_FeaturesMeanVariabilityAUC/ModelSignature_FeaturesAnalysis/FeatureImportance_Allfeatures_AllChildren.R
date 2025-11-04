# Load Packages the function that can find the pathname given the name of file
#install.packages("rio")
library(rio)
library(easypackages)
libraries("readxl", "ggplot2", "ggalt")
library(readxl)
library(writexl)
#install.packages("here")
library(here)
#install.packages("ggplot2")
library(ggplot2)
library(ggalt)

find_file <- function(filename) {
  path <- list.files(path = here::here(), 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])}
  
ModelsFeatures_200 <- read_excel(find_file("FeatureandCoefficients_200iterations.xlsx"))

path <- "/Users/susmusharma/Desktop/Classes_Records/First_Year_Paper/R_Codes/New_R_Codes_for_AllChildren/RunElasticNetFunction_andSaveModelOutputs/FeatureandCoefficients_200iterations.xlsx"
## Original Features 
Data_features <- names(read_excel(find_file("ModelsOutputTrain_200iterations.xlsx"), sheet = 1)[1, 9:79])

# Reading data from all sheets 
Features <- import_list(path , rbind=TRUE) 
Features_New <- Features # because the previous one has all of them, we save a copy so we can remove the intercept
Features_New <- Features_New[!Features_New$Features == "(Intercept)", ]

## Loop to grab the number of occurrence
ENLRVariables <- unique(Features_New$Features)
NumofOccurence <- c()
for (i in 1:length(ENLRVariables)){
  NumofOccurence[i] <- length(Features_New[Features_New$Features == ENLRVariables[i], 2])} # give me how much each feature occur
VariablesandCount <- data.frame(ENLRVariables, NumofOccurence)
VariablesandCount <- VariablesandCount[order(VariablesandCount$NumofOccurence, decreasing = F), ]
VariablesandCount$ENLRVariables = 
  factor(VariablesandCount$ENLRVariables, levels = VariablesandCount$ENLRVariables) # gives me data frame with features and occurrence

## Grab non-useful data features according to Elastic Net Logistic Regression.
Unused_Variables_method1 <- setdiff(Data_features, VariablesandCount$ENLRVariables)
Unused_Features <- 
  data.frame("ENLRVariables" = Unused_Variables_method1, "NumofOccurence" = rep(0, length(Unused_Variables_method1)))
Unused_Features$ENLRVariables <- 
  factor(Unused_Features$ENLRVariables, levels = Unused_Features$ENLRVariables)


## Grabbing the features from the saved data to compare its accurate.
# ModelsFeatures_200 <- ModelsFeatures_200[order(ModelsFeatures_200$Weightedimportance, decreasing = F),]
# ModelsFeatures_200 <- 
#    ModelsFeatures_200[-49, ]
#  ModelsFeatures_200$Entire_Feature_in1 <- 
#    factor(ModelsFeatures_200$Entire_Feature_in1, levels = ModelsFeatures_200$Entire_Feature_in1)
#View(ModelsFeatures_200)
 
 # ModelsFeatures_200 <- data.frame("ENLRVariables" = ModelsFeatures_200$Entire_Feature_in1, 
 #                                  "NumofOccurence" = ModelsFeatures_200$Weightedimportance)
 ModelsFeatures_200 <- rbind(Unused_Features, VariablesandCount)
#write_xlsx(ModelsFeatures_200, "ModelsFeatures_200_WFII.xlsx")
 ##________________________________________________________________________________________
# Feature Importance Plot: Lollipop plot
ggplot(data= ModelsFeatures_200, aes(y = NumofOccurence,
      x = ENLRVariables)) + geom_lollipop(point.size = 2,
      point.colour = "burlywood3", pch = 19, bg = 2) + theme_minimal()+ coord_flip()+
      labs(y = "Features Importance") + xlab("Features")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(size=10, face="bold"), axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold")) + coord_flip() +
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(),      # draws the x-axis line
        axis.ticks.x = element_line(),
        axis.line.y = element_line())  + scale_y_continuous(expand = c(0, 2), limits = c(0, NA))    # draws the x-axis line
        #axis.ticks.y = element_line() ) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
#ggtitle( "Weighted Feature Importance for DLD") +

## Feature Importance Plot: Bar plot
ggplot(data= ModelsFeatures_200, aes(y = NumofOccurence, 
 x = ENLRVariables)) + geom_bar(stat="identity", color= "black", fill = "lightsalmon", alpha = .4) +
  theme_minimal()+ coord_flip()+ ggtitle( "Weighted Feature Importance for DLD") +
  labs(y = "Features Importance") + xlab("Features")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(size=10, face="bold"), 
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"))

## Feature Importance Plot: Point plot
ggplot(data= ModelsFeatures_200, aes(x = NumofOccurence, 
  y = ENLRVariables)) +geom_point() + theme_bw()+
  ggtitle( "Weighted Feature Importance for DLD") +
  xlab("Features") +ylab("Features Importance") +
theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(size=10, face = "bold"), 
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"))