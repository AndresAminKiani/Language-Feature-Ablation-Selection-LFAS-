# importing the required library
install.packages("rio")
library(ggplot2)
library(readxl)
library(openxlsx)
library(writexl)
library(rio)
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
path <- "/Users/susmusharma/Desktop/Classes_Records/First_Year_Paper/R_Codes/New_R_Codes_for_AllChildren/RunElasticNetFunction_andSaveModelOutputs/FeatureandCoefficients_200iterations.xlsx"

# reading data from all sheets 
Features <- import_list(path , rbind=TRUE) 
Features_New <- Features
Features_New <- Features_New[!Features_New$Features == "(Intercept)", ]

## Boxplot figure 
ggplot(Features_New, aes(x = Features, y = Coefficients)) + 
  geom_boxplot() + coord_flip() + labs(title = "Beta coefficients in 200 iterations") +
  theme_bw() + theme(plot.title=element_text(face="bold"))

## Loop to grab the mean
ENLRVariables <- unique(Features_New$Features)
MeanCoefficient <- c()
NumofOccurence <- c()
for (i in 1:length(ENLRVariables)){
MeanCoefficient[i] <- mean(Features_New[Features_New$Features == ENLRVariables[i], 2])
NumofOccurence[i] <- length(Features_New[Features_New$Features == ENLRVariables[i], 2])
}
VarandCoeff <- data.frame(ENLRVariables, MeanCoefficient, NumofOccurence)
VarandCoeff <- 
  format(VarandCoeff[order(VarandCoeff$NumofOccurence,VarandCoeff$MeanCoefficient, 
                           decreasing = T), ], scientific=F)
path <- "/Users/susmisharma/Desktop/ExperimentalMeasures_DLDvsTD_Elastic"
# openxlsx::write.xlsx(VarandCoeff, 
#           file = paste(path, '/Var_MeanCoeff_SelectionNum.xlsx', sep = ""))


