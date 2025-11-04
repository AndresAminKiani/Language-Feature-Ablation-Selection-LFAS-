## Specify the location (path and directory of the file) of the Montgomery database. 
library(easypackages)
libraries("readxl", "ggplot2", "ggalt", "openxlsx", "writexl", "rio", 
          "tidyverse", "pROC")
library(here)

find_file <- function(filename) {
  path <- list.files(path = here::here(), 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])}

data = find_file("Master9_Mtch_117x2_v13_with_PSs_1-1-18_Corrected634ID_zscore.xlsx")
Ohio_data <<- read_excel(data)
Experimental_Measures <- 
  data.frame(Ohio_data[,36:68], Ohio_data[,70:76],
             Ohio_data[,78:85], Ohio_data[,88:94],
             Ohio_data[,96:107], Ohio_data[,109: 112])


## Removing participants with missing values from exp data as well as from other 
## relevant features of the data (age, sex, id, comp-score, etc).
Missing_Mat <- which(is.na(Experimental_Measures), arr.ind = T)
Experimental_Measures_final <<- na.omit(Experimental_Measures)
Ohio_Label <- Ohio_data$SLI[-Missing_Mat[, 1]]

Glmnet_Model <<- glmnet(as.matrix(Experimental_Measures_final), 
                        Ohio_Label, 
                        alpha = 0.5, #alphA, #type.measure = "auc",
                        family = "binomial" , 
                        standardize = F) 
# Extract the lambda sequence
lambda_seq <- Glmnet_Model$lambda

## Here, we observe that if we run the same data over and over, we get the same result. 
Features_Coeff = coef(Glmnet_Model,  s = Glmnet_Model$lambda[10])[,1][coef(Glmnet_Model, s = Glmnet_Model$lambda[10])[,1] != 0]
Features_Coeff
#Features_Coeff_last = Features_Coeff
#Features_Coeff_last == Features_Coeff

## Try 2: Let's run the same analysis but on a shuffled version of the data
My_Sample <<- unlist(lapply(1:1, function(i) sample(nrow(Experimental_Measures_final),
                                                            replace = F)))
Sample_Train <- My_Sample[1: as.integer(1 * length(Ohio_Label))]
Exp_Measures_Shuffled <- Experimental_Measures_final[Sample_Train, ]
Ohio_Label_Shuffled <- Ohio_Label[Sample_Train]

Glmnet_Model_Shuffled <<- glmnet(as.matrix(Exp_Measures_Shuffled), 
                        Ohio_Label_Shuffled, 
                        alpha = 0.5, 
                        family = "binomial", 
                        standardize = F, lambda = lambda_seq) 

## Here, we observe that if we run the shuffled data over and over, we may not get the same results since there is still some randomness from shuffling.   
Features_Coeff_Shuffled = coef(Glmnet_Model_Shuffled,  s = Glmnet_Model_Shuffled$lambda[10])[,1][coef(Glmnet_Model_Shuffled, 
                                              s = Glmnet_Model_Shuffled$lambda[10])[,1] != 0]
Features_Coeff_Shuffled

Features_Coeff_Shuffled == Features_Coeff
all.equal(Features_Coeff_Shuffled, Features_Coeff)
identical(Features_Coeff_Shuffled, Features_Coeff)