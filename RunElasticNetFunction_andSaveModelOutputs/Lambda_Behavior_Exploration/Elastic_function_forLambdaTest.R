Elastic_function_AlphaTest <- function(alphA, partition_percentage) {
  
  ## loading the relevant libraries 
  install.packages(setdiff(c("glmnet", "readxl", "pROC", "here"), 
                           rownames(installed.packages())))
library(pROC)
  
  find_file <- function(filename) {
    path <- list.files(path = here::here(), 
                       pattern = filename,
                       recursive = TRUE, 
                       full.names = TRUE)
    if (length(path) == 0) stop(paste("File", filename, "not found!"))
    if (length(path) > 1) warning("Multiple files found. Using the first one.")
    return(path[1])
  }
  
  ## Loading the data
  Data_Ori <- read_excel(find_file("Master9_Mtch_117x2_v13_with_PSs_1-1-18_Corrected634ID_zscore.xlsx"))

  ## Loading the data and pulling out dependent and independent variables
  Ohio_data <<- Data_Ori
  Experimental_Measures <- 
    data.frame(Ohio_data[,36:68], Ohio_data[,70:76],
               Ohio_data[,78:85], Ohio_data[,88:94],
               Ohio_data[,96:107], Ohio_data[,109: 112])
  #View(Experimental_Measures)
  
  ## Removing participants with missing values from exp data as well as from other 
  ## relevant features of the data (age, sex, id, comp-score, etc).
  Missing_Mat <- which(is.na(Experimental_Measures), arr.ind = T)
  Experimental_Measures_final <<- na.omit(Experimental_Measures) 
  Ohio_Label <<- Ohio_data$SLI[-Missing_Mat[, 1]]
  SLI_Label <- Data_Ori$group[-Missing_Mat[, 1]]
  
  ## Performing the cross validation elastic net logistic regression model 
  My_model <<- cv.glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
                        alpha = alphA, family = "binomial")
  
  ## Predicting the probability score for the entire data with the model
  RS_Predicted_All <- predict(My_model, s = My_model$lambda.1se, 
                              type = "response", newx = as.matrix(Experimental_Measures_final))
  RS_Predicted_All <<- as.numeric(format(RS_Predicted_All, scientific = F))}
  
