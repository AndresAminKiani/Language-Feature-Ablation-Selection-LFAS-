Elastic_function_AlphaTest <- function(alphA, partition_percentage, seed_num) {
  
  # List of packages you need
  packages_needed <- c("glmnet", "readxl", "pROC", "here")
  
  # Install missing ones (except readxl — we’ll handle it separately)
  missing <- setdiff(packages_needed, rownames(installed.packages()))
  install.packages(setdiff(missing, "readxl"))
  
  # Reinstall readxl from source to fix macOS iconv issue
  #install.packages("readxl", type = "source")
  
  # Load all libraries
  lapply(packages_needed, library, character.only = TRUE)
  
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
  
  set.seed(seed_num)
  foldid <- sample(1:10, nrow(Experimental_Measures_final), replace = TRUE)
  ## Performing the cross validation elastic net logistic regression model 
  My_model <<- cv.glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
                         nfolds = 10, alpha = alphA, family = "binomial", 
                        foldid = foldid)
  
  ## Predicting the probability score for the entire data with the model
  RS_Predicted_All <- predict(My_model, s = My_model$lambda.1se, 
                              type = "response", newx = as.matrix(Experimental_Measures_final))
  RS_Predicted_All <<- as.numeric(format(RS_Predicted_All, scientific = F))
  
  # ## Converting the prob score into classification using the optimal threshold
  ROC <- roc(ifelse(SLI_Label == "SLI", 0, 1),
             as.numeric(RS_Predicted_All))
  threshold_experimental <<- coords(ROC, x="best")
  #Prediction_All <<- ifelse(RS_Predicted_All < threshold_experimental[[1]], 0, 1)
  Prediction_All <<- ifelse(RS_Predicted_All < .5, 0, 1)}
