Function_IterativelySaves_ModelOutput_3 <- function(data, Num_of_iteration, pathname){
  
  ## Installing necessary packages and groundhog them
  install.packages(setdiff(c("groundhog", "easypackages", "glmnet", "readxl", "pROC", "openxlsx"), 
                           rownames(installed.packages())))
  library("groundhog")
  pkgs <- c("glmnet", "readxl", "pROC", "writexl", "openxlsx")
  groundhog.library(pkgs, "2024-04-15")
  
  ## Loading the data and pulling out dependent and independent variables
  Ohio_data <<- read_excel(data)
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
  Group <<- Ohio_data$group[-Missing_Mat[, 1]]
  Sex <<- Ohio_data$sex[-Missing_Mat[, 1]]
  Age <<- as.integer(Ohio_data$at_mo_at_test/12)[-Missing_Mat[, 1]]
  ID <<- Ohio_data$id[-Missing_Mat[, 1]] 
  Comp_zscore <<- Ohio_data$composite_z_score[-Missing_Mat[, 1]]
  
  ## Select different sample combinations with sample without replacement
  Num_of_iteration <- Num_of_iteration
  # My_Sample <<- lapply(1:Num_of_iteration, function(i) {
  #               set.seed(1)
  #               sample(1:nrow(Experimental_Measures_final))})
    
  set.seed(1334)                                                        
  My_Sample <<- lapply(1:Num_of_iteration, function(i) sample(nrow(Experimental_Measures_final), replace = F))
    # 
  ## Modeling across 200 reshuffled data to generalize model's behavior
  alpha <- 0.5
  #Performance_training <- c()
  #Performance_Both <- data.frame()
  Ls_Models_Output_Train <- list()
  Features <- list()
  Coefficients <- list()
  FeatureandCoefficients <- list()
  
  ## Iteratively running the function that creates the models and saving model output
  for (i in 1:Num_of_iteration){
    Sample = unlist(My_Sample[i])
    ElasticNetFunctions_InternalCode_2(alphA = alpha, 
                                          partition_percentage = 1, 
                                       Sample)
    
    ##Grabbing probability scores including other relevant features of the participants
    Model_iteration <- paste("Model_iteration", i, sep="")
    Ls_Models_Output_Train[[Model_iteration]] <- Experimentaldata_withProb_Train
    
    ## Grabbing features along with their coefficients
    Features[[i]] <- 
      names(as.matrix(coef(My_experimental_model, 
            s = My_experimental_model$lambda.1se))[as.matrix(coef(My_experimental_model, 
            s = My_experimental_model$lambda.1se)) != 0, ])
    Coefficients[[i]] <- 
      as.matrix(coef(My_experimental_model, 
            s = My_experimental_model$lambda.1se))[as.matrix(coef(My_experimental_model, 
            s = My_experimental_model$lambda.1se))!=0]
    
    FeatureandCoefficients[[Model_iteration]] <- 
      data.frame(Features = Features[[i]], Coefficients = Coefficients[[i]])
    print(i)
  }
  Ls_Models_Output_Train <<- Ls_Models_Output_Train
  
  ## Saving model outputs and performance in Excel
  openxlsx::write.xlsx(Ls_Models_Output_Train, 
                       file = paste(pathname, '/ModelsOutputTrain_200iterations.xlsx', sep = ""))
  openxlsx::write.xlsx(FeatureandCoefficients, 
                       file = paste(pathname, '/FeatureandCoefficients_200iterations.xlsx', sep = ""))
}