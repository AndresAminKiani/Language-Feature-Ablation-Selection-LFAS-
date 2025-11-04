ElasticNetFunctions_InternalCode_2 <- function(alphA, partition_percentage, Sample){
  
  ## Partition of the data into training and testing dataset
  split_percentage <- partition_percentage
  Sample_Train <- Sample[1: as.integer(split_percentage * length(Ohio_Label))]
  Sample_Test <- Sample[as.integer(split_percentage * length(Ohio_Label) +1): length(Sample)]
  
  ## Extracting other features into training and testing
  Id_train <- ID[Sample_Train]
  Exp_Measures_Training <- Experimental_Measures_final[Sample_Train, ]
  Ohio_Label_Training <- Ohio_Label[Sample_Train]
  Group_Train <- Group[Sample_Train]
  Comp_zscore_Train <- Comp_zscore[Sample_Train]
  Sex_Train <- Sex[Sample_Train]
  Age_Train <- Age[Sample_Train]
  
  ## Creating a elastic net logistic regression model using the training data
  set.seed(1)
  My_experimental_model <<- cv.glmnet(as.matrix(Exp_Measures_Training), 
                                      Ohio_Label_Training, 
                                      alpha = 0.5, 
                                      family = "binomial" , standardize = T)
  
  ## I. Probability of Train data
  ## Computing the probability score on the training data using the model
  Validating_withtrain <- predict(My_experimental_model, 
                                  s = My_experimental_model$lambda.1se, 
                                  type = "response",
                                  newx = as.matrix(Exp_Measures_Training))
  Validating_withtrain <- as.numeric(format(Validating_withtrain, scientific = F))
  
  ## Converting the prob score into classification using the optimal threshold
  ROC_train <- roc(ifelse(Group_Train == "SLI", 0, 1), Validating_withtrain)
  threshold_experimental <<- coords(ROC_train, x="best")
  Ex_Final_predict_Train <- ifelse(Validating_withtrain < threshold_experimental[[1]], 0, 1)
  
  ## Saving ENLR prediction scores along with train participant's experimental measures
  Experimentaldata_withProb_Train <<- data.frame(Id_train, Validating_withtrain, Ex_Final_predict_Train, Ohio_Label_Training, 
                                                 Group_Train, Comp_zscore_Train, Sex_Train, Age_Train, Exp_Measures_Training)
  names(Experimentaldata_withProb_Train) <<- c("ID","Exp_Model_ProbScore", "Exp_Prediction", "Ohio_Label", "Group", "Comp_zscore",
                                               "Sex", "Age", names(Exp_Measures_Training))}