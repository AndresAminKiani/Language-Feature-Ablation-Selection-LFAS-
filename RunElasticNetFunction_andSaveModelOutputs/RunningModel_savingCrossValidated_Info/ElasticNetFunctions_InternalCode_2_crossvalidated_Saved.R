ElasticNetFunctions_InternalCode_2_crossvalidated_Saved <- function(alphA, partition_percentage, Sample){
  
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
  foldid <<- sample(rep(1:10, length.out = nrow(Exp_Measures_Training)))
  Probability_Score_CV <- rep(NA, nrow(Exp_Measures_Training))  # placeholder to store probabilities
  
  for (i in 1:10) {
    Trainingfold_participants <- which(foldid != i)  # 9/10 of the data
    Testingfold_participants  <- which(foldid == i)  # 1/10 held out
    
    cv_model_eachfold <- cv.glmnet(as.matrix(Exp_Measures_Training[Trainingfold_participants, ]),
                                   Ohio_Label_Training[Trainingfold_participants],
                          family = "binomial",
                          alpha = 0.5,
                          standardize = T)
                          #type.measure = "auc")
    
    Probability_Score_CV[Testingfold_participants] <- predict(cv_model_eachfold,
                                  newx = as.matrix(Exp_Measures_Training[Testingfold_participants, ]),
                                  s = "lambda.1se",
                                  type = "response")
  }
  cv_auc <<- auc(roc(Ohio_Label_Training, Probability_Score_CV))
  
  #set.seed(1)
  My_experimental_model_Final <<- cv.glmnet(as.matrix(Exp_Measures_Training), 
                                      Ohio_Label_Training, 
                                      alpha = 0.5, #alphA, #type.measure = "auc",
                                      family = "binomial" , 
                                      standardize = T,  
                                      foldid = foldid)
  My_experimental_model_Final$foldid
  
  ## I. Probability of Train data
  ## Computing the probability score on the training data using the model
  Probability_Score_InSample <- predict(My_experimental_model_Final, 
                                  s = My_experimental_model_Final$lambda.1se, 
                                  type = "response",
                                  newx = as.matrix(Exp_Measures_Training))
  Probability_Score_InSample <- as.numeric(format(Probability_Score_InSample, scientific = F))
  
  ## Converting the prob score into classification using the optimal threshold
  ROC_train <- roc(ifelse(Group_Train == "SLI", 0, 1), Probability_Score_InSample)
  threshold_experimental <<- coords(ROC_train, x="best")
  In_sample_prediction <- ifelse(Probability_Score_InSample < threshold_experimental[[1]], 0, 1)
  
  ## Saving ENLR prediction scores along with train participant's experimental measures
  Experimentaldata_withProb_Train <<- data.frame(Id_train, Probability_Score_InSample, In_sample_prediction, Probability_Score_CV, Ohio_Label_Training, 
                                                 Group_Train, Comp_zscore_Train, Sex_Train, Age_Train, Exp_Measures_Training)
  names(Experimentaldata_withProb_Train) <<- c("ID","ProbScore_InSample", "Exp_Prediction_InSample", "ProbScore_CV", "Ohio_Label", "Group", "Comp_zscore",
                                               "Sex", "Age", names(Exp_Measures_Training))
  
}