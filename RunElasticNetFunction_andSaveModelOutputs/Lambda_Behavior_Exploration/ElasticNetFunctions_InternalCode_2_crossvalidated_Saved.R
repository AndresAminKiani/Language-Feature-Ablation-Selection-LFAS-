ElasticNetFunctions_InternalCode_2_crossvalidated_Saved <- function(alphA, partition_percentage, Sample){
  
  ## Partition of the data into training and testing dataset
  split_percentage <- partition_percentage
  Sample_Train <- Sample[1: as.integer(split_percentage * length(Ohio_Label))]
  Sample_Test <- Sample[as.integer(split_percentage * length(Ohio_Label) +1): length(Sample)]
  foldid_SaveAll <- list()
  Id_CVSaveAll <- list()
  
  ## Extracting other features into training and testing
  Id_train <<- ID[Sample_Train]
  Exp_Measures_Training <<- Experimental_Measures_final[Sample_Train, ]
  Ohio_Label_Training <- Ohio_Label[Sample_Train]
  Group_Train <- Group[Sample_Train]
  Comp_zscore_Train <- Comp_zscore[Sample_Train]
  Sex_Train <- Sex[Sample_Train]
  Age_Train <- Age[Sample_Train]
  
  ## Creating a elastic net logistic regression model using the training data
  set.seed(1)
  foldid <<- sample(rep(1:10, length.out = nrow(Exp_Measures_Training)))
  foldid_SaveAll[[Model_num]] <<- foldid
  Id_CVSaveAll[[Model_num]] <<- Id_train
  # Probability_Score_CV <- rep(NA, nrow(Exp_Measures_Training))  # placeholder to store probabilities
  # 
  # for (j in 1:10) {
  #   Trainingfold_participants <- which(foldid != j)  # 9/10 of the data
  #   Testingfold_participants  <- which(foldid == j)  # 1/10 held out
  #   
  #   cv_model_eachfold <- cv.glmnet(as.matrix(Exp_Measures_Training[Trainingfold_participants, ]),
  #                                  Ohio_Label_Training[Trainingfold_participants],
  #                         family = "binomial",
  #                         alpha = 0.5,
  #                         standardize = T)
  #                         #type.measure = "auc")
  #   
  #   Probability_Score_CV[Testingfold_participants] <- predict(cv_model_eachfold,
  #                                 newx = as.matrix(Exp_Measures_Training[Testingfold_participants, ]),
  #                                 s = "lambda.1se",
  #                                 type = "response")
  # }
  # cv_auc <<- auc(roc(Ohio_Label_Training, Probability_Score_CV))
  
  #set.seed(1)
  My_experimental_model_Final <<- cv.glmnet(as.matrix(Exp_Measures_Training), 
                                      Ohio_Label_Training, 
                                      alpha = 0.5, #alphA, #type.measure = "auc",
                                      family = "binomial" , 
                                      standardize = T) 
                                     # foldid = foldid)
  # # Extract lambda values
  # lambda_min <- My_experimental_model_Final$lambda.min
  # lambda_1se <- My_experimental_model_Final$lambda.1se
  # 
  # # Index for each lambda
  # index_min <- which(My_experimental_model_Final$lambda == lambda_min)
  # index_1se <- which(My_experimental_model_Final$lambda == lambda_1se)
  # 
  # # Corresponding performance measures (binomial deviance)
  # deviance_min <- My_experimental_model_Final$cvm[index_min]
  # deviance_1se <- My_experimental_model_Final$cvm[index_1se]
  # 
  # # Extract number of non-zero coefficients from cross-validation path
  # nonzero_min <- My_experimental_model_Final$nzero[index_min]
  # nonzero_1se <- My_experimental_model_Final$nzero[index_1se]
  # 
  # EachModel_lambda_info <<- data.frame(
  #   lambda_type = c("lambda.min", "lambda.1se"),
  #   lambda_value = c(lambda_min, lambda_1se),
  #   binomial_deviance = c(deviance_min, deviance_1se),
  #   nonzero = c(nonzero_min, nonzero_1se),
  #   Iteration = c(Model_num, Model_num))  # This adds a column called 'flag' with value 1 for both rows
  
  
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
  Experimentaldata_withProb_Train <<- data.frame(Id_train, Probability_Score_InSample, In_sample_prediction, Ohio_Label_Training, 
                                                 Group_Train, Comp_zscore_Train, Sex_Train, Age_Train, Exp_Measures_Training)
  names(Experimentaldata_withProb_Train) <<- c("ID","ProbScore_InSample", "Exp_Prediction_InSample", "Ohio_Label", "Group", "Comp_zscore",
                                               "Sex", "Age", names(Exp_Measures_Training))}