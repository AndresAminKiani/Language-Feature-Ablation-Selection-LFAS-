seed_list <- 1
all_results <- list()

for (seed in seed_list) {
  alpha = 0.5
    # assuming this sets global Prediction_All and Ohio_Label
    Elastic_function_AlphaTest(alphA = alpha, partition_percentage = 1)
}

plot(My_model, xvar = "lambda", label = TRUE)
mtext("Features shrink with lambda. Top axis indicates the number of features with non-zero coefficients at each lambda.", 
      side = 3, line = 2, cex = 0.8, col = "darkred")

# Get the fitted glmnet model
fit <- My_model$glmnet.fit

# Extract lambdas and coefficient matrix
lambda_vals <- fit$lambda                  # Raw lambdas
coef_matrix <- as.matrix(fit$beta)         # Rows = features, cols = lambdas

# Optional: Pick top 10 most active features
coef_sd <- apply(coef_matrix, 1, sd)
top_feats <- names(sort(coef_sd, decreasing = TRUE))[1:10]

# Plot coefficients vs RAW lambda
matplot(x = lambda_vals,
        y = t(coef_matrix[top_feats, ]),
        type = "l", lty = 1, col = 1:10,
        xlab = "Lambda (raw scale)", ylab = "Coefficient",
        main = "Top Feature Coefficient Paths vs. Raw Lambda")
legend("topright", legend = top_feats, col = 1:10, lty = 1, cex = 0.8)
abline(h = 0, lty = 2, col = "gray40")

final_model <- glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
       family = "binomial", lambda = My_model$lambda.1se, alpha = .5) # My_model$call$alpha

##
Model_1 <- names(as.matrix(coef(My_model))[as.matrix(coef(My_model)) != 0, ])
as.matrix(coef(My_model, ))[as.matrix(coef(My_model))!=0]

Model_2 <- names(as.matrix(coef(final_model))[as.matrix(coef(final_model)) != 0, ])
as.matrix(coef(final_model, ))[as.matrix(coef(final_model))!=0]

Features <- 
  names(as.matrix(coef(My_experimental_model_Final))[as.matrix(coef(My_experimental_model_Final)) != 0, ])
Coefficients <- 
  as.matrix(coef(My_experimental_model_Final, s = My_model$lambda.1se))[as.matrix(coef(My_experimental_model_Final, s = My_model$lambda.1se))!=0]
FeatureandCoefficients[[Model_iteration]] <- 
  data.frame(Features = Features[[i]], Coefficients = Coefficients[[i]])

My_model <<- cv.glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
                       alpha = alphA, family = "binomial")
final_model <- glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
                      family = "binomial", lambda = My_model$lambda.1se, alpha = .5) # My_model$call$alpha
final_model_2 <- glmnet(as.matrix(Experimental_Measures_final), Ohio_Label, 
                      family = "binomial", lambda = My_model$lambda.1min, alpha = .5)
Model_1 <- names(as.matrix(coef(My_model))[as.matrix(coef(My_model)) != 0, ])
Model_2 <- names(as.matrix(coef(final_model))[as.matrix(coef(final_model)) != 0, ])
Model_3 <- names(as.matrix(coef(final_model_2))[as.matrix(coef(final_model_2)) != 0, ])
Model_1 == Model_2

names(as.matrix(coef(My_model, s = My_model$lambda.1se))[as.matrix(coef(My_model, s = My_model$lambda.1se)) != 0, ])
