seed_list <- 1:20
all_results <- list()

for (seed in seed_list) {
  #set.seed(seed)
  Performance_Alpha <- numeric(10)
  
  for (i in 1:10) {
    alpha <- i / 10
    # assuming this sets global Prediction_All and Ohio_Label
    Elastic_function_AlphaTest(alphA = alpha, partition_percentage = 1, seed = seed)
    
    # store correct predictions
    Performance_Alpha[i] <- sum(Ohio_Label == Prediction_All)
  }
  all_results[[as.character(seed)]] <- Performance_Alpha
}

Performance_Alpha_Unnested <- do.call(rbind, all_results)
Performance_Alpha_Unnested_Averaged <- colMeans(Performance_Alpha_Unnested)
#Performance_Alpha
plot(seq(0.1, 1, by = .1), Performance_Alpha_Unnested_Averaged, type = "l", lwd = 3,
     col = "steelblue", ylim = c(0, 223),
     main = paste(" 100% Used as Training for Best Alpha Value"), 
     xlab = "Alpha Value", ylab = "Average Elastic-Net Classification Acc Across 20 Random Sampling of Data")
