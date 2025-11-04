library(writexl)
library(dplyr)
library(tidyr)
library(readxl)
#install.packages("here")
library(here)

find_file <- function(filename) {
  path <- list.files(path = "~/Desktop", 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])
}


# Step 1: Load the proability of models data 
ProbabilityScores_withCompScores_ForAll = read_excel(find_file("EachModel_ProbabilityScores_forAllParticipants.xlsx"))
#list.files(path = "~", pattern = "EachModel_ProbabilityScores_forAllParticipants.xlsx", recursive = TRUE, full.names = TRUE)


# Step 2: Grab the probability scores as matrix and mean and standard deviation of the matrix for each participant
ProbabilityScores = ProbabilityScores_withCompScores_ForAll[1:(dim(ProbabilityScores_withCompScores_ForAll)[1]-2), 2:dim(ProbabilityScores_withCompScores_ForAll)[2]]
ProbabilityScores_Matrix_numeric = apply(ProbabilityScores, 2, as.numeric)
Mean_ProbabilityScores_Matrix = colMeans(ProbabilityScores_Matrix_numeric[,1:223]) # Mean
SD_ProbabilityScores_Matrix = apply(ProbabilityScores_Matrix_numeric[, 1:223], 2, sd, na.rm = TRUE) # SD
SE_ProbabilityScores_Matrix = SD_ProbabilityScores_Matrix / sqrt(200) # SE

# Step 3: Grab Confidence Interval
ci_upper <- Mean_ProbabilityScores_Matrix + 1.96 * SE_ProbabilityScores_Matrix # Upper CI
ci_lower <- Mean_ProbabilityScores_Matrix - 1.96 * SE_ProbabilityScores_Matrix # Lower CI

# Step 4: Grab composite scores and Ohio Label for analysis 
CompositeScore = as.numeric(ProbabilityScores_withCompScores_ForAll[201, 2:dim(ProbabilityScores_withCompScores_ForAll)[2]])
Ohio_Label = as.numeric(ProbabilityScores_withCompScores_ForAll[202, 2:dim(ProbabilityScores_withCompScores_ForAll)[2]])

# Step 5: Create a data.frame with CI, Mean, Composite Scores etc
Variability_Data = data.frame(CompositeScore, Ohio_Label, SD_ProbabilityScores_Matrix, Mean_ProbabilityScores_Matrix, ci_upper, ci_lower)
Variability_Data <- Variability_Data[order(Variability_Data$CompositeScore), ]
Variability_Data$Ohio_Label <- factor(Variability_Data$Ohio_Label, levels = c("0", "1"), labels = c("Typical", "DLD"))

# Step 6: Test if models variablity are better in the extreme end of the disorder and typical or in the middle cut-off regions
# Start with an empty plot using type = "n" (no points or lines)
plot(Variability_Data$CompositeScore, Variability_Data$SD_ProbabilityScores_Matrix, 
     type = "n", 
     lwd = 2, 
     xlab = "Composite Score", 
     ylab = "Probabiliy Score", 
     main = "Composite Score vs. Probability Metrics",
     ylim = c(-.1,.1))

# Add standard deviation as blue line
lines(Variability_Data$CompositeScore, Variability_Data$SD_ProbabilityScores_Matrix, 
      lwd = 2, col = "blue")

# Add mean probability score as red line
lines(Variability_Data$CompositeScore, Variability_Data$Mean_ProbabilityScores_Matrix, 
      lwd = 4, col = "red")

# Optional: Add a legend
legend("topright", legend = c("SD", "Mean"), 
       col = c("blue", "red"), lwd = 2)

## Plot confidence interval
plot(Variability_Data$CompositeScore, Variability_Data$Mean_ProbabilityScores_Matrix, 
     type = "p", lwd = 2, col = "blue", ylim = range(0, 1),
     xlab = "Composite Score",
     ylab = "Probability Score",
     main = "Mean with Confidence Interval")
polygon(c(Variability_Data$CompositeScore, rev(Variability_Data$CompositeScore)),
        c(Variability_Data$ci_upper, rev(Variability_Data$ci_lower)),
        col = rgb(0, 0, 1, 0.5), border = 'black')

# Checking if the quadratic model is mathematically significant
model <- lm(SD_ProbabilityScores_Matrix ~ CompositeScore + I(CompositeScore^2), data = Variability_Data)
summary(model)

# Plot raw data
plot(Variability_Data$CompositeScore, Variability_Data$SD_ProbabilityScores_Matrix,
     main = "SD vs Composite Score with Quadratic Fit",
     xlab = "Composite Score",
     ylab = "Standard Deviation",
     pch = 19,
     type = "p",
     col = "gray")

# Add fitted line
lines(sort(Variability_Data$CompositeScore),
      fitted(model)[order(Variability_Data$CompositeScore)],
      col = "blue",
      lwd = 3)

## Step 7: Plotting the mean figure with ggplot 
library(ggplot2)
ggplot(Variability_Data, aes(y = CompositeScore, x = Mean_ProbabilityScores_Matrix, color = Ohio_Label, group = "Ohio_Label")) +
  geom_point(shape = 16, size = 3, alpha = .5) +  # main curve
  #geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("DLD" = "forestgreen", "Typical" = "steelblue")) +
  labs(
    title = "Average Predicted Scores Across Composite Score",
    y = "Composite Z Score",
    x = "Participants Average Probability Score for DLD from 200 models") +
  theme_classic(base_size = 14)


## Step 7: Plotting the mean figure with ggplot 
library(ggplot2)
ggplot(Variability_Data, aes(x = CompositeScore, y = Mean_ProbabilityScores_Matrix, color = Ohio_Label, group = "Ohio_Label")) +
  geom_point(shape = 16, size = 3, alpha = .5) +  # main curve
 # geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("DLD" = "forestgreen", "Typical" = "steelblue")) +
  labs(
    title = "Average Predicted Scores Across Composite Score",
    x = "Composite Z Score",
    y = "Participants Average Probability Score for DLD from 200 models") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

## Step 7: Plotting the predictability variability across composite z score figure with ggplot 
library(ggplot2)
ggplot(Variability_Data, aes(x = CompositeScore, y = SD_ProbabilityScores_Matrix, color = Ohio_Label, group = "Ohio_Label")) +
  geom_point(shape = 1, size = 2) +  # main curve +
  scale_color_manual(values = c("DLD" = "forestgreen", "Typical" = "steelblue")) +
  labs(
    title = "Predicted Scores Variabillity Across Composite Score",
    x = "Composite Z Score",
    y = "Participants Predicted Score Variability from 200 models") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

