ggplot(AllModels_Lambdas_Info, aes(x = lambda_type, y = lambda_value, group = Iteration, color = Iteration)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Lambda.min vs Lambda.1se Per Model",
       x = "Lambda Type",
       y = "Lambda Value",
       color = "Model ID") +
  theme_minimal()

ggplot(AllModels_Lambdas_Info, aes(x = lambda_value, y = binomial_deviance, color = lambda_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Lambda vs Deviance across Models",
       x = "Lambda Value",
       y = "Binomial Deviance",
       color = "Lambda Type") +
  theme_minimal()