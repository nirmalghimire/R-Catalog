# Load necessary libraries
library(ggplot2)

# Sample data for a normal linear regression model
set.seed(123)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
model <- lm(y ~ x)
residuals <- model$residuals
fitted <- model$fitted.values

# Create Residuals vs Fitted plot
ggplot(data.frame(fitted, residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Plot (Normal)",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Sample data for a non-linear relationship
set.seed(123)
x_nonlin <- rnorm(100)
y_nonlin <- x_nonlin^2 + rnorm(100)
model_nonlin <- lm(y_nonlin ~ x_nonlin)
residuals_nonlin <- model_nonlin$residuals
fitted_nonlin <- model_nonlin$fitted.values

# Create Residuals vs Fitted plot
ggplot(data.frame(fitted_nonlin, residuals_nonlin), aes(x = fitted_nonlin, y = residuals_nonlin)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Plot (Non-Linear)",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Create Q-Q plot for normal residuals
ggplot(data.frame(sample = residuals), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot (Normal Residuals)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Create Q-Q plot for non-normal residuals
ggplot(data.frame(sample = residuals_nonlin), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot (Non-Normal Residuals)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Create Scale-Location plot for normal residuals
sqrt_abs_resid <- sqrt(abs(residuals))
ggplot(data.frame(fitted, sqrt_abs_resid), aes(x = fitted, y = sqrt_abs_resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot (Normal)",
       x = "Fitted Values",
       y = "Square Root of Standardized Residuals") +
  theme_minimal()

# Create Scale-Location plot for non-normal residuals
sqrt_abs_resid_nonlin <- sqrt(abs(residuals_nonlin))
ggplot(data.frame(fitted_nonlin, sqrt_abs_resid_nonlin), aes(x = fitted_nonlin, y = sqrt_abs_resid_nonlin)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot (Non-Normal)",
       x = "Fitted Values",
       y = "Square Root of Standardized Residuals") +
  theme_minimal()

# Create Residuals vs Leverage plot for normal residuals
leverage <- hatvalues(model)
ggplot(data.frame(leverage, residuals), aes(x = leverage, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Leverage Plot (Normal)",
       x = "Leverage",
       y = "Residuals") +
  theme_minimal()

# Create Residuals vs Leverage plot for non-normal residuals
leverage_nonlin <- hatvalues(model_nonlin)
ggplot(data.frame(leverage_nonlin, residuals_nonlin), aes(x = leverage_nonlin, y = residuals_nonlin)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Leverage Plot (Non-Normal)",
       x = "Leverage",
       y = "Residuals") +
  theme_minimal()

# Install and load the pwr package
# install.packages("pwr")
library(pwr)

# Calculate sample size for multiple linear regression
pwr.f2.test(u = 3, # number of predictors
            f2 = 0.15, # effect size (medium)
            sig.level = 0.05, # significance level
            power = 0.80) # power
