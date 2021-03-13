library(tidyverse)
library(caret)

# Boston data set: median house value (mdev), percentage of lower status of the population (lstat).
data("Boston", package = "MASS")

set.seed(123)
training.samples <- Boston$medv %>%
    createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Plot of the data points
ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    #stat_smooth(method="auto") + 
    ggtitle("House Value in Boston vs Percentage of lower status population") + 
    theme(plot.title = element_text(hjust = 0.5))


#####################
# Linear Regression #
#####################

# Build the model
model <- lm(medv ~ lstat, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
LR.performance = 
    data.frame(
        RMSE = RMSE(predictions, test.data$medv),
        R2 = R2(predictions, test.data$medv),
        Name = "Linear Regression"
    )
LR.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ x) + 
    ggtitle("Linear Regression") + 
    theme(plot.title = element_text(hjust = 0.5))


#########################
# Polynomial Regression #
#########################

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
PR.performance = data.frame(
    RMSE = RMSE(predictions, test.data$medv),
    R2 = R2(predictions, test.data$medv),
    Name = "Polynomial Regression"
)
PR.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE)) + 
    ggtitle("Polynomial Regression") + 
    theme(plot.title = element_text(hjust = 0.5))

#####################################
# Log transformed Linear Regression #
#####################################

# Build the model
model <- lm(medv ~ log(lstat), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
LLR.performance = data.frame(
    RMSE = RMSE(predictions, test.data$medv),
    R2 = R2(predictions, test.data$medv),
    Name = "Log transformed Linear Regression"
)
LLR.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ log(x)) + 
    ggtitle("Log transformed Linear Regression") + 
    theme(plot.title = element_text(hjust = 0.5))


#####################
# Spline Regression #
#####################

library(splines)
# Build the model
knots <- quantile(train.data$lstat, p = c(0.25, 0.5, 0.75))
model <- lm (medv ~ bs(lstat, knots = knots), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
SPL.performance = data.frame(
    RMSE = RMSE(predictions, test.data$medv),
    R2 = R2(predictions, test.data$medv),
    Name = "Spline Regression"
)
SPL.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3)) + 
    ggtitle("Spline Regression") + 
    theme(plot.title = element_text(hjust = 0.5))



##############################
#   Non Parametric Models    #
# -------------------------- #
# Generalized Additive Model #
##############################

library(mgcv)
# Build the model
model <- gam(medv ~ s(lstat), data = train.data)
model
# Make predictions
predictions <- model %>% predict(test.data)
predictions
# Model performance
GAM.performance = data.frame(
    RMSE = RMSE(predictions, test.data$medv),
    R2 = R2(predictions, test.data$medv),
    Name = "Generalized Additive Model"
)
GAM.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = gam, formula = y ~ s(x)) + 
    ggtitle("Generalized Additive Model") + 
    theme(plot.title = element_text(hjust = 0.5))



####################
# Local Regression #
####################

# Build the model
model <- loess(medv ~ lstat, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
LoR.performance = 
    data.frame(
        RMSE = RMSE(predictions, test.data$medv),
        R2 = R2(predictions, test.data$medv),
        Name = "Local Regression"
    )
LoR.performance

ggplot(train.data, aes(lstat, medv) ) +
    geom_point() +
    stat_smooth(method = loess, formula = y ~ x) + 
    ggtitle("Local Regression") + 
    theme(plot.title = element_text(hjust = 0.5))



#######################
# Summary of the Data #
#######################

LR.performance
PR.performance
LLR.performance
SPL.performance
GAM.performance
LoR.performance


ggplot(train.data, aes(lstat, medv) ) +
    geom_point(aes(y=medv, color="Data")) +
    stat_smooth(method = lm, formula = y ~ x, aes(color = 'linear'), se=FALSE) + 
    stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), aes(color = 'polinomial'), se=FALSE) + 
    stat_smooth(method = lm, formula = y ~ log(x), aes(color = 'logarithmic'), se=FALSE) + 
    stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), aes(color = 'spline'), se=FALSE) + 
    stat_smooth(method = gam, formula = y ~ s(x), aes(color = 'adative'), se=FALSE) + 
    stat_smooth(method = loess, formula = y ~ x, aes(color = 'local'), se=FALSE) + 
    ggtitle("Regression model comparison") + 
    theme(plot.title = element_text(hjust = 0.5))

# RMSE = Root Mean Square Error (minimize), R2 = R squared (maximize)
total.performance <- rbind(LR.performance, PR.performance, LLR.performance, SPL.performance, GAM.performance, LoR.performance)
total.performance

