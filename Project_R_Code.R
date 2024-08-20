# Import data and load packages
firedata <- read.csv("forestfires.csv", header=T)

library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(GGally)
library(gridExtra)
library(car)
library(glmnet)
library(Matrix)
library(ggfortify)
library(reshape2)

theme_update(plot.title = element_text(hjust = 0.5))

X <- as.numeric(firedata$X)
Y <- as.numeric(firedata$Y)
ISI <- as.numeric(firedata$ISI)
temp <- as.numeric(firedata$temp)
RH <- as.numeric(firedata$RH)
wind <- as.numeric(firedata$wind)
rain <- as.numeric(firedata$rain)
area <- as.numeric(firedata$area)
month <- as.numeric(firedata$month)

# Re-code Variables
# Re-code Month Variable into Season Variable (categorical variable)
season <- numeric()
season[firedata$month=="dec" | firedata$month=="jan" | firedata$month=="feb"] <- 0 # winter
season[firedata$month=="mar" | firedata$month=="apr" | firedata$month=="may"] <- 1 # spring
season[firedata$month=="jun" | firedata$month=="jul" | firedata$month=="aug"] <- 2 # summer
season[firedata$month=="sep" | firedata$month=="oct" | firedata$month=="nov"] <- 3 # fall
firedata$season <- season

# Re-code Day Variable
day <- numeric()
day[firedata$day=="mon"] <- 1
day[firedata$day=="tue"] <- 2
day[firedata$day=="wed"] <- 3
day[firedata$day=="thu"] <- 4
day[firedata$day=="fri"] <- 5
day[firedata$day=="sat"] <- 6
day[firedata$day=="sun"] <- 7
firedata$day <- day

# Randomize rows (before splitting into training and validation)
# Use set.seed() function for reproducibility
set.seed(1)
sample <- sample(nrow(firedata))
firedata <- firedata[sample,]

# Form Training and Validation Data Sets (split data 50-50)
firedata_train <- firedata[1:259,] # 50% of the data
firedata_valid <- firedata[260:517,] # 50% of the data

# Assign Variables in Training Data Set
X <- firedata_train$X
Y <- firedata_train$Y
month <- firedata_train$month
season <- firedata_train$season
day <- firedata_train$day
temp <- firedata_train$temp
RH <- firedata_train$RH
wind <- firedata_train$wind
rain <- firedata_train$rain
area <- firedata_train$area
ISI <- firedata_train$ISI

# Summary Statistics
summary(firedata_train)

# Scatterplot and Correlation Matrix
data <- data.frame(ISI, temp, wind, RH, day, rain, area)
ggpairs(data, upper = list(continuous = wrap("points", alpha = 0.5, size = 0.3)),
        mapping = ggplot2::aes(color = factor(season, labels = c("Winter", "Spring", "Summer", "Fall"))),
        lower = list(continuous = wrap('cor', size = 4))) +
  theme(axis.text = element_text(size = 7)) +
  labs(title = "Figure 1: Scatterplot and Correlation Matrix")

# Boxplots
legend_title <- "season"
data1 <- data.frame(ISI, season)
data1_melt <- melt(data1, id = "season")
p1 <- ggplot(data1_melt, aes(x = variable, y = value, color = factor(season))) + geom_boxplot() + theme_bw() +
  theme(axis.text.x=element_blank()) + labs(x = "", y = "Initial Spread Index (ISI)") +
  scale_color_discrete(legend_title, labels = c("Winter", "Spring", "Summer", "Fall"))

data2 <- data.frame(temp, season)
data2_melt <- melt(data2, id = "season")
p2 <- ggplot(data2_melt, aes(x = variable, y = value, color = factor(season))) + geom_boxplot() + theme_bw() +
  theme(axis.text.x=element_blank()) + labs(x = "", y = "Temperature (ºC)") +
  scale_color_discrete(legend_title, labels = c("Winter", "Spring", "Summer", "Fall"))

data3 <- data.frame(wind, season)
data3_melt <- melt(data3, id = "season")
p3 <- ggplot(data3_melt, aes(x = variable, y = value, color = factor(season))) + geom_boxplot() + theme_bw() +
  theme(axis.text.x=element_blank()) + labs(x = "", y = "Wind (km/hr)") +
  scale_color_discrete(legend_title, labels = c("Winter", "Spring", "Summer", "Fall"))

grid.arrange(p1, p2, p3, nrow = 1, top = "Figure 2: Boxplots of Selected Variables by Season")

# Combined Plot of Histogram and Q-Q Plot for Response Variable (ISI)
# Histogram of Response Variable (ISI)
ISI_data <- data.frame(ISI)
p1 <- ggplot(ISI_data, aes(x = ISI, color = season)) +
  geom_histogram(binwidth = 2, color = "black", fill = "red") +
  labs(x = "Initial Spread Index (ISI)", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q Plot for Response Variable (ISI)
ISI_data <- data.frame(ISI, factor(season))
p2 <- ggplot(ISI_data, aes(sample = ISI, color = factor(season))) +
  stat_qq(size = 1) +
  geom_qq_line(color = "orange") +
  labs(x = "Theoretical Quantile", y = "Initial Spread Index (ISI)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall"))

# Combined Plot of Histogram and Q-Q Plot for ISI
grid.arrange(p1, p2, nrow = 1, top = "Figure 3: Histogram and Q-Q Plot of Initial Spread Index (ISI)")

# LASSO
x <- model.matrix(ISI~X+Y+season+day+temp+RH+wind+rain+area, firedata_train)
y <- ISI
fit <- glmnet::glmnet(x, y, alpha = 1)

# Plot coefficients vs. lasso penalty
pallete <- c('black', 'red', 'blue', 'green', 'orange','maroon','magenta','cyan', 'yellow', 'gray')
Lasso <- autoplot(fit, cex = 0.5, xlim = c(-0.1,2.2), ylim = c(-0.9,0.7)) + 
  scale_colour_manual(values = pallete) + 
  labs(title = "Figure 4: LASSO Trace Plot") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 0.0, y = 0.2, label = "temp") +
  annotate("text", x = 0.75, y = 0.45, label = "wind") +
  annotate("text", x = 1.5, y = 0.2, label = "season") +
  annotate("text", x = 1.75, y = -0.13, label = "day") +
  annotate("text", x = 1.5, y = -0.4, label = "rain") +
  annotate("text", x = 1, y = 0.05, label = "RH")
Lasso

# Ordinary Least Squares Model (Using Training Data)
# (ISI vs. Temperature, Wind, Season (categorical))
model_train <- lm(ISI~temp + wind + factor(season))
summary(model_train)

# Construct Weighted Least Squares (WLS) Model
# Residuals
residual <- residuals(model_train)

# Absolute value of residual
abs_residual <- abs(residuals(model_train))

data <- data.frame(temp, residual, abs_residual)

p11 <- ggplot(data, aes(x = temp, y = residual)) + geom_point(size = 0.1) +  
  ggtitle("Residuals") 

p12 <- ggplot(data, aes(x = temp, y = abs_residual)) + geom_point(size = 0.1) +  
  ggtitle("Absolute Residuals") 

grid.arrange(p11, p12, ncol = 2)

# Weighted Least Squares
# Calculate fitted values from a regression of absolute residuals vs predictors
wts <- 1 / fitted(lm(abs(residuals(model_train))~temp + wind + factor(season)))^2

# Fit a WLS model using weights = 1 / (fitted values)^2
wls_train <- lm(ISI~temp + wind + factor(season), weights = wts)
summary(wls_train)

# Weighted Least Squares Diagnostics
stand_resid_train <- rstandard(wls_train)
fitted_train <- fitted(wls_train)
leverages_train <- hatvalues(wls_train)
student_resid_train <- rstudent(wls_train)
season <- factor(season)
index_train <- seq(1, 259, 1)

data_diagnostics_train <- data.frame(stand_resid_train, fitted_train, leverages_train, student_resid_train, ISI, season, index_train)

# Weighted Least Squares Combined Standardized Residuals Plots
# Standardized Residuals vs. ISI Fitted Values Plot
p13 <- ggplot() + geom_point(data = data_diagnostics_train, aes(x = fitted_train, y = stand_resid_train, col = season), size = 1) +
  geom_hline(yintercept = 2, color = "orange") + geom_hline(yintercept = -2, color = "orange") +
  labs(x = "ISI Fitted Value", y = "Standardized Residual") +
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  theme_bw() +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall")) +
  theme(plot.title = element_text(hjust = 0.5))

# Standardized Residuals vs. ISI True Values Plot
p14 <- ggplot() + geom_point(data = data_diagnostics_train, aes(x = ISI, y = stand_resid_train, col = season), size = 1) +
  geom_hline(yintercept = 2, color = "orange") + geom_hline(yintercept = -2, color = "orange") +
  labs(x = "ISI True Value", y = "Standardized Residual") +
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  theme_bw() +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall")) +
  theme(plot.title = element_text(hjust = 0.5))

# Combined Standardized Residuals Plots
grid.arrange(p13, p14, nrow = 1, top = "Figure 5: Weighted Least Squares Standardized Residuals Plots")

# Weighted Least Squares Combined Histogram and Q-Q Plots for Standardized Residuals
# Histogram of Standardized Residuals
p15 <- ggplot(data_diagnostics_train, aes(x = stand_resid_train)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "red") +
  labs(x = "Standardized Residual", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q Plot
p16 <- ggplot(data_diagnostics_train, aes(sample = stand_resid_train, color = season)) +
  stat_qq(size = 1) +
  geom_qq_line(color = "orange") +
  labs(x = "Theoretical Quantile", y = "Standardized Residual") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall"))

# Combined Histogram and Q-Q Plots for Standardized Residuals
grid.arrange(p15, p16, nrow = 1, top = "Figure 6: Weighted Least Squares Histogram and Q-Q Plot of Standardized Residuals")

# Weighted Least Squares Combined Outlier Detection Plots
# Plot of Leverages (to detect outliers in the x-space)
p17 <- ggplot() + geom_point(data = data_diagnostics_train, aes(x = index_train, y = leverages_train, color = season), size = 1) +
  geom_hline(yintercept = 1 / length(index_train), color = "orange") + geom_hline(yintercept = 1, color = "orange") +
  labs(x = "Index", y = "Leverage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall"))

# Plot of Studentized Residuals (to detect outliers in the y-space)
p18 <- ggplot() + geom_point(data =  data_diagnostics_train, aes(x = index_train, y = student_resid_train, color = season), size = 1) +
  geom_hline(yintercept = -3, color = "orange") + geom_hline(yintercept = 3, color = "orange") +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  labs(x = "Index", y = "Studentized Residual") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall"))

# Combined Outlier Detection Plots
grid.arrange(p17, p18, nrow = 1, top = "Figure 7: Weighted Least Squares Leverage and Studentized Residuals Plots")

# Remove Outliers from Training Data
ISI_rev <- ISI[-c(which(student_resid_train > 3 | student_resid_train < -3 | leverages_train > 0.20))]
temp_rev <- temp[-c(which(student_resid_train > 3 | student_resid_train < -3 | leverages_train > 0.20))]
season_rev <- season[-c(which(student_resid_train > 3 | student_resid_train < -3 | leverages_train > 0.20))]
wind_rev <- wind[-c(which(student_resid_train > 3 | student_resid_train < -3 | leverages_train > 0.20))]

data_rev <- data.frame(ISI_rev, temp_rev, season_rev, wind_rev)

# Ordinary Least Squares (After Removing Outliers)
model_train_rev <- lm(ISI_rev~temp_rev + wind_rev + factor(season_rev))
summary(model_train_rev)

# Weighted Least Squares (After Removing Outliers)
# Calculate fitted values from a regression of absolute residuals vs predictors
wts_rev <- 1 / fitted(lm(abs(residuals(model_train_rev))~temp_rev + wind_rev + factor(season_rev)))^2

# Fit a WLS model using weights = 1 / (fitted values)^2
wls_train_rev <- lm(ISI_rev~temp_rev + wind_rev + factor(season_rev), weights = wts_rev)
summary(wls_train_rev)

# Weighted Least Squares Validation
# Residuals for training data
resid_train_rev <- resid(wls_train_rev)

# Prediction for validation data
data <- data.frame(temp_rev = firedata_valid$temp, wind_rev = firedata_valid$wind, season_rev = factor(firedata_valid$season))
predict_valid <- predict(wls_train_rev, se.fit = TRUE, newdata = data)
resid_valid <- firedata_valid$ISI - predict_valid$fit

# Mean Square Error for training data
mean((resid_train_rev)^2) # MSE for training data is 10.37
mean((resid_valid)^2) # MSE for validation data is 20.33

# Relative Mean Square Error (can multiply by 100 to convert to a %)
mean((resid_train_rev)^2) / mean((ISI_rev)^2) # Relative MSE for training data is approximately 0.1144 (11.44%)
mean((resid_valid)^2) / mean((firedata_valid$ISI)^2) # Relative MSE for validation data is approximately 0.1801 (18.01%)

# Remove outlier from validation data and repeat previous analysis
# Look for an outlier in the validation dataset 
summary(resid_valid)
head(sort(resid_valid, decreasing = T), n = 10)
tail(sort(resid_valid, decreasing = T), n = 10)

# Remove the largest residual (nearly 3x the second largest residual in magnitude)
predict <- predict_valid$fit
predict_valid_rev <- predict[-c(which(resid_valid > 15))]
resid_valid_rev <- resid_valid[-c(which(resid_valid > 15))]
ISI_valid_rev <- firedata_valid$ISI[-c(which(resid_valid > 15))]
season_valid_rev <- firedata_valid$season[-c(which(resid_valid > 15))]

# Repeat the previous analysis (mean square error/relative mean square error) with this outlier removed
# Mean Square Error for validation data
mean((resid_valid_rev)^2) # MSE for validation data (without outlier) is 12.40

# Relative Mean Square Error for validation data (can multiply by 100 to convert to a %)
mean((resid_valid_rev)^2) / mean((ISI_valid_rev)^2) # Relative MSE for validation data (without outlier) is approximately 0.1226 (12.26%)

# Create data frame with validation observations and predicted values
test <- data.frame(ISI_valid_rev, factor(season_valid_rev), predict_valid_rev, 1:length(predict_valid_rev));
colnames(test)[1] = "ISI"
colnames(test)[2] = "Season"
colnames(test)[3] = "Prediction"
colnames(test)[4] = "Index"

# Combined Validation Plots
# Plot Initial Spread Index vs Prediction for Validation Data Set 
p19 <- ggplot(data = test, aes(x = ISI, y = Prediction, color = Season)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "orange") +
  labs(x = "ISI True Value", y = "ISI Predicted Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(labels = c("Winter", "Spring", "Summer", "Fall"))

# Further Comparisons of Predicted Values vs. True Values for Validation Data
p20 <- ggplot(data = test, aes(x = Index)) +
  geom_line(aes(y = ISI, color = "ISI")) + 
  geom_line(aes(y = Prediction, color = "Prediction"), linetype = "twodash") +  
  scale_color_manual(name = element_blank(), labels = c("True ISI","Predicted ISI"),
                     values = c("pink", "steelblue")) + labs(y = "") + 
  labs(x = "Index", y = "ISI") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Combined Validation Plots
grid.arrange(p19, p20, ncol = 1, top = "Figure 8: Weighted Least Squares Validation and Prediction")