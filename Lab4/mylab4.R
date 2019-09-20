spruce.df = read.csv("SPRUCE.csv")

windows()

# Load the s20x library and make a lowess smoother scatter plot
library(s20x)
trendscatter(Height~BHDiameter, f = 0.5, data = spruce.df)

# Make a linear model 
spruce.lm = with(spruce.df, lm(Height~BHDiameter))

# Find height residuals
height.res = residuals(spruce.lm)

# Find fitted values for height
height.fit = fitted(spruce.lm)

# Plot residuals vs fitted values
plot(height.fit, height.res)
trendscatter(height.fit, height.res)

# Make a residual plot
plot(spruce.lm, which =1)

# Test the normality of the linear model
normcheck(spruce.lm, shapiro.wilk = TRUE)


# Task 4

# Make a quadratic model of the data
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2), data=spruce.df)

# Plot height vs diameter with the quadratic trendline overlaid
with(spruce.df, plot(BHDiameter, Height))

# Make the quadratic trendline
quad_trend = function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}

# Add the trendline
curve(quad_trend, lwd = 2, col = "green", add = TRUE)


# Fitted values for the quadratic trend
quad.fit = fitted(quad.lm)

# Plot the residuals against the fitted quadratic function
plot(quad.fit, residuals(quad.lm))

# Test the normality of the quadratic model
normcheck(quad.lm, shapiro.wilk = TRUE)

# Get the confidence intervals of the model coefficients
ciReg(quad.lm)


# Predict height from diameter
predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))


# Summarize the linear model coefficients and R-squared
summary.lm(quad.lm)$adj

