# Task 2
spruce = read.csv("SPRUCE.csv")
head(spruce)

# Task 3
# Plot the spruce tree data appropriately
plot(Height~BHDiameter, main = "Justin Kleiber's Spruce Scatter Plot", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)

# Load the s20x library and make three trendline scatter plots
library(s20x)
layout(matrix(1:3, nrow = 3, ncol = 1))
trendscatter(Height~BHDiameter, f = 0.5, data = spruce)
trendscatter(Height~BHDiameter, f = 0.6, data = spruce)
trendscatter(Height~BHDiameter, f = 0.7, data = spruce)

# Make a linear model of the spruce data
spruce.lm = lm(Height~BHDiameter, data = spruce)

windows()
# Re-create the scatterplot and overlay the trendline on top of it
plot(Height~BHDiameter, main = "Spruce Scatter Plot with Trendline", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)
abline(spruce.lm)

# Task 4
windows()

# Show four plots at once
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))

# Plot 1, the scatter plot with the fitted line
plot(Height~BHDiameter, main = "Plot with Fitted Line", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)
abline(spruce.lm)

# Plot 2, same as 1, but with RSS deviations
plot(Height~BHDiameter, main = "RSS Plot", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)
abline(spruce.lm)

# Predict the values of height from diameter
yhat = fitted(spruce.lm)
# Find residual segments
with(spruce,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})


# Plot 3, the model sum of squares
plot(Height~BHDiameter, main = "MSS Plot", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)
abline(spruce.lm)

# Make a naive model (the mean of heights)
with(spruce, abline(h=mean(Height)))

# Predict the values of height from diameter
yhat = fitted(spruce.lm)

# Make the explained deviations (explained by the model)
with(spruce, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
MSS=with(spruce,sum((yhat-mean(Height))^2))
MSS


# Plot 4, the total sum of squares
plot(Height~BHDiameter, main = "Justin Kleiber's Plot", bg="Blue", pch=21, cex = 1.2, ylim=c(0,1.1*max(Height)), xlim=c(0,1.1*max(BHDiameter)), data=spruce)

# Show the naive model (the mean of heights)
with(spruce, abline(h=mean(Height)))

with(spruce, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
TSS=with(spruce, sum((Height-mean(Height))^2))
TSS


# Task 5

# Summarizing the spruce linear model
summary(spruce.lm)

# Predict height for diameters of 15, 18, and 20 cm
predict(spruce.lm, data.frame(BHDiameter = c(15, 18, 20)))


# Task 6
windows()
library(JKleiberPkg)
plotModel(spruce, spruce$BHDiameter, spruce$Height, spruce$BHDiameter, "Height vs Diameter")

library(ggplot2)
g = ggplot(spruce, aes(x=BHDiameter,y=Height,colour=BHDiameter))
g = g + geom_point() + geom_line() + geom_smooth(method="lm")
g + ggtitle("Height vs BHDiameter")
