
# A - scattergram
seeds = read.csv("SEEDGERM.csv")
plot(seeds$CHANGE~seeds$TEMP)


# B - least squares prediction equation
seeds.lm = lm(seeds$CHANGE~seeds$TEMP)
summary(seeds.lm)
# Equation is y = 78.516 - 0.2389x

# C - best fit line
abline(seeds.lm$coefficients[1], seeds.lm$coefficients[2])


# D - Model adequacy test
summary(seeds.lm)
# From the summary we can determine that beta0 is NOT adequate 
# because the p-value is bigger than alpha

# E - Outliers
# From C, it looks like the data point above 300 deg K is an outlier.
#We can examine this using cooks20x
library(s20x)
cooks20x(seeds.lm)

# F - Removing outliers
seeds = seeds[-5, ]
seeds

# The outlier is removed, so let's redo the analysis
plot(seeds$CHANGE~seeds$TEMP)
seeds.lm2 = lm(seeds$CHANGE~seeds$TEMP)
summary(seeds.lm2)
abline(seeds.lm2$coefficients[1], seeds.lm2$coefficients[2], col = "Red")

# From this we can tell the model is adequate after removing the outlier
# because the p value for beta 0 is less than alpha = 0.01
