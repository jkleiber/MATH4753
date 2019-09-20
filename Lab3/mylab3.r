# Task 2
spruce = read.csv("SPRUCE.csv")
head(spruce)

# Task 3
# Plot the spruce tree data appropriately
plot(HEAT~RATIO, main = "Justin Kleiber's Spruce Scatter Plot", bg="Blue", pch=21, ylim=c(0,max(HEAT)), xlim=c(0,3), data=spruce)