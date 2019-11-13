# 1
phish = read.csv("PHISHING.csv")
length(phish[phish$INTTIME >= 120, ]) / length(phish$INTTIME)
head(phish)


set.seed(25)
n=3
alpha = 0.05
x = c(3, 4, 5)
t = qt(1-alpha/2, n - 1)
mp = c(-1, 1)
ci = mean(x) + mp*t*sd(x)/sqrt(n)
ci

t.test(x, conf.level = 1 - alpha)$conf.int

#15
solar = read.csv("SOLARAD.csv")
solar$STJOS - solar$IOWA

#16
diazinon = read.csv("DIAZINON.csv")



read.csv(file.choose())-> data
data$REAC.R
t.test(data$REAC.R,data$REAC.U)$conf
