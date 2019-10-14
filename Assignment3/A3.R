# 1
phish = read.csv("PHISHING.csv")
length(phish[phish$INTTIME >= 120, ]) / length(phish$INTTIME)
head(phish)
