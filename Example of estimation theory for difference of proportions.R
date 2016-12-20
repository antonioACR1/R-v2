#Estimation theory

# Example 1: Difference of proportions. Two simple random samples are drawn from boys and girls from a country. The first sample is 400 boys and the second sample is 300 girls.
# 40% of boys from the first sample claim that Superman is their main hero, and 30% of girls claim that Superman is their main hero.

# Null hypothesis: the population proportion of boys who choose Superman as main hero is equal to the population proportion of girls who choose Superman as main hero.

#The sample statistic is 0.4 - 0.3 = 0.1, is this difference statistically significant? I will choose a confidence level equal to 90%.

# To find the confidence interval, I use prop.test()

model1 <- prop.test(c(160,90),c(400,300),alternative="two.sided",conf.level=0.90,correct=FALSE)
model1

#Results:
## data:  c(160, 90) out of c(400, 300)
## X-squared = 7.4667, df = 1, p-value = 0.006285
## alternative hypothesis: two.sided
## 90 percent confidence interval:
##  0.04069396 0.15930604
## sample estimates:
## prop 1 prop 2 
##    0.4    0.3

# Conclusion: I'm 90% confident that the difference of the actual proportions of nationwide boys who prefer Superman and nationwide girls who prefer Superman lies in 
# the interval (0.04,0.15). This difference is positive, so I conclude that more boys than girls prefer Superman.