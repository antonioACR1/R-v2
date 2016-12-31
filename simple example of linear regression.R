#The dataset is about information of 19 hotels. Information includes annual energy consumption, area in square meters, number of rooms,etc.
#Suppose these hotels represent a simple random sample of hotels in a study to see if there is a relationship between energy consumption and area in square meters
#Is there a significant relationship between energy consumption and area? I will use a 0.05 level of significance

#read the file
df<-read.csv(file="~/hotel_energy.csv",header=TRUE)
head(df)
##> head(df)
##  hotel   enrgcons    area age numrooms occrate effrooms
##1     1  1,953,916  43,000   6      420  32.60%   136.92
##2     2  1,045,555  19,979  16      215  63.00%   135.45
##3     3  4,245,313  46,529   7      273  65.05%   177.59
##4     4  2,126,199  20,962   6      222  70.50%   156.51
##5     5  2,785,958  24,212   5      474  69.70%   330.38
##6     6 13,833,968 112,200   4      787  48.97%   385.39

#check types
str(df)

#enrgcons and area are factors, so convert them to numeric and use gsub() to avoid the "," to be interpreted as decimal
x=as.numeric(gsub(",","",df$area))
y=as.numeric(gsub(",","",df$enrgcons))

#check na's
sum(is.na(df$enrgcons))
sum(is.na(df$area))
#ok

plot(x,y,xlab="area in square meters",ylab="energy consumption",main="scatter plot",col="red",pch=16)

#get correlation coefficient
cor(x,y)
##> cor(x,y)
##[1] 0.8789621


#apply lm()
model <- lm(x~y)



#residual plot
residual <- resid(model)
plot(y,residual,xlab="energy",ylab="residuals",main="residual plot",col="red",pch=16)
abline(0,0)

#check details of my model
summary(model)
##> summary(model)
##Call:
##lm(formula = x ~ y)
##Residuals:
##   Min     1Q Median     3Q    Max 
##-14453  -5587  -2462   2351  31929 
##Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
##(Intercept) 1.316e+04  5.119e+03   2.571   0.0198 *  
##y           4.851e-03  6.384e-04   7.599 7.29e-07 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##Residual standard error: 11600 on 17 degrees of freedom
##Multiple R-squared:  0.7726,    Adjusted R-squared:  0.7592 
##F-statistic: 57.75 on 1 and 17 DF,  p-value: 7.294e-07

#the null hypothesis says that the slope of the regression line corresponding to the annual enery consumption and area of the population hotels is zero
#in other words, the null hypothesis says that there is no relationship between energy consumption and area
#the p-value is less than 0.05, so we reject the null hypothesis
#therefore, there is a relationship between area and energy consumption of the entire population of hotels


#now a simple prediction
#suppose we have a hotel having area equal to 25000 square meters
#what is the expected annual energy consumption for this hotel?

predict(model, new=data.frame(y=c(25000)))
#the predicted annual energy consumption is
##> predict(model, new=data.frame(y=c(25000)))
##       1 
##13280.22 

