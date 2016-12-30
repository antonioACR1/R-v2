#My objective is to apply a generalized linear model glm() to a dataset containing information about people applying for a loan
#Among the features of the dataset, there are two which I want to use as explanatory variables, 'annual_inc' (annual income) and 'inq_last_6mths' (number of inquiries in the last 6 months)

#PART O: Arrange my data and then split my data. Also fit my model using glm()

#read file and have a look

df <- read.csv(file="~/LoanStats3a.csv", header = TRUE, stringsAsFactors=FALSE, skip = 1)
head(df)

#too many columns with NA, delete those having at least 80% NA’s just to have a better view of the data
poor_coverage <- sapply(df, function(x){
   coverage <- 1-sum(is.na(x))/length(x)
   coverage < 0.8}
)

df <- df[,poor_coverage==FALSE]
head(df)

#now get rid of columns that I consider not important 

#description 
df[,'desc']<-NULL
#website
df[,'url']<- NULL
#zip code
df[,'zip_code']<-NULL
#address
df[,'addr_state']<-NULL
#purpose of loan
df[,'purpose']<-NULL
#id
df[,'member_id']<- NULL
#job title
df[,'title']<-NULL

#check again
head(df)
#it looks better for analysis

#now check loan_status values

unique(df$loan_status)
##> unique(df$loan_status)
## [1] "Fully Paid"                                         
## [2] "Charged Off"                                        
## [3] "Current"                                            
## [4] "In Grace Period"                                    
## [5] "Late (16-30 days)"                                  
## [6] "Late (31-120 days)"                                 
## [7] "Default"                                            
## [8] ""                                                   
## [9] "Does not meet the credit policy. Status:Fully Paid" 
##[10] "Does not meet the credit policy. Status:Charged Off"

#I will decide which ones I consider bad indicators and create new column indicating this (1 is bad, 0 is not bad)

bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off", "Does not meet the credit policy. Status:Charged Off")
df$is_bad <- ifelse(df$loan_status %in% bad_indicators, 1, ifelse(df$loan_status=="", NA, 0))

#check again
head(df) 

#Now I would like to perform logistic regression where the explanatory variables are 'annual_inc' (annual income) and 'inq_last_6mths' (number of inquiries
#in the last 6 months) and the response variable is 'is_bad' (good or bad applicants, 0 for good and 1 for bad).

#Subset my data according to what I want to do
df1 <- df[,c('id','annual_inc','inq_last_6mths','is_bad')]
#check na's
sum(is.na(df1$annual_inc))
#seven na's in 'annual_inc' column

sum(is.na(df1$inq_last_6mths))
#thirty-two na's in 'inq_last_6mths' column

#omit rows having na's
df1 <- na.omit(df1)
head(df1)

##> head(df1)
##       id annual_inc inq_last_6mths is_bad
##1 1077501      24000              1      0
##2 1077430      30000              5      1
##3 1077175      12252              2      0
##4 1076863      49200              1      0
##5 1075358      80000              0      0
##6 1075269      36000              3      0


#check types
str(df1)
#in the previous line, types are OK

#check summary
summary(df1)


#I will choose a random sample df2 of size 500. I want to apply logistic regression to both df2 (viewed as simple random sampling) and df1 (viewed as the entire population)
df2 <- df[,c('annual_inc','inq_last_6mths','is_bad')]
df2 <- df2[sample(nrow(df2), size=500,replace=FALSE, prob=NULL),]
head(df2)
##> head(df2)
##      annual_inc inq_last_6mths is_bad
##1415      100000              0      0
##7852      160000              0      1
##23955     125000              1      0
##15519      94000              1      0
##7173       45000              3      1
##11649      70000              0      0


#I will work with both df1 and df2 

#split df1 into train and test. Use 75% of my dataset for training 

sample.size <- floor(0.75 * nrow(df1))
#set the seed to make your partition reproductible
set.seed(123)
training <- sample(seq_len(nrow(df1)), size = sample.size)
#define train and test
train <- df1[training, ]
test <- df1[-training, ]

#now use glm() applied to train and df2
model.full <- glm(is_bad ~ annual_inc + inq_last_6mths,data=train,family=binomial)
model.sample <- glm(is_bad ~ annual_inc + inq_last_6mths, data=df2, family=binomial)

#in the two previous lines I used binomial because I'm solving a classification problem and binomial is suggested for these kind of problems

PART 1: Work with model.full 

#Step 1: Use likelihood ratio test to compare my full model against reduced models


#first define two reduced models 

#in the first one, remove the parameter 'inq_last_6mths' from my full model

model.reduced1 <- glm(is_bad ~ annual_inc, data=train, family = binomial)

model.reduced1
##> model.reduced1
##Call:  glm(formula = is_bad ~ annual_inc, family = binomial, data = train)
##Coefficients:
##(Intercept)   annual_inc  
## -1.486e+00   -3.338e-06  
##Degrees of Freedom: 31878 Total (i.e. Null);  31877 Residual
##Null Deviance:      27340 
##Residual Deviance: 27260        AIC: 27260


#in the second one remove the parameter 'annual_inc' instead
model.reduced2 <- glm(is_bad ~ inq_last_6mths, data=train, family=binomial)
model.reduced2 
##> model.reduced2 
##Call:  glm(formula = is_bad ~ inq_last_6mths, family = binomial, data = train)
##Coefficients:
##   (Intercept)  inq_last_6mths  
##       -1.9028          0.1613  
##Degrees of Freedom: 31878 Total (i.e. Null);  31877 Residual
##Null Deviance:      27340 
##Residual Deviance: 27020        AIC: 27020

#Now I use the likelihood ration test to compare my full model against my two reduced models.
#For this purpose, I use anova() with the option test="Chisq"

anova(model.reduced2, model.full, test="Chisq")

##> anova(model.reduced2, model.full, test="Chisq")
##Analysis of Deviance Table
##Model 1: is_bad ~ inq_last_6mths
##Model 2: is_bad ~ annual_inc + inq_last_6mths
##  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
##1     31877      27019                          
##2     31876      26924  1   94.779 < 2.2e-16 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#The p-value is less than 0.05, so we reject the null hypothesis. 
#Intuitively, the null hypothesis says that removing the variable 'inq_last_6mths' will not affect my full model
#Since we can reject the null hypothesis, we conclude that removing the variable 'inq_last_6mths will' will affect negatively to my full model

anova(model.reduced1, model.full, test="Chisq")
##> anova(model.reduced1, model.full, test="Chisq")
##Analysis of Deviance Table
##Model 1: is_bad ~ annual_inc
##Model 2: is_bad ~ annual_inc + inq_last_6mths
##  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
##1     31877      27255                          
##2     31876      26924  1   331.28 < 2.2e-16 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The p-value is less than 0.05, so similarly we conclude that removing the variable 'annual_inc' from my full model will affect my full model in a negative way


#Step 2: Use Wald's test to get the same conclusion as in Step 1

library(survey)
#Wald's test
regTermTest(model.full,"annual_inc")

##> regTermTest(model.full,"annual_inc")
##Wald test for annual_inc
## in glm(formula = is_bad ~ annual_inc + inq_last_6mths, family = binomial, 
##    data = train)
##F =  79.1834  on  1  and  31876  df: p= < 2.22e-16 

#the conclusion is the same as in step 1

#Wald's test again
regTermTest(model.full,"inq_last_6mths")
##> regTermTest(model.full,"inq_last_6mths")
##Wald test for inq_last_6mths
## in glm(formula = is_bad ~ annual_inc + inq_last_6mths, family = binomial, 
##    data = train)
##F =  333.9903  on  1  and  31876  df: p= < 2.22e-16 

#the conclusion isthe sameasin step 1

#Step 3: Check variable importance for my full model

library(caret)

varImp(model.full)
##> varImp(model.full)
##                 Overall
##annual_inc      8.898505
##inq_last_6mths 18.275401

#the number of inquiries seems to be more important than the annual income


#Step 4: Prediction and testing my results using ROC curve

#I will check how good my model is by predicting the classification for my tet data
prediction <- predict(model.full, newdata=test, type="response")
#
prediction <- data.frame(prediction)
#round my prediction probabilities
predicted.values <- round(prediction$prediction)
#compare predicted values with actual values
comparison<-data.frame(predicted.values, test[,"is_bad"])
#put names
colnames(comparison)=c('predicted','actual')
head(comparison)

accuracy <- table(comparison)

##> accuracy
##         actual
##predicted    0    1
##        0 9087 1527
##        1    7    6

#the accuracy of our prediction is:
sum(diag(accuracy))/sum(accuracy)
##> sum(diag(accuracy))/sum(accuracy)
##[1] 0.8556507


#another way
confusionMatrix(data=predicted.values, test$is_bad)

#now the ROC curve

library(ROCR)
# Compute AUC (area under the curve) for predicting 'is_bad' with the model 'model.full'
prob <- predict(model.full, newdata=test, type="response")
pred <- prediction(prob, test$is_bad)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(perf)
abline(a=0,b=1)
#the further away from the diagonal line is, the better. In my case, the ROC curve suggests that I still need to improve my model

#the following code is to get the area under the ROC curve
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
##> auc
##[1] 0.5962469
#in general, we want this area to be > 0.8 

PART 2: Work with model.sample

#Step 1: Compare model.sample with a reduced model

model.sample
##> model.sample
##Call:  glm(formula = is_bad ~ annual_inc + inq_last_6mths, family = binomial, 
##    data = df2)
##Coefficients:
##   (Intercept)      annual_inc  inq_last_6mths  
##    -2.042e+00      -1.191e-06       2.915e-01  
##Degrees of Freedom: 499 Total (i.e. Null);  497 Residual
##Null Deviance:      412.2 
##Residual Deviance: 400.9        AIC: 406.9

#define a new reduced model containing only the intercept term

model.sample.reduced <- glm(is_bad ~ 1, data=df2, family=binomial)
model.sample.reduced
##> model.sample.reduced
##Call:  glm(formula = is_bad ~ 1, family = binomial, data = df2)
##Coefficients:
##(Intercept)  
##     -1.782  
##Degrees of Freedom: 499 Total (i.e. Null);  499 Residual
##Null Deviance:      412.2 
##Residual Deviance: 412.2        AIC: 414.2

anova(model.sample, model.sample.reduced, test="Chisq")
##> anova(model.sample, model.sample.reduced, test="Chisq")
##Analysis of Deviance Table
##Model 1: is_bad ~ annual_inc + inq_last_6mths
##Model 2: is_bad ~ 1
##  Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
##1       497     400.86                        
##2       499     412.16 -2  -11.296 0.003524 **
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#intuitively, the null hypothesis says that my model.sample will not be affected after removing the variables 'inq_last_6mths' and 'annual_inc'
#rejecting the null hypothesis means that this pair of variables is important to classify whether or not an applicant will default the requested loan
#the p-value is less than 0.05, so reject the null hypothesis

#Step 2: Perform tests on individual parameters

summary(model.sample)
##> summary(model.sample)
##Call:
##glm(formula = is_bad ~ annual_inc + inq_last_6mths, family = binomial, 
##    data = df2)
##Deviance Residuals: 
##    Min       1Q   Median       3Q      Max  
##-1.2791  -0.5568  -0.4843  -0.4716   2.2784  
##Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)    
##(Intercept)    -2.042e+00  2.526e-01  -8.082 6.36e-16 ***
##annual_inc     -1.191e-06  2.769e-06  -0.430 0.667116    
##inq_last_6mths  2.915e-01  8.649e-02   3.371 0.000749 ***
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##(Dispersion parameter for binomial family taken to be 1)
##    Null deviance: 412.16  on 499  degrees of freedom
##Residual deviance: 400.86  on 497  degrees of freedom
##AIC: 406.86
##Number of Fisher Scoring iterations: 6

#the p-value corresponding to annual_inc is more than 0.05. Therefore, this variable does not appear to have a significant impact on the probability of defaulting a loan, once the variable inq_last_6mths is included in the model
#the p-value corresponding to inq_last_6mths is less than 0.05, so this variable appears to have a significant impact on the probability of defaulting a loan, once the variable annual_inc is included in the model

#Step 3: Prediction

exp(coef(model.sample))
##> exp(coef(model.sample))
##   (Intercept)     annual_inc inq_last_6mths 
##     0.1298228      0.9999988      1.3384884 


#In particular, if we fix the variable annual_inc, then increasing the number of inquiries in the last 6 months by one will increase the odds of defaulting a loan by 33%

#the following code will get 95% confidence intervals
exp(confint.default(model.sample))
##> exp(confint.default(model.sample))
##                   2.5 %    97.5 %
##(Intercept)    0.0791287 0.2129941
##annual_inc     0.9999934 1.0000042
##inq_last_6mths 1.1297827 1.5857485

#therefore, we are 95% confident that if we increase by one the number of inquiries in the last 6 months then the probability of defaulting a loan will increase between %12 and 58%

#Step 4: Predicting new applicants

#suppose I have a new applicant with annual income equal to $10560.00 and number of inquiries in the last 6 months equal to 8
pi.hat = predict.glm(model.sample, data.frame(annual_inc = 10560.00, inq_last_6mths = 8), type = "response", se.fit=TRUE)
#the probability is:
exp(pi.hat$fit)/(1+exp(pi.hat$fit))
##> exp(pi.hat$fit)/(1+exp(pi.hat$fit))
##        1 
##0.6385546 

##i.e., the predicted (approximated) probability of default is 63%

#get 95% confidence interval
confidence.interval <- c(pi.hat$fit-1.96*pi.hat$se.fit,pi.hat$fit+1.96*pi.hat$se.fit)
#transform to probability type
exp(confidence.interval)/(1+exp(confidence.interval))
##> exp(confidence.interval)/(1+exp(confidence.interval))
##        1         1 
##0.5690230 0.7027289 

#therefore we are 95% confident that the actual probability that this person will default a loan is between 56% and 70%









