#In this simple code I try to use neural network to predict default loans based on the status of previous payments from previous months.
#This code is not good at all. It fails for roughly 15% of my testing set, however I decided to upload it because it's the first time I use neural networks.
 
#Open library after installing it.
library("neuralnet")

#open file
df <- read.csv(file="~/default.csv", header = TRUE, stringsAsFactors=FALSE, skip = 1, sep=";")
head(df)
str(df)

#define my train and test sets.
train <- df[1:24000,]
test <- df[24001:30000,]

#apply neural network. The columns from 'PAY_0' to 'PAY_6' represent the payment status between April 2005 and September 2005. The values vary from -1, to 1,2,..,9.
#-1 means pay duly, 1 means payment delay for 1 months, and so on until 9 which means payment delay for 9 months or more.
#the response variable is default.payment.next month with binary value 1 (default) or 0 (not default). 
model <- neuralnet(default.payment.next.month ~ PAY_0 + PAY_2 + PAY_3 + PAY_4 +PAY_5 + PAY_6, train, hidden=4, lifesign="minimal", linear.output=FALSE, threshold=0.1)

#plot my neural network
plot(model, rep="best")

#subset the data which I want to predict using my previous model
sample_test <- subset(test, select=c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6"))
head(sample_test)

#predict
model_result <- compute(model, sample_test)
str(model_result)

#compare my prediction results with the actual values
comparison_table <- data.frame(actual=test$default.payment.next.month,prediction=model_result$net.result)
head(comparison_table)

#round my prediction probability values to see if they match the actual result
comparison_table$prediction <- round(comparison_table$prediction)
comparison_table
str(comparison_table)

#count how many of my predictions are correct and how many are not
comparison_table$comparison <- ifelse(comparison_table$actual == comparison_table$prediction,1,0)
comparison_table
str(comparison_table)
table(comparison_table$comparison)

#So from 6000 samples from my test dataset, I failed to predict 1009 but I succeeded in 4991 of them.
#My model is not good but it's a start...
