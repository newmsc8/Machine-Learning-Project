#load necessary packages
library(rpart)

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
model.type <- args[2] #model type (tree, ada, nb)
response <- args[3] #which response variable are we modeling (NextAdmitDate, NextAdmitCost, or NextDateCost)
file.text <- args[4] #file location for errors to report to
file.rds <- args[5] #dfile location to print out last cross validation's model

#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[6]
}

df <- read.csv(file.name) #read in data
k = 10 #number of folds for cross validation
ret = data.frame() #where we will hold results

#if we're only modeling one response variable, remove the other
if(response == "NextAdmitDate") {
	df$NextAdmitCost<-NULL
	formula = formula("NextAdmitDate~.")
} else if(response == "NextAdmitCost") {
	df$NextAdmitDate<-NULL
	formula = formula("NextAdmitCost~.")
} else {
	df$NextDateCost<-(df$NextAdmitCost*10)+df$NextAdmitDate
	df$NextAdmitCost<-NULL
	df$NextAdmitDate<-NULL
	formula = formula("NextDateCost~.")
}

#shuffle the data
df.shuffle = df[1:nrow(df),]

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds

	cur<-vector()

	#separate train and test data
	df.train = df.shuffle[which(1:nrow(df.shuffle)%%k != i%%k),]
	df.test = df.shuffle[which(1:nrow(df.shuffle)%%k == i%%k),]

	#train model
	model = switch(model.type,
		tree = rpart(formula, df.train, control=rpart.control(cp=cp)),
		ada = adaboost.M1(formula=formula, data=df.train),
		nb = naiveBayes(formula, data=df.train))
	#use model to predict values for test data
	p = switch(model.type,
		tree = predict(model, newdata=df.test, type='prob')[,2],
		ada = predict(model, newdata=df.test, type='prob')[,2],
		nb = predict(model, newdata=df.test,type='raw')[,2])

	#results matrix has actual values in column 1 and predicted values in column 2
	cur = cbind(df.test$response, p)
	ret = rbind(ret, cur)
}



	

