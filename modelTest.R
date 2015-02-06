#load necessary packages
library(rpart)
library(e1071)

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
model.type <- args[2] #model type (tree, ada, nb)
response <- args[3] #which response variable are we modeling (CostLabel, RiskLabel, or RiskCost)
file.text <- args[4] #file location for errors to report to
file.rds <- args[5] #dfile location to print out last cross validation's model

#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[6]
}

df <- read.csv(file.name) #read in data
k = 10 #number of folds for cross validation
ret = data.frame() #where we will hold results

#remove VisitLink variable (this is only an ID)
df$VisitLink<-NULL

#if we're only modeling one response variable, remove the other
if(response == "RiskLabel") {
	df$CostLabel<-NULL
	formula = formula("RiskLabel~.")
} else if(response == "CostLabel") {
	df$RiskLabel<-NULL
	formula = formula("CostLabel~.")
} else {
	df$RiskCost<-paste(df$CostLabel,df$RiskLabel)
	df$CostLabel<-NULL
	df$RiskLabel<-NULL
	formula = formula("RiskCost~.")
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
		svm = svm(formula=formula, data=df.train),
		nb = naiveBayes(formula, data=df.train))
	#use model to predict values for test data
	p = switch(model.type,
		tree = predict(model, newdata=df.test,type='class'),
		svm = predict(model, newdata=df.test,type='class'),
		nb = predict(model, newdata=df.test),type='class')

	#results matrix has actual values in column 1 and predicted values in column 2
	cur = cbind(df.test$response, p)
	ret = rbind(ret, cur)
}
low30
med30
high30
lo

correct=ret[ret[,1]==ret[,2],]
message('accuracy: ', nrow(correct)/nrow(df.shuffle))
totalp = 0
numc = 3
if((response==RiskLabel)||(response==CostLabel)) {
	predict = ret[ret[,2]==1,]
	true = predict[predict[,1]==1,]
	message('precision 1: ',true/predict)
	totalp = totalp+(true/predict)
	predict = ret[ret[,2]==2,]
	true = predict[predict[,1]==2,]
	message('precision 2: ',true/predict)
	totalp = totalp+(true/predict)
	predict = ret[ret[,2]==3,]
	true = predict[predict[,1]==3,]
	message('precision 3: ',true/predict)
	totalp = totalp+(true/predict)
} else {
	for(i in 1:9) {
		predict = ret[ret[,2]==i,]
		true = predict[predict[,1]==i,]
		message('precision ',i, ': ',true/predict)
		totalp = totalp+(true/predict)
	}
	numc=9
}
message('average precision: ',totalp/numc)

totalr = 0
if((response==RiskLabel)||(response==CostLabel)) {
	actual = ret[ret[,1]==1,]
	true = actual[actual[,2]==1,]
	message('recall 1: ',true/actual)
	totalr = totalr+(true/actual)
	actual = ret[ret[,1]==2,]
	true = actual[actual[,2]==2,]
	message('recall 2: ',true/actual)
	totalr = totalr+(true/actual)
	actual = ret[ret[,1]==3,]
	true = actual[actual[,2]==3,]
	message('precision 3: ',true/actual)
	totalr = totalr+(true/actual)
} else {
	for(i in 1:9) {
		actual = ret[ret[,1]==i,]
		true = actual[actual[,2]==i,]
		message('precision ',i, ': ',true/actual)
		totalr = totalr+(true/actual)
	}
}
message('average recall: ',totalr/numc)




	

