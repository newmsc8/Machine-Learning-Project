#load necessary packages
library(rpart)
library(e1071)

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
model.type <- args[2] #model type (tree, svm, nb)
response <- args[3] #which response variable are we modeling (CostLabel, RiskLabel, or RiskCost)

message('file name: ',file.name,' -- model type: ',model.type,' -- response: ',response)
#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[4]
	message('cp: ',cp)
}


df <- read.csv(file.name) #read in data
k = 10 #number of folds for cross validation
ret = data.frame() #where we will hold results

#remove VisitLink variable (this is only an ID)
df$VisitLink<-NULL
message('removed visit link')

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
message('set response variable, remove others')

#shuffle the data
df.shuffle = df[1:nrow(df),]
message('shuffle')

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds

	cur<-vector()
	message(i)

	#separate train and test data
	df.train = df.shuffle[which(1:nrow(df.shuffle)%%k != i%%k),]
	df.test = df.shuffle[which(1:nrow(df.shuffle)%%k == i%%k),]

	#train model
	model = switch(model.type,
		tree = rpart(formula, df.train, control=rpart.control(cp=cp)),
		svm = svm(formula=formula, data=df.train),
		nb = naiveBayes(formula, data=df.train))
	message('model made')
	#use model to predict values for test data
	p = switch(model.type,
		tree = predict(model, newdata=df.test,type='class'),
		svm = predict(model, newdata=df.test,type='class'),
		nb = predict(model, newdata=df.test),type='class')
	message('prediction done')

	#results matrix has actual values in column 1 and predicted values in column 2
	if(response == "RiskLabel") {
		cur = cbind(df.test$RiskLabel, p)
	} else if(response == "CostLabel") {
		cur = cbind(df.test$CostLabel, p)
	} else {
		cur = cbind(df.test$RiskCost, p)
	}
	ret = rbind(ret, cur)
	message('ret made')
}

#the total number of people predicted correctly by the model
correct=ret[ret[,1]==ret[,2],]
#the accuracy = (total number predicted correctly)/(total number in file)
message('accuracy: ', nrow(correct)/nrow(df.shuffle))

#calculate the precision of each class
totalp = 0 #running precision total
numc = 3 #stores the number of classes (used in average)
if((response=='RiskLabel')||(response=='CostLabel')) {
	predict = ret[ret[,2]==1,]
	true = predict[predict[,1]==1,]
	#precision of class 1
	message('precision 1: ',nrow(true)/nrow(predict))
	totalp = totalp+(nrow(true)/nrow(predict))
	predict = ret[ret[,2]==2,]
	true = predict[predict[,1]==2,]
	#precision of class 2
	message('precision 2: ',nrow(true)/nrow(predict))
	totalp = totalp+(nrow(true)/nrow(predict))
	predict = ret[ret[,2]==3,]
	true = predict[predict[,1]==3,]
	#precision of class 3
	message('precision 3: ',nrow(true)/nrow(predict))
	totalp = totalp+(nrow(true)/nrow(predict))
} else {
	for(i in 1:9) {
		#for LP create loop
		predict = ret[ret[,2]==i,]
		true = predict[predict[,1]==i,]
		message('precision ',i, ': ',nrow(true)/nrow(predict))
		totalp = totalp+(nrow(true)/nrow(predict))
	}
	numc=9
}
#report average precision
message('average precision: ',totalp/numc)

#running recall total
totalr = 0
if((response=='RiskLabel')||(response=='CostLabel')) {
	actual = ret[ret[,1]==1,]
	true = actual[actual[,2]==1,]
	#recall of class 1
	message('recall 1: ',nrow(true)/nrow(actual))
	totalr = totalr+(nrow(true)/nrow(actual))
	actual = ret[ret[,1]==2,]
	true = actual[actual[,2]==2,]
	#recall of class 2
	message('recall 2: ',nrow(true)/nrow(actual))
	totalr = totalr+(nrow(true)/nrow(actual))
	actual = ret[ret[,1]==3,]
	true = actual[actual[,2]==3,]
	#recall of class 3
	message('precision 3: ',nrow(true)/nrow(actual))
	totalr = totalr+(nrow(true)/nrow(actual))
} else {
	for(i in 1:9) {
		#for LP create loop
		actual = ret[ret[,1]==i,]
		true = actual[actual[,2]==i,]
		message('precision ',i, ': ',nrow(true)/nrow(actual))
		totalr = totalr+(nrow(true)/nrow(actual))
	}
}
#report average recall
message('average recall: ',totalr/numc)

#report F1 measure using average recall and precision
message('f-measure: ',(2*(totalp/numc)*(totalr/numc))/((totalp/numc)+(totalr/numc)))

#report counts so we can identify who 1,2,3(,4,5,6,7,8,9) are
message('figure out who is who: ')
if(response=='CostLabel') {
	message('number low: ',nrow(df.shuffle[df.shuffle$CostLabel=='low',]))
	message('number medium: ',nrow(df.shuffle[df.shuffle$CostLabel=='medium',]))
	message('number high: ',nrow(df.shuffle[df.shuffle$CostLabel=='high',]))
	message('number 1: ',nrow(ret[ret[,1]==1,]))
	message('number 2: ',nrow(ret[ret[,1]==2,]))
	message('number 3: ',nrow(ret[ret[,1]==3,]))
} else if(response=='RiskLabel') {
	message('number 30days: ',nrow(df.shuffle[df.shuffle$CostLabel=='30days',]))
	message('number 60days: ',nrow(df.shuffle[df.shuffle$CostLabel=='60days',]))
	message('number 90days: ',nrow(df.shuffle[df.shuffle$CostLabel=='90days',]))
	message('number 1: ',nrow(ret[ret[,1]==1,]))
	message('number 2: ',nrow(ret[ret[,1]==2,]))
	message('number 3: ',nrow(ret[ret[,1]==3,]))
} else {
	message('number low 30days: ',nrow(df.shuffle[df.shuffle$CostLabel=='low 30days',]))
	message('number low 60days: ',nrow(df.shuffle[df.shuffle$CostLabel=='low 60days',]))
	message('number low 90days: ',nrow(df.shuffle[df.shuffle$CostLabel=='low 90days',]))
	message('number medium 30days: ',nrow(df.shuffle[df.shuffle$CostLabel=='medium 30days',]))
	message('number medium 60days: ',nrow(df.shuffle[df.shuffle$CostLabel=='medium 60days',]))
	message('number medium 90days: ',nrow(df.shuffle[df.shuffle$CostLabel=='medium 90days',]))
	message('number high 30days: ',nrow(df.shuffle[df.shuffle$CostLabel=='high 30days',]))
	message('number high 60days: ',nrow(df.shuffle[df.shuffle$CostLabel=='high 60days',]))
	message('number high 90days: ',nrow(df.shuffle[df.shuffle$CostLabel=='high 90days',]))
	message('number 1: ',nrow(ret[ret[,1]==1,]))
	message('number 2: ',nrow(ret[ret[,1]==2,]))
	message('number 3: ',nrow(ret[ret[,1]==3,]))
	message('number 4: ',nrow(ret[ret[,1]==4,]))
	message('number 5: ',nrow(ret[ret[,1]==5,]))
	message('number 6: ',nrow(ret[ret[,1]==6,]))
	message('number 7: ',nrow(ret[ret[,1]==7,]))
	message('number 8: ',nrow(ret[ret[,1]==8,]))
	message('number 9: ',nrow(ret[ret[,1]==9,]))
}

save.image()	

