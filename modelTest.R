#load necessary packages
library(rpart)
library(e1071)
library(caret)

printMetrics<-function(ret) {
	#the total number of people predicted correctly by the model
	correct=ret[ret[,1]==ret[,2],]
	#the accuracy = (total number predicted correctly)/(total number in file)
	message('accuracy: ', nrow(correct)/nrow(ret))

	numc = 3 #stores the number of classes (used in average)
	if(response=='ReadmitAndCostBucket') {
		numc=9
	}

	#calculate the precision of each class
	totalp = 0 #running precision total
	for(i in 1:numc) {
		#for LP create loop
		predict = ret[ret[,2]==i,,drop=FALSE]
		true = predict[predict[,1]==i,,drop=FALSE]
		if(nrow(predict)!=0) {
			message('precision ',i,': ',nrow(true)/nrow(predict))
			totalp = totalp+(nrow(true)/nrow(predict))
		} else {
			message('precision ',i,': 0')
		}
	}
	#report average precision
	message('average precision: ',totalp/numc)

	#calculate the recall of each class
	totalr = 0 #running recall total
	for(i in 1:numc) {
		#for LP create loop
		actual = ret[ret[,1]==i,,drop=FALSE]
		true = actual[actual[,2]==i,,drop=FALSE]
		if(nrow(actual)!=0) {
			message('recall ',i,': ',nrow(true)/nrow(actual))
			totalr = totalr+(nrow(true)/nrow(actual))
		} else {
			message('recall ',i,': 0')
		}
	}
	#report average recall
	message('average recall: ',totalr/numc)

	#report F1 measure using average recall and precision
	message('f-measure: ',(2*(totalp/numc)*(totalr/numc))/((totalp/numc)+(totalr/numc)))
}

printTotals<-function(ret,df.shuffle) {
	#report counts so we can identify who 1,2,3(,4,5,6,7,8,9) are
	message('figure out who is who: ')
	if(response=='CostBucket') {
		message('number low: ',nrow(df.shuffle[df.shuffle$CostBucket=='low',,drop=FALSE]))
		message('number medium: ',nrow(df.shuffle[df.shuffle$CostBucket=='medium',,drop=FALSE]))
		message('number high: ',nrow(df.shuffle[df.shuffle$CostBucket=='high',,drop=FALSE]))
		message('number 1: ',nrow(ret[ret[,1]==1,,drop=FALSE]))
		message('number 2: ',nrow(ret[ret[,1]==2,,drop=FALSE]))
		message('number 3: ',nrow(ret[ret[,1]==3,,drop=FALSE]))
	} else if(response=='ReadmitBucket') {
		message('number 30days: ',nrow(df.shuffle[df.shuffle$ReadmitBucket=='30days',,drop=FALSE]))
		message('number 60days: ',nrow(df.shuffle[df.shuffle$ReadmitBucket=='60days',,drop=FALSE]))
		message('number 90days: ',nrow(df.shuffle[df.shuffle$ReadmitBucket=='90days',,drop=FALSE]))
		message('number 1: ',nrow(ret[ret[,1]==1,,drop=FALSE]))
		message('number 2: ',nrow(ret[ret[,1]==2,,drop=FALSE]))
		message('number 3: ',nrow(ret[ret[,1]==3,,drop=FALSE]))
	} else {
		message('number low 30days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='low 30days',,drop=FALSE]))
		message('number low 60days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='low 60days',,drop=FALSE]))
		message('number low 90days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='low 90days',,drop=FALSE]))
		message('number medium 30days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='medium 30days',,drop=FALSE]))
		message('number medium 60days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='medium 60days',,drop=FALSE]))
		message('number medium 90days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='medium 90days',,drop=FALSE]))
		message('number high 30days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='high 30days',,drop=FALSE]))
		message('number high 60days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='high 60days',,drop=FALSE]))
		message('number high 90days: ',nrow(df.shuffle[df.shuffle$ReadmitAndCostBucket=='high 90days',,drop=FALSE]))
		message('number 1: ',nrow(ret[ret[,1]==1,,drop=FALSE]))
		message('number 2: ',nrow(ret[ret[,1]==2,,drop=FALSE]))
		message('number 3: ',nrow(ret[ret[,1]==3,,drop=FALSE]))
		message('number 4: ',nrow(ret[ret[,1]==4,,drop=FALSE]))
		message('number 5: ',nrow(ret[ret[,1]==5,,drop=FALSE]))
		message('number 6: ',nrow(ret[ret[,1]==6,,drop=FALSE]))
		message('number 7: ',nrow(ret[ret[,1]==7,,drop=FALSE]))
		message('number 8: ',nrow(ret[ret[,1]==8,,drop=FALSE]))
		message('number 9: ',nrow(ret[ret[,1]==9,,drop=FALSE]))
	}
}
	

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
model.type <- args[2] #model type (tree, svm, nb)
response <- args[3] #which response variable are we modeling (CostBucket, ReadmitBucket, or ReadmitAndCostBucket)
sample.size<-as.numeric(args[4])

message('file name: ',file.name,' -- model type: ',model.type,' -- response: ',response)
#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[5]
	message('cp: ',cp)
}


df <- read.csv(file.name) #read in data
df$X<-NULL
df$X.1<-NULL
df$ReadmitAndCostBucket <- paste(df$ReadmitBucket,df$CostBucket,sep=" ")
df$ReadmitBucket<-as.factor(df$ReadmitBucket)
df$ReadmitAndCostBucket<-as.factor(df$ReadmitAndCostBucket)
df$CostBucket<-as.factor(df$CostBucket)
k = 10 #number of folds for cross validation
preds<-c() #where we will hold predictions
truths<-c() #where we will hold ground truth
ret = data.frame() #where we will hold preds and truths

#get columns for round 1
#df<-df[,c("DXCCS1", "DRG", "PRCCS1", "AGE", "RACE", "FEMALE", "NCHRONIC", "LOS", "TOTCHG", "CostBucket", "ReadmitBucket","ReadmitAndCostBucket")]

#if we're only modeling one response variable, remove the other
if(response == "ReadmitBucket") {
	df$CostBucket<-NULL
	df$ReadmitAndCostBucket<-NULL
	formula = formula("ReadmitBucket~.")
} else if(response == "CostBucket") {
	df$ReadmitBucket<-NULL
	df$ReadmitAndCostBucket<-NULL
	formula = formula("CostBucket~.")
} else {
	df$CostBucket<-NULL
	df$ReadmitBucket<-NULL
	formula = formula("ReadmitAndCostBucket~.")
}
message('set response variable, remove others')

#if a sample size argument was provided, take a sample of the data of the specified size
if(sample.size>0) {
	df<-df[sample(nrow(df), sample.size),]
	message('sample')
}

#shuffle the data
df.shuffle = df[1:nrow(df),]
message('shuffle')

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds

	#cur<-vector()
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
	print(colnames(df.test))
	p = switch(model.type,
		tree = predict(model, newdata=df.test,type='class'),
		svm = predict(model, newdata=df.test,type='class'),
		nb = predict(model, newdata=data.frame(df.test),type='class'))
	message('prediction done')

	#store predictions in preds and associated ground truths in truths and ensure proper factor levels/labels
	if(response == "ReadmitBucket") {
		preds<-c(preds,p)
		preds<-factor(preds, levels=1:nlevels(df$ReadmitBucket), labels=levels(df$ReadmitBucket))
		truths<-c(truths,df.test$ReadmitBucket)
	  	truths<-factor(truths, levels=1:nlevels(df$ReadmitBucket), labels=levels(df$ReadmitBucket))
		message('stored in preds and truths')
	} else if(response == "CostBucket") {
		preds<-c(preds,p)
		preds<-factor(preds, levels=1:nlevels(df$CostBucket), labels=levels(df$CostBucket))
		truths<-c(truths,df.test$CostBucket)
	  	truths<-factor(truths, levels=1:nlevels(df$CostBucket), labels=levels(df$CostBucket))
		message('stored in preds and truths')
	} else {
		preds<-c(preds,p)
		preds<-factor(preds, levels=1:nlevels(df$ReadmitAndCostBucket), labels=levels(df$ReadmitAndCostBucket))
		truths<-c(truths,df.test$ReadmitAndCostBucket)
  		truths<-factor(truths, levels=1:nlevels(df$ReadmitAndCostBucket), labels=levels(df$ReadmitAndCostBucket))
		message('stored in preds and truths')
	}
}

#aggregate truths and preds
ret=cbind(truths,preds)
message('ret made')

#print metrics
printMetrics(ret)
#print totals to ID who is who
printTotals(ret,df.shuffle)

#print confusion matrix
message('confusion matrix')
confusionMatrix(preds,truths)

#save predictions/truths for future reference if needed
preds.file<-paste(model.type,response,'preds.rds',sep=" ")
truths.file<-paste(model.type,response,'truths.rds',sep=" ")
if(model.type=='tree') {
	preds.file<-paste(cp,preds.file,sep=" ")
	truths.file<-paste(cp,truths.file,sep=" ")
}
saveRDS(preds,file=preds.file)
saveRDS(truths,file=truths.file)	

