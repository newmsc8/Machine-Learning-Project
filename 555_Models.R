#load necessary packages
library(rpart)
library(e1071)
library(caret)

#function to calculate average accuracy (takes vector of predictions, and vector of true responses)
averageAccuracy<-function(x.preds,x.truth) {

	#ensure vectors are strings for comparison
	x.preds<-as.character(x.preds)
	x.truth<-as.character(x.truth)

	#running score for each instance
	score <- 0
	message('scoring averageAccuray')
	for(i in 1:length(x.truth)) {
	
		intersect.labels<-union.labels<-2
		
		#separate risk and cost for both prediction vector and truth vector
		truth.risk<-unlist(strsplit(x.truth[i],' '))[1]
		truth.cost<-unlist(strsplit(x.truth[i],' '))[2]
		pred.risk<-unlist(strsplit(x.preds[i],' '))[1]
		pred.cost<-unlist(strsplit(x.preds[i],' '))[2]
	
		if(truth.risk!=pred.risk) {
			#decrease intersection and increase union for inaccurate readmit prediction
			intersect.labels<-intersect.labels-1
			union.labels<-union.labels+1
		}
    		if(truth.cost!=pred.cost) {
			#decrease intersection and increase union for inaccurate cost prediction
      			intersect.labels<-intersect.labels-1
      			union.labels<-union.labels+1
    		}
	
		#increase running score total with score from this instance (intesection/union)
    		score<-score+(intersect.labels/union.labels)
	}
	#final accuracy	- running total/number of instances
	score/length(x.truth)

}

#function to calculate F1-Measure (takes average recall, and average precision)
f1Measure<-function(averageRecall,averagePrecision) {
	message('scoring f1Measure')
	(2*averagePrecision*averageRecall)/(averagePrecision+averageRecall)
}

#function to calculate average recall (takes vector of predictions, and vector of true responses)
averageRecall<-function(x.preds,x.truth) {
	message('scoring averageRecall')

	#ensure vectors are strings for comparison
	x.preds<-as.character(x.preds)
	x.truth<-as.character(x.truth)

	#column bind vectors to produce matrix where first column is true values and second column is predicted values
	ret = cbind(x.truth,x.preds,sep=' ')

	#vector of all possible classes
	classes<-c('High High', 'High Medium', 'High Low', 'Medium High', 'Medium Medium','Medium Low','Low High', 'Low Medium', 'Low Low')

	#calculate the recall of each class
	totalr = 0 #running recall total
	for(i in 1:length(classes)) {
		#select all instances where the true value was a given class
		actual = ret[ret[,1]==classes[i],,drop=FALSE]
		#select all instances where the true value was a given class and that were accurately predicted
		true = actual[actual[,2]==classes[i],,drop=FALSE]
		if(nrow(actual)!=0) {
			#add percentage of instances where the true value was a given class that were accurately predicted to running total
			message('recall ',i,': ',nrow(true)/nrow(actual))
			totalr = totalr+(nrow(true)/nrow(actual))		
		} else {
			message('recall ',classes[i],': 0')
		}
}
#report average recall (running total/number of classes)
totalr/length(classes)

}

#function to calculate precision (takes vector of predictions, and vector of true responses)
averagePrecision<-function(x.preds,x.truth) {
	message('scoring averagePrecision')

	#ensure vectors are strings for comparison
	x.preds<-as.character(x.preds)
	x.truth<-as.character(x.truth)

	#column bind vectors to produce matrix where first column is true values and second column is predicted values
	ret = cbind(x.truth,x.preds,sep=' ')

	#vector of all possible classes
	classes<-c('High High', 'High Medium', 'High Low', 'Medium High', 'Medium Medium','Medium Low','Low High', 'Low Medium', 'Low Low')
	
	#calculate the precision of each class
	totalp = 0 #running precision total
	for(i in 1:length(classes)) {
		#select all instances where the predicted value was a given class
		predict = ret[ret[,2]==classes[i],,drop=FALSE]
		#select all instances where the predicted value was a given class and that were accurately predicted
		true = predict[predict[,1]==classes[i],,drop=FALSE]
		if(length(predict)!=0) {
			#add percentage of instances where the predicted value was a given class that were accurately predicted to running total
			message('precision ',classes[i],': ',nrow(true)/nrow(predict))
			totalp = totalp+(nrow(true)/nrow(predict))
		} else {
			message('precision ',classes[i],': 0')
		}
	}
	#report average precision (running total/number of classes)
	totalp/length(classes)
}

#read arguments
args <- commandArgs(trailingOnly = TRUE)
file.name <- args[1] #where the data is located
approach <- args[2] #what approach are we using (BR,LP,CC,BL)
model.type <- args[3] #base model type (tree, svm, nb, random, majority)
sample.size <- as.numeric(args[4]) #use 0 for full file

message('file name: ',file.name,' -- appraoch: ',approach,' -- model type: ',model.type,' -- sample size: ',sample.size)
#if the model is a tree, provide complexity parameter (usually 0.01, 0.001, or 0.0005)
if(model.type == "tree") {
	cp <- args[5]
	message('cp: ',cp)
}


df <- read.csv(file.name) #read in data

#get columns for round 1
#df$CostBucket<-as.character(df$CostLabel)
#df[df$CostLabel=='high',]$CostBucket<-'High'
#df[df$CostLabel=='medium',]$CostBucket<-'Medium'
#df[df$CostLabel=='low',]$CostBucket<-'Low'
#df$ReadmitBucket<-as.character(df$RiskLabel)
#df[df$RiskLabel=='90days',]$ReadmitBucket<-'High'
#df[df$RiskLabel=='60days',]$ReadmitBucket<-'Medium'
#df[df$RiskLabel=='30days',]$ReadmitBucket<-'Low'
#df$CostLabel<-NULL
#df$RiskLabel<-NULL
#df<-df[,c("DXCCS_1", "PRCCS_1", "AGE", "RACE", "FEMALE", "NCHRONIC", "LOS", "TOTCHG", "CostBucket", "ReadmitBucket")]

df$X<-NULL #get rid of unnecessary column
df$X.1<-NULL #get rid of unnecessary column
df$ReadmitAndCostBucket <- paste(df$ReadmitBucket,df$CostBucket,sep=" ") #create LP label
df$ReadmitBucket<-as.factor(df$ReadmitBucket) #ensure readmit response variable is a factor
df$ReadmitAndCostBucket<-as.factor(df$ReadmitAndCostBucket) #ensure LP response variable is a factor
df$CostBucket<-as.factor(df$CostBucket) #ensure cost response variable is a factor
df$FEMALE<-as.logical(df$FEMALE) #ensure gender is a boolean
df$RACE<-as.factor(df$RACE) #ensure race is a factor

#ensure that all U-codes are factors
for(i in grep("^U_",colnames(df))) {
	df[,i]<-as.factor(df[,i])
}

#get columns for round 3
df<-df[,c('TOTCHG','CumulativeCost','AGE','LOS','REVCD_921','REVCD_361','REVCD_272','REVCD_391','REVCD_440','DXCCS_249','U_ULTRASOUND','REVCD_402','DXCCS_2','REVCD_302','DXCCS_157','PRCCS_50','DXCCS_109','PRCCS_216','PRCCS_223','PRCCS_54','CostBucket','ReadmitBucket','ReadmitAndCostBucket')]

k = 10 #number of folds for cross validation
preds<-c() #where we will hold predictions
truths<-c() #where we will hold ground truth

#remove unnecessary response variable depending on approach
if((approach == "LP") || (approach=='BL') ) {
	df$CostBucket<-NULL
	df$ReadmitBucket<-NULL
} else {
	df$ReadmitAndCostBucket<-NULL
}
message('remove unnecessary buckets')
message('ncol: ',ncol(df))

#if a sample size argument was provided, take a sample of the data of the specified size
if(sample.size>0) {
	df<-df[sample(nrow(df), sample.size),]
	message('sample')
}

#shuffle the data
df.shuffle = df[1:nrow(df),]
message('shuffle')

#if we are building a naive bayes model, make sure all predictors are of consumable form (numeric or factor)
if(model.type == 'nb') {
	df.shuffle[, -which(sapply(df.shuffle, is.numeric))]<-as.data.frame(lapply((df.shuffle[,-which(sapply(df.shuffle, is.numeric))]), as.factor))
}

for(i in 1:k) { #10 fold cross validation, modify k for fewer folds

	#cur<-vector()
	message(i)

	#separate train and test data
	df.train = df.shuffle[which(1:nrow(df.shuffle)%%k != i%%k),]
	df.test = df.shuffle[which(1:nrow(df.shuffle)%%k == i%%k),]

	#train model
	if(approach == 'LP') {
		#train an LP model
		message('in LP')
		formula = formula('ReadmitAndCostBucket~.')
		model = switch(model.type,
			tree = rpart(formula=formula, data=df.train, control=rpart.control(cp=cp)),
			svm = svm(formula=formula, data=df.train),
			nb = naiveBayes(formula=formula, data=df.train))
		message('model made')
		#use model to predict readmit and cost
		p = switch(model.type,
			tree = predict(model, newdata=df.test,type='class'),
			svm = predict(model, newdata=df.test,type='class'),
			nb = predict(model, newdata=df.test[,-which(names(df.shuffle) %in% c('ReadmitAndCostBucket'))]))
		message('prediction done')
	} else if(approach == 'CC') {
		message('in CC')
		#train a model that classifies cost and uses readmit bucket as a predictor
		c.model = switch(model.type,
			tree = rpart(formula=formula('CostBucket~.'), data=df.train, control=rpart.control(cp=cp)),
			svm = svm(formula=formula('CostBucket~.'), data=df.train),
			nb = naiveBayes(formula=formula('CostBucket~.'), data=df.train))
		message('model made 1')
		#train a model that classifies risk but does NOT use cost bucket as a predictor
		r.model = switch(model.type,
			tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
			svm = svm(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]),
			nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]))
		message('model made 2')
 
  		print("making predictions")

		#use risk model to predict risk bucket
  		r.p = switch(model.type,
			tree = predict(r.model, newdata=df.test,type='class'),
			svm = predict(r.model, newdata=df.test,type='class'),
			nb = predict(r.model, newdata=df.test,type='class'))
		message('prediction done 1')

		#set risk predictions as readmit bucket and then use cost model to predict cost bucket
		c.df.test<-df.test
		c.df.test$ReadmitBucket<-r.p
		c.p = switch(model.type,
			tree = predict(c.model, newdata=c.df.test,type='class'),
			svm = predict(c.model, newdata=c.df.test,type='class'),
			nb = predict(c.model, newdata=c.df.test,type='class'))
		message('prediction done 2')
		
		#join risk and cost prediction as overall prediction
		p = paste(r.p,c.p,sep=' ')
	} else if(approach=='BR') {
		message('in BR')
		#build a model to predict cost WITHOUT using readmit bucket as a predictor
		c.model = switch(model.type,
			tree = rpart(formula=formula('CostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ReadmitBucket'))], control=rpart.control(cp=cp)),
			svm = svm(formula=formula('CostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ReadmitBucket'))]),
			nb = naiveBayes(formula=formula('CostBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('ReadmitBucket'))]))
		message('model made 1')
		#build a model to predict risk WITHOUT using cost bucket as a predictor
		r.model = switch(model.type,
			tree = rpart(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))], control=rpart.control(cp=cp)),
			svm = svm(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]),
			nb = naiveBayes(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.shuffle) %in% c('CostBucket'))]))
		message('model made 2')
 
  		print("making predictions")

		#use risk model to predict readmit bucket
  		r.p = switch(model.type,
			tree = predict(r.model, newdata=df.test,type='class'),
			svm = predict(r.model, newdata=df.test,type='class'),
			nb = predict(r.model, newdata=df.test,type='class'))
		message('prediction done 1')

		#use cost model to predict cost bucket
		c.p = switch(model.type,
			tree = predict(c.model, newdata=df.test,type='class'),
			svm = predict(c.model, newdata=df.test,type='class'),
			nb = predict(c.model, newdata=df.test,type='class'))
		message('prediction done 2')
		
		#join risk and cost prediction as overall prediction
		p = paste(r.p,c.p,sep=' ')

	} else if(model.type=='majority') {
		majority.value = tail(names(sort(table(as.character(df.train$ReadmitAndCostBucket)))), 1)
		p = rep(majority.value,nrow(df.test))
	} else {
		p<-sample(levels(df.train$ReadmitAndCostBucket),nrow(df.test),replace=TRUE)
	}

	#add new predictions to our aggregate vector of predictions
	preds<-c(preds,as.character(p))
	#add appropriate truth values for response variable(s) to our aggregate vector of true values
	if((approach == 'LP') || (approach=='BL')) {
		truths<-c(truths,as.character(df.test$ReadmitAndCostBucket))
	} else {
		truths<-c(truths,as.character(paste(df.test$ReadmitBucket,df.test$CostBucket,sep=' ')))
	}

	message('stored in preds and truths')
}
#report average accuracy
message('average accuracy: ',averageAccuracy(preds,truths))
#report average precision
aprecision = averagePrecision(preds,truths)
message('average precision: ',aprecision)
#report average recall
arecall = averageRecall(preds,truths)
message('average recall: ',arecall)
#report f1measure
message('f1measure: ',f1Measure(arecall,aprecision))

#convert predictions/true values to factors for confusion matrix
both<-as.factor(rbind(preds,truths))

#print confusion matrix
message('confusion matrix')
confusionMatrix(both[1:length(preds)],both[(length(preds)+1):length(both)])

#save predictions/truths for future reference if needed
preds.file<-paste(approach,model.type,'preds.rds',sep=" ")
truths.file<-paste(approach,model.type,'truths.rds',sep=" ")
if(model.type=='tree') {
	preds.file<-paste(cp,preds.file,sep=" ")
	truths.file<-paste(cp,truths.file,sep=" ")
}
saveRDS(preds,file=preds.file)
saveRDS(truths,file=truths.file)	

