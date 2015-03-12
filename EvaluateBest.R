#load necessary packages
library(rpart)
library(e1071)
library(caret)
library(randomForest)

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
file.test.name <- args[1] #where the data is located
file.train.name<-args[2]
approach <- 'BR'
model.type <- 'svm'

message('train file name: ',file.train.name,' --test file name: ',file.test.name,' -- appraoch: ',approach,' -- model type: ',model.type)

df.test <- read.csv(file.test.name) #read in test data
df.train<-read.csv(file.train.name) #read in train data
df.test$X<-NULL #get rid of unnecessary column
df.train$X<-NULL
df.test$X.1<-NULL #get rid of unnecessary column
df.train$X.1<-NULL
df.test$ReadmitAndCostBucket <- paste(df.test$ReadmitBucket,df.test$CostBucket,sep=" ") #create LP label
df.train$ReadmitAndCostBucket <- paste(df.train$ReadmitBucket,df.train$CostBucket,sep=" ")
df.test$ReadmitBucket<-as.factor(df.test$ReadmitBucket) #ensure readmit response variable is a factor
df.train$ReadmitBucket<-as.factor(df.train$ReadmitBucket)
df.test$ReadmitAndCostBucket<-as.factor(df.test$ReadmitAndCostBucket) #ensure LP response variable is a factor
df.train$ReadmitAndCostBucket<-as.factor(df.test$ReadmitAndCostBucket)
df.test$CostBucket<-as.factor(df.test$CostBucket) #ensure cost response variable is a factor
df.train$CostBucket<-as.factor(df.train$CostBucket)
df.test$FEMALE<-as.logical(df.test$FEMALE) #ensure gender is a boolean
df.train$FEMALE<-as.logical(df.train$FEMALE)
df.test$RACE<-as.factor(df.test$RACE) #ensure race is a factor
df.train$RACE<-as.factor(df.train$RACE)

#ensure that all U-codes are factors
for(i in grep("^U_",colnames(df.test))) {
	df.test[,i]<-as.factor(df.test[,i])
}
#ensure that all U-codes are factors
for(i in grep("^U_",colnames(df.train))) {
	df.train[,i]<-as.factor(df.train[,i])
}

#get columns for round 3
df.test<-df.test[,c('TOTCHG','CumulativeCost','AGE','LOS','REVCD_921','REVCD_361','REVCD_272','REVCD_391','REVCD_440','DXCCS_249','U_ULTRASOUND','REVCD_402','DXCCS_2','REVCD_302','DXCCS_157','PRCCS_50','DXCCS_109','PRCCS_216','PRCCS_223','PRCCS_54','CostBucket','ReadmitBucket','ReadmitAndCostBucket')]
#get columns for round 3
df.train<-df.train[,c('TOTCHG','CumulativeCost','AGE','LOS','REVCD_921','REVCD_361','REVCD_272','REVCD_391','REVCD_440','DXCCS_249','U_ULTRASOUND','REVCD_402','DXCCS_2','REVCD_302','DXCCS_157','PRCCS_50','DXCCS_109','PRCCS_216','PRCCS_223','PRCCS_54','CostBucket','ReadmitBucket','ReadmitAndCostBucket')]

preds<-c() #where we will hold predictions
truths<-c() #where we will hold ground truth

#remove unnecessary response variable depending on approach
df.test$ReadmitAndCostBucket<-NULL
df.train$ReadmitAndCostBucket<-NULL
message('remove unnecessary buckets')
message('ncol: ',ncol(df))

#shuffle the data
df.train = df.train[1:nrow(df.train),]
df.test = df.test[1:nrow(df.test),]
message('shuffle')

#train model
message('in BR')
#build a model to predict cost WITHOUT using readmit bucket as a predictor
c.model = svm(formula=formula('CostBucket~.'), data=df.train[,-which(names(df.train) %in% c('ReadmitBucket'))])
message('model made 1')
#build a model to predict risk WITHOUT using cost bucket as a predictor
r.model = svm(formula=formula('ReadmitBucket~.'), data=df.train[,-which(names(df.train) %in% c('CostBucket'))])
message('model made 2')
 
print("making predictions")

#use risk model to predict readmit bucket
r.p = predict(r.model, newdata=df.test,type='class')
message('prediction done 1')

#use cost model to predict cost bucket
c.p = predict(c.model, newdata=df.test,type='class')
message('prediction done 2')
		
#join risk and cost prediction as overall prediction
p = paste(r.p,c.p,sep=' ')

#add new predictions to our aggregate vector of predictions
preds<-c(preds,as.character(p))
#add appropriate truth values for response variable(s) to our aggregate vector of true values
truths<-c(truths,as.character(paste(df.test$ReadmitBucket,df.test$CostBucket,sep=' ')))

message('stored in preds and truths')

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
preds.file<-paste(approach,model.type,'preds TEST.rds',sep=" ")
truths.file<-paste(approach,model.type,'truths TEST.rds',sep=" ")

saveRDS(preds,file=preds.file)
saveRDS(truths,file=truths.file)	

