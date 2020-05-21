###################################################

Logistic Regression

####################################################

set.seed(1)

logistic.fit=glm(Response~INT_TIME1+A15C+H2_30+H8_30+INDTOT2+E13+CHR_SUM+A13+A17D+C1A+

C3G1+D2+E8A1+ M5+M32+O1D+Q11 +U2H

+U3A+U3B+ANY_INS+FRML_SAT+E10B1_R+H2_PRB+CHR_6M,data=ISEN613,

family=binomial,subset=trainindex)

summary(logistic.fit)

logistic.probs=predict(logistic.fit, newdata= ISEN613[-trainindex,], type="response")

logistic.pred=rep("FALSE",253) logistic.pred[logistic.probs>0.5]="TRUE" table(logistic.pred, ISEN613[-trainindex, ]$RCT_LINK) mean(logistic.pred==ISEN613[-trainindex, ]$RCT_LINK) table(logistic.pred, Response[-trainindex]) mean(logistic.pred==Response[-trainindex])
###########################################################

ROC Curve Logistic

##########################################################

LR.pred=predict(logistic.fit,type="response")

#LDA.pred=lda.prob$posterior[,2]

roc.curve=function(s,print=FALSE)

{

Ps=(LR.pred>s)*1

FP=sum((Ps==1)*(Response=="FALSE"))/sum(Response=="FALSE")

TP=sum((Ps==1)*(Response=="TRUE"))/sum(Response=="TRUE")

if(print==TRUE)

{

print(table(Observed=Response,Predicted=Ps))

}

vect=c(FP,TP)

names(vect)=c("FPR","TPR")

return(vect)

}

threshold=0.5

roc.curve(threshold,print=TRUE)

roc.curve=Vectorize(roc.curve)

M.roc=roc.curve(seq(0,1,by=0.01))

plot(M.roc[1,],M.roc[2,],col="grey",lwd=2,xlab="False Positive Rate",ylab="True Positive Rate")

#####################################################

QDA

#####################################################

set.seed(1)

qda.fit=qda(Response~INT_TIME1+A15C+H2_30+H8_30+INDTOT2+E13+CHR_SUM+A13+A17D+C1A+C3

G1+D2+E8A1+ M5+M32+O1D+Q11 +U2H +U3A+U3B+ANY_INS+FRML_SAT+E10B1_R+H2_PRB+CHR_6M,

data=ISEN613,subset=trainindex)

qda.fit

qda.class=predict(qda.fit,ISEN613[-trainindex,])$class

table(qda.class,ytest)

mean(qda.class==ytest)

summary(qda.fit)

#####################################################

KNN

#####################################################

set.seed(1)

library(class)

e=c("RCT_LINK", "Response")

train1=train[,!names(train)%in%e]

test1=test[,!names(test)%in%e]

n=length(ytest)

dim(train1)

dim(test1)

mean.knn.pred=c()

error.knn.pred=c()

for (i in 1:n){

knn.pred1=knn(train1,test1,ytrain ,k=i)

mean.knn.pred[i]=mean(knn.pred1==ytest)

error.knn.pred[i]=1-mean.knn.pred[i]

}

mean.knn.pred

max(mean.knn.pred)

min(error.knn.pred)

f=which(error.knn.pred==min(error.knn.pred))	#value of k at which we get minimum error

which(mean.knn.pred==max(mean.knn.pred))

plot(mean.knn.pred,xlab = "K", ylab="Percentage Accuracy", main="Plot for Percentage Accuracy vs K nearest neighbours") #Accuracy plot

plot(error.knn.pred,xlab = "K", ylab="Percentage Error", main="Plot for Percentage Error vs K nearest neighbours") # Error plot

points(f,error.knn.pred[f],col="red", pch=20)



knn.pred17=knn(train1,test1,ytrain,k=17)

table(knn.pred17,test$Response)



########################################################

SVM

########################################################

set.seed(1)

library(e1071)

svmfit=svm(Response~INT_TIME1+A15C+H2_30+H8_30+INDTOT2+E13+CHR_SUM+A13+A17D+C1A+C3 G1+D2+E8A1+

M5+M32+O1D+Q11 +U2H +U3A+U3B+ANY_INS+FRML_SAT+E10B1_R+H2_PRB+CHR_6M, data=ISEN613,subset=trainindex,kernel="radial",gamma=1,cost=1) summary(svmfit)

#for gamma=1,cost=ee5

svmfit=svm(Response~INT_TIME1+A15C+H2_30+H8_30+INDTOT2+E13+CHR_SUM+A13+A17D+C1A+C3 G1+D2+E8A1+

M5+M32+O1D+Q11 +U2H +U3A+U3B+ANY_INS+FRML_SAT+E10B1_R+H2_PRB+CHR_6M, data=ISEN613,subset=trainindex,kernel="radial",gamma=1,cost=1e5) summary(svmfit)

tune.out=tune(svm,Response~INT_TIME1+A15C+H2_30+H8_30+INDTOT2+E13+CHR_SUM+A13+A17D+C 1A+C3G1+D2+E8A1+

M5+M32+O1D+Q11 +U2H

+U3A+U3B+ANY_INS+FRML_SAT+E10B1_R+H2_PRB+CHR_6M,data=ISEN613[trainindex,],kernel="radial"

,ranges=list(cost=c(.1,1,10,100,1000),gamma=c(.5,1,2,3,4)))

summary(tune.out)

table(true=ISEN613[-trainindex,"Response"],pred=predict(tune.out$best.model,ISEN613[-trainindex,]))

pred=predict(tune.out$best.model,ISEN613[-trainindex,])

mean(pred==ISEN613[-trainindex,"Response"])


########################################

#Ridge Regression for reduced parameters

########################################

install.packages("glmnet")

library(glmnet)

x=model.matrix(RCT_LINK~INT_TIME1 + A15C +	H2_30 +	H8_30 +	INDTOT2 + E13 +	CHR_SUM

+A13	+  A17D +	C1A  +	C3G1 +	D2+	E8A1 +

M5	+	M32  +	O1D +	Q11 +	U2H +	U3A	+  U3B +	ANY_INS + FRML_SAT +

E10B1_R + H2_PRB +  CHR_6M ,ISEN613)[,-1]

y=as.double(ISEN613$RCT_LINK)

set.seed(1)

#	To find best lambda k=10

set.seed(1)

folds=sample(1:k,nrow(ISEN613), replace = TRUE)

bestlamjr=rep(0,10)

library(leaps)

for(j in 1:k){

grid=10^seq(10,-2,length=100)

train= folds!=j

test=(-train)

cv.outjr =cv.glmnet(x[train,],y[train], alpha=0,lambda = grid)

bestlamjr[j]=cv.outjr$lambda.min

}

bestlamjr

bestlambdar=bestlamjr[j]

bestlambdar

#	Using best lambda on training data set to find prediction accuracy on test data set trainIndex1r=sample(1:nrow(x),nrow(x)*0.7,replace = FALSE) test2r=(-trainIndex1r)


y.testr=y[test2r]

ridge.mod=glmnet(x[trainIndex1r,],y[trainIndex1r],alpha=0,lambda = bestlambdar)

summary(ridge.mod)

set.seed(1)

ridge.pred=predict(ridge.mod,s=bestlambdar,newx = x[test2r,])

summary(ridge.pred)

newyr=c(ridge.pred,y)

newyr

mean((newyr-y.testr)^2)

########################################

#Neural Network

########################################

dim(newdata)

attach(newdata)

set.seed(1)

ideal = class.ind(newdata$RCT_LINK)

n1 = nrow(newdata)

trainIndex=sample(1:n1, size=round(0.7*n1), replace = FALSE)

DA.train=newdata[trainIndex,]

dim(DA.train)

DA.test=newdata[-trainIndex,]

n <- names(DA.train)

#f <- as.formula(paste("RCT_LINK ~", "A15C + H1_30 + H2_30 + H8_30 + A13 + A17D + B3D + C1A + C3G1 + D2 + E8A1 + E13 + M32 + O1D + Q11 + U2H + U3A + U3B + CHR_SUM + ANY_INS + FRML_SAT + E10B1_R + CHR_6M"))

nn <- neuralnet(RCT_LINK ~ INT_TIME1 + A15C +  H2_30 +	H8_30 +  INDTOT2 + E13 +   CHR_SUM
+A13  +  A17D +	C1A	+	C3G1 +	D2+	E8A1 +	
M5  +	M32	+	O1D +	Q11 +	U2H +   U3A	+  U3B +  ANY_INS + FRML_SAT +

E10B1_R + H2_PRB +  CHR_6M,data=DA.train, hidden = c(24,10), linear.output=F)

pr.nn <- compute(nn,DA.test[,1:25])

#plot(nn)

pr.nn$net.result <- ifelse(pr.nn$net.result<0.4,0,1)

mean(pr.nn$net.result==DA.test$RCT_LINK)

table(pr.nn$net.result==DA.test$RCT_LINK)

