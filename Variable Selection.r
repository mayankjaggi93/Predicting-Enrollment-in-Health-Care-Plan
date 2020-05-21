#Converting to categorical


attach(NewDataset)

a=c("TIME","NUM_INTERVALS","INT_TIME1","A15A","A15B","A15C","D3","H1_30","H2_30","H3_30","H 4_30",

"H5_30","H6_30","H7_30","H8_30","H9_30","H10_30","H11_30","H12_30","H13_30","H15A","H15B",

"H16A","H16B","H17A","H17B","ALCQ_30","RAWPF","PF","RP","RAWBP","BP","RAWGH","GH","RAWVT ",

"VT","RAWSF","SF","RAWRE","RE","RAWMH","MH","HT","PCS","MCS","CES_D","C_MS","C_AU","C_DU" ,



"RAW_RE","DEC_RE","RAW_AM","DEC_AM","RAW_TS","DEC_TS","PHYS","PHYS2","INTER","INTRA","IM PUL",

"IMPUL2","SR","CNTRL","INDTOT","INDTOT2","PSS_FR","PSS_FA","DRUGRISK","SEXRISK","TOTALRAB", "RABSCALE","ANY_VIS","ANY_VIS_CUMUL")



abc=NewDataset[,!names(NewDataset)%in%a]

#View(abc)

abc[]=lapply(abc[],factor)

sapply(abc,class)

ISEN613=data.frame(TIME,NUM_INTERVALS,INT_TIME1,A15A,A15B,A15C,D3,H1_30,H2_30,H3_30,H4_30,
H5_30,H6_30,H7_30,H8_30,H9_30,H10_30,H11_30,H12_30,H13_30,H15A,H15B,H16A,H16B,H17A,H17B,
ALCQ_30,RAWPF,PF,RP,RAWBP,BP,RAWGH,GH,RAWVT,VT,RAWSF,SF,RAWRE,RE,RAWMH,MH,HT,PC

S,MCS,CES_D,C_MS,C_AU,C_DU,RAW_RE,DEC_RE,RAW_AM,DEC_AM,RAW_TS,DEC_TS,PHYS,PHYS2,INT

ER,INTRA,IMPUL,IMPUL2,SR,CNTRL,INDTOT,INDTOT2,PSS_FR,PSS_FA,DRUGRISK,SEXRISK,TOTALRAB,RAB

SCALE,ANY_VIS,ANY_VIS_CUMUL,abc)

b=c("C1M","E14F","H12_30","H12_RT","H9_PRB","H12_PRB","E8A3","E14E","H6_RT","H9_30","H9_RT",

"H11_RT","S1B","S1D","H6_PRB","H11_PRB","ANY_VIS_CUMUL", "PC_REC",

"PC_REC7","PCS","MCS","ANY_VIS","E10B2_R","E10A","U5","U4","ANY_UTIL","REG_MD")	#with only

one categorical level and less variability and the obvious ones including cumulative
 

ISEN613=ISEN613[,!names(ISEN613)%in%b] #View(ISEN613)
 


#now the dataframe has 404 variables and 842 obs

 



#data split

sum(is.na(ISEN613))

#attach(ISEN613)

#set.seed (3)

#Response=ifelse(RCT_LINK==0,"FALSE","TRUE")

#ISEN613=data.frame(ISEN613,Response) #now its 405 variables #View(ISEN613)



trainindex=sample (1: nrow(ISEN613),size=0.7*nrow(ISEN613), replace=FALSE)

train=ISEN613[trainindex,]

test=ISEN613[-trainindex,]



ytest=test$Response

ytrain=train$Response

ytest

length(ytest)

length(ytrain)

dim(ISEN613)

dim(test)

dim(train)

length(ytest)

View(ISEN613)



########################################

#LASSO For variable Selection

########################################

install.packages("glmnet")

library(glmnet)

x=model.matrix(RCT_LINK~.,ISEN613)[,-1]

y=as.double(ISEN613$RCT_LINK)

#	10 fold cross validation to find best lambda k=10

set.seed(1)

folds=sample(1:k,nrow(ISEN613), replace = TRUE)

bestlamj=rep(0,10)

library(leaps)

for(j in 1:k){

grid=10^seq(10,-2,length=100)

train= folds!=j

test=(-train)

cv.outj =cv.glmnet(x[train,],y[train], alpha=1,lambda = grid)

bestlamj[j]=cv.outj$lambda.min

}

bestlamj

bestlambda=bestlamj[j]

bestlambda

out=glmnet(x,y,alpha=1,lambda=grid)

plot(out)

lasso.coef=predict(out,type="coefficient",s=bestlambda)[1:434,]

lasso.coef[lasso.coef!=0] #Predictors with non zero coefficients using lasso

trainIndex1=sample(1:nrow(x),nrow(x)*0.7,replace = FALSE)

test2=(-trainIndex1)

y.test=y[test2]

lasso.mod=glmnet(x[trainIndex1,],y[trainIndex1],alpha=1,lambda = bestlambda)

plot(lasso.mod)

summary(lasso.mod)

set.seed(1)

lasso.pred=predict(lasso.mod,s=bestlambda,newx = x[test2,])

summary(lasso.pred)

newy=c(lasso.pred,y)

newy

mean((newy-y.test)^2)




#Randomforest on all attributes



set.seed(1)

library(randomForest)

rf.ISEN613=randomForest(Response~.-

RCT_LINK,data=ISEN613,subset=trainindex,mtry=21,importance=TRUE)

summary(rf.ISEN613)

rf.ISEN613

yhat.rf = predict(rf.ISEN613 ,newdata =test)


l1=mean(yhat.rf==test$Response)	#test accuracy of random forest
l1	

varImpPlot(rf.ISEN613,sort=TRUE,n.var=25)





#Bortua

library(Boruta)

set.seed(1)

boruta.train = Boruta(RCT_LINK~.-Response, data = train, doTrace = 2)

print(boruta.train)



plot(boruta.train, xlab = "", xaxt = "n")

lz=lapply(1:ncol(boruta.train$ImpHistory),function(i)

boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])

names(lz) = colnames(boruta.train$ImpHistory)

Labels = sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),

at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)



final.boruta = TentativeRoughFix(boruta.train)

print(final.boruta)

a=getSelectedAttributes(final.boruta, withTentative = F) #this shows the selected attributes a


plot(a,xlab = "", xaxt = "n")



boruta.df = attStats(final.boruta)

class(boruta.df)

print(boruta.df)

yhat.bortua = predict(boruta.train ,newdata =test)

sapply(boruta.train,class)



##Random forest on bortua attributes



set.seed(1)							
library(randomForest)						
rf.ISEN613_1=randomForest(Response~INT_TIME1 + A15C +	H2_30 +	H8_30 +  INDTOT2 + E13 +
CHR_SUM  +A13	+  A17D +	C1A  +	C3G1 +	D2+	E8A1 +	
M5	+  M32	+  O1D +	Q11 +	U2H +		U3A  +	U3B +  ANY_INS + FRML_SAT

+	E10B1_R + H2_PRB + CHR_6M, data=ISEN613,subset=trainindex,mtry=5,importance=TRUE) summary(rf.ISEN613_1)

yhat.rf1 = predict(rf.ISEN613_1 ,newdata =test) l4=mean(yhat.rf1==test$Response)

l4

#RANDOM FOREST ON ATTRIBUTES THAT ARE FROM RANDOM FOREST 25 VARIABLES set.seed(1)

library(randomForest)

rf.ISEN613_2=randomForest(Response~E13 + INT_TIME1 +A15C + O1D + B9E + C_MS + RAWGH + B7 +

CES_D

+A13+B9H+GH+B9D+B11D+D5+CHR_SUM+E8A1+RAW_TS+R1P+RAW_AM+H16B+B9I+B9B+CHR_6M+B9

F,data=ISEN613,subset=trainindex,mtry=5,importance=TRUE)

summary(rf.ISEN613_2)

yhat.rf2 = predict(rf.ISEN613_2 ,newdata =test)

l5=mean(yhat.rf2==test$Response)

l5

#Since l5<l4 this suggests that attributes from boruta have a higher prediction accuracy than that from random forest. So, these variables are selected.

#Tree plotting		
library(tree)		
tree.ISEN613=tree(Response~INT_TIME1 + A15C +	H2_30 +  H8_30 +  INDTOT2 + E13 +
CHR_SUM  +A13  +  A17D +  C1A  +  C3G1 +	D2+	E8A1 +
M5  +  M32  +  O1D +  Q11 +   U2H +	U3A  +  U3B +  ANY_INS + FRML_SAT +
E10B1_R + H2_PRB +  CHR_6M ,data=train)	
summary(tree.carseats)	
plot(tree.ISEN613)	
text(tree.ISEN613,pretty = 0)	
