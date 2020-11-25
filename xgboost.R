library(xgboost)

# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

xgb.train = xgb.DMatrix(data=as.matrix(crime_training[,-22]),label= as.integer(crime_training[,22]$target)-1)
xgb.test = xgb.DMatrix(data=as.matrix(crime_testing[,-3]),label= as.integer(crime_testing[,3]$target)-1)

test.label =  as.integer(crime_testing[,3]$target)-1
train.label = as.integer(crime_training[,22]$target)-1
data.teste = as.matrix(crime_testing[,-3])
data.treino = as.matrix(crime_training[,-22])

detach(sample.int)
train = data.treino[1:2000,]
test = data.teste[2000:3500,]
xtrain = train
ytrain = train.label[1:2000]
xtest = test
ytest = test.label[2000:3500]


num_class = 4
params = list(
  booster="gbtree",
  eta=0.05,
  max_depth=10,
  gamma=1,
  subsample=.75,
  colsample_bytree=0.333333333333333,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)


xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=7,
  early_stopping_rounds=10,
  verbose=0,
  watchlist=list(val1=xgb.train,val2=xgb.test)
)



xgb.fit

xgb.pred = predict(xgb.fit,data.teste,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(crime_testing$target)
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(crime_testing$target)[test.label+1]

result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))


importance<- xgb.importance(model = xgb.fit)

xgb.plot.importance(xgb.fit)

print(xgb.plot.importance(importance_matrix = importance,top_n = 5))
