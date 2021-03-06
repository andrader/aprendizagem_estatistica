# Validacao XGBoost #===================#
#
#
#=======================================#

standard=c(2,2,3,2)
set.seed(1)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, xtest)
}
require(reshape2)
conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

(RMSE_eta = sqrt(colMeans((ytest-pred_eta)^2)))

set.seed(1)
conv_cs = matrix(NA,500,length(cs))
pred_cs = matrix(NA,length(test), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs
for(i in 1:length(cs)){
  params = list(eta = eta[standard[1]], colsample_bylevel = cs[i],
                subsample = ss[standard[4]], max_depth = md[standard[3]],
                min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, xtest)
}
conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))

(RMSE_cs = sqrt(colMeans((ytest-pred_cs)^2)))


set.seed(1)
conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,length(test),length(md))
colnames(conv_md)=colnames(pred_md)=md
for(i in 1:length(md)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[standard[4]],max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500,params=params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, xtest)
}

(RMSE_md=sqrt(colMeans((ytest-pred_md)^2)))

set.seed(1)
conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,length(test),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss
for(i in 1:length(ss)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500,params=params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, xtest)
}

(RMSE_ss=sqrt(colMeans((ytest-pred_ss)^2)))


