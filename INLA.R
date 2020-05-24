setwd('C:\\Users\\lenovo\\Desktop\\毕业论文')
#data <- read.csv("./data/graduate-admissions/Admission_Predict.csv",
#                 header = T)
data0 <- read.csv("./data/graduate-admissions/Admission_Predict_Ver1.1.csv",
                  header = T) # 载入数据
plot(density(data0$Chance.of.Admit)) #查看可能性分布曲线
hist(data0$Chance.of.Admit) #查看毕业可能性直方图

sum(is.na(data0)) # 检查缺失值
data1 <- data0[,-1] # 去掉id列
str(data1)

summary(data1)
# 描述性统计
library(psych)
describe(data1,quant = c(.25,.75),type = 1) # summary

sum(data1$Chance.of.Admit>0.6) # 我们假设可能性大于0.6的学生可以成功录取
sum(data1$Chance.of.Admit>0.8)/nrow(data0) # 可以看到成功录取的学生比例为80%

colnames(data1) <- c('GRE','TOEFL','UR','SOP','LOR','CGPA','RE','y') # 将我们关注的变量命名为y
# 数据可视化
library(GGally)
ggpairs(data1) # 绘制散点图，各变量频率曲线，相关系数
#ggplot(data1,aes(x=y)) + geom_density()
#a <- density(data1$y)

data1$y0 <- ifelse(data1$y>0.8,1,0) # 将0.6以上定为1,0.6以下定为0
data1$y0 <- factor(data1$y0);levels(data1$y0) <- c('1','2') # 成功入学为2，失败为1
str(data1)
table(data1$y0) # 成功入学有403人
data <- data1[,-8] # data数据集为去掉y0即概率列
data2 <- data1[,-9] # data2数据集为去掉y即判断列
str(data)

library(caret)
# 进行划分
set.seed(2020)
Index <- createDataPartition(data$y0,p=0.7,list=F,times=1) # 70%为训练集，30%为测试集
train <- data[Index,] 
test <- data[-Index,]
dim(train);str(train)

#train$RE <- factor(train$RE);test$RE <- factor(test$RE)
#train$UR <- factor(train$UR);test$UR <- factor(test$UR)


################################
###    INLA-logistics
################################
library(INLA)
#################################
# 使用data数据集进行分析
#################################
# 标准化
#table(train$SOP);hist(train$SOP)
#train$SOP <- scale(train$SOP)
#median(train$SOP);mean(train$SOP)
trainscale <- preProcess(train[,-7],method = c('center','scale')) # 标准化
traininla <- predict(trainscale,train) 
testinla <- predict(trainscale,test) # 对测试集标准化（依照训练集）

#########################################################################################
#################                     构造sas使用的数据集
#########################################################################################
trainmcmc <- traininla
trainmcmc$y0 <- ifelse(trainmcmc$y0=='2',1,0);trainmcmc$y0 <- as.numeric(trainmcmc$y0) # 将变量类型转换为numeric
#trainmcmc$UR <- factor(trainmcmc$UR)
colnames(trainmcmc)[8] <- 'COA'
#trainmcmc <- model.matrix(~.,data = trainmcmc)[,-1];trainmcmc <- data.frame(trainmcmc)
#trainmcmc$UR2 <- ifelse(trainmcmc$UR2==1,2,0)
#trainmcmc$UR3 <- ifelse(trainmcmc$UR3==1,3,0)
#trainmcmc$UR4 <- ifelse(trainmcmc$UR4==1,4,0)
#trainmcmc$UR5 <- ifelse(trainmcmc$UR5==1,5,0)
write.csv(trainmcmc,file = './data/graduate-admissions/MCMC_data3.csv')

#traininla <- train
#testinla <- test
#######################
###   INLA建模
#######################
#colnames(train1)
#traininla$UR <- as.numeric(traininla$UR);traininla$RE <- as.numeric(traininla$RE) 
traininla$y0 <- as.numeric(traininla$y0)-1 # 将变量类型转换为numeric

Ntrial <- rep(1,nrow(traininla)) # 这里是认为每次做一次实验，即trial为1，然后目标为0或1，即录取或不录取
#traininla$UR <- factor(traininla$UR);traininla <- as.data.frame(traininla)

GAinla <- inla(y0 ~ GRE+TOEFL+UR+SOP+LOR+CGPA+RE, data = traininla,Ntrials = Ntrial,
                  control.compute = list(dic = TRUE),family = "binomial",
                  control.family = list(
                    control.link = list(model = "logit"))
               )

#GAinla <- inla(y0 ~GRE+TOEFL+UR+SOP+LOR+CGPA+RE, family = 'binomial',data = traininla,Ntrials = Ntrial)

summary(GAinla)

#  画出后验分布
#beta0 <- GAinla$marginals.fixed$GRE
#plot(beta0[,1],beta0[,2],type = 'l')
#beta0 = inla.tmarginal(fun = function(x) x, marginal = 
#                         GAinla$marginals.fixed$`(Intercept)`)
beta0 = data.frame(inla.smarginal(marginal = 
                         GAinla$marginals.fixed$`(Intercept)`))
beta0.s = inla.zmarginal(marginal=GAinla$marginals.fixed$`(Intercept)`) # 对参数各统计量总结 包括均值标准差和各分位数
(beta0.hpd = inla.hpdmarginal(0.95,marginal=GAinla$marginals.fixed$RE))# 对参数各统计量总结 包括均值标准差和各分位数
ggplot(beta0,aes(x=x,y=y)) + geom_line() + 
  theme(axis.line = element_line(size = 3, colour = "grey80"),
        axis.text = element_text(colour = "blue"),
        axis.ticks = element_line(size = 2)) + 
  labs(x = expression(beta_7),y = "Posterior density")
#plot(beta0, type="l", xlab = expression(beta_0), ylab = "Posterior density")


# 提取参数
(cof <- GAinla$summary.fixed$mean)
# 提取训练集数据
traindata <- data.frame(cbind(rep(1,nrow(traininla)),traininla[,-8]));colnames(traindata)[1] <- 'intercept' 
# 对每一行数据乘以得到的参数
result <- apply(t(apply(traindata,1,function(x) cof*x)),1,sum)
# 计算预测概率
prob <- exp(result)/(1+exp(result))
# View(cbind(prob,traininla$y0))
# 这里用了一个循环，用来判断判断使得判断为1的最合适的概率
#acc <- c();j = 1;index <- seq(0.01,0.95,by = 0.01)
#for(i in index){
#  res <- ifelse(prob>i,1,0)
#  table <- table(res,traininla$y0)
#  acc[j] <- sum(diag(table))/sum(table)
#  j <- j + 1
#}
#plot(acc,type = 'b');(finprob <- index[which.max(acc)])

res <- ifelse(prob>0.8,1,0) # 
(table <- table(res,traininla$y0))
# 得到混淆矩阵
pre <- factor(res);ref <- factor(traininla$y0)
conM.inla <- confusionMatrix(pre,ref,positive = '1')
conM.inla
# 用得到的参数对测试集预测
#testinla$UR <- as.numeric(testinla$UR);testinla$RE <- as.numeric(testinla$RE)
testinla$y0 <- as.numeric(testinla$y0)-1 # 将变量类型转换为numeric
testdata <- data.frame(cbind(rep(1,nrow(testinla)),testinla[,-8]));colnames(testdata)[1] <- 'intercept' 
# 对每一行数据乘以得到的参数
testresult <- apply(t(apply(testdata,1,function(x) cof*x)),1,sum)
# 计算预测概率
testprob <- exp(testresult)/(1+exp(testresult))
#View(testprob)
testres <- ifelse(testprob>0.8,1,0)
testpre <- factor(testres);testref <- factor(testinla$y0)
conM.inla <- confusionMatrix(testpre,testref,positive = '1')
conM.inla

###############################
# 使用data2数据集进行分析
###############################
data2$y <- data2$y*100 # 这里把概率乘一百，表示做100次实验的成功录取次数
# 进行划分
set.seed(2020)
Index2 <- createDataPartition(data2$y,p=0.7,list=F,times=1) # 70%为训练集，30%为测试集
train2 <- data2[Index2,]
test2 <- data2[-Index2,]
dim(train2);str(train2)
# 标准化
train2scale <- preProcess(train2[,-c(3,7,8)],method = c('center','scale')) # 标准化
train2inla <- predict(train2scale,train2) 
test2inla <- predict(train2scale,test2) # 对测试集标准化（依照训练集）
# 建模
Ntrial2 <- rep(100,nrow(train2inla)) # 这里是认为每次做100次实验，即trial为100，然后目标为y，即录取的次数
GAinla2 <- inla(y ~GRE+TOEFL+UR+SOP+LOR+CGPA+RE, family = 'binomial',data = train2inla,Ntrials = Ntrial2)
summary(GAinla2)
# 提取参数
(cof2 <- GAinla2$summary.fixed$mean)
# 用得到的参数对测试集预测
test2data <- data.frame(cbind(rep(1,nrow(test2inla)),test2inla[,-8]));colnames(test2data)[1] <- 'intercept' 
# 对每一行数据乘以得到的参数
test2result <- apply(t(apply(test2data,1,function(x) cof2*x)),1,sum)
# 计算预测概率
test2prob <- exp(test2result)/(1+exp(test2result))
#View(test2prob)
test2res <- factor(ifelse(test2prob>0.6,1,0))
test2ref <- factor(ifelse(test2inla$y>60,1,0))
conM.inla2 <- confusionMatrix(test2res,test2ref,positive = '1')
conM.inla2


########################################################################
##########     MCMC
########################################################################
library(mcmc)
out <- glm(y0 ~ GRE+TOEFL+UR+SOP+LOR+CGPA+RE, data = trainmcmc,
           family=binomial(link = "logit"))
summary(out)
x <- trainmcmc
x$y0 <- NULL
x <- as.matrix(x)
x <- cbind(1, x)
dimnames(x) <- NULL
y <- as.numeric(trainmcmc$y0)-1
lupost <- function(beta, x, y){
  eta <- x %*% beta
  p <- 1/(1 + exp(-eta))
  logl <- sum(log(p[y==1])) + sum(log(1-p[y==0]))
  return(logl+sum(dnorm(beta,0,100,log = TRUE)))
}
set.seed(2020)
beta.init <- as.numeric(coefficients(out))
out <- metrop(lupost,scale = 0.3,beta.init,nbatch = 40000,blen = 1,x=x,y=y)
out <- metrop(out, nbatch = 60000, blen = 1, 
              outfun = function(z,...) c(z, z^2), x = x, y = y)
out$accept
out$time
(foo <- apply(out$batch, 2, mean))
(mu <- foo[1:8])
(sigmasq <- foo[9:16] - mu^2)
(sigma <- sqrt(sigmasq))
(mu.mcse <- apply(out$batch[, 1:8], 2, sd)/sqrt(out$nbatch))
View(out[["batch"]])
plot(ts(out$batch[,1:8]))
acf(out$batch[,1:8])
#################################
####   LearnBayes 
#################################
library(LearnBayes)
#xy <- cbind(y,x)
fit <- laplace(lupost,beta.init,x,y)
acc <- c();learn_mean <- data.frame()
for(i in seq(0.1,0.9,by=0.1)){
  proposal <- list(var = fit$var,scale = i)
  fitrw <- rwmetrop(logpost = lupost,proposal = proposal,
                    start = t(fit$mode),100000,y,x)
  acc[10*i] <- fitrw$accept
  learn_mean <- rbind(learn_mean,apply(fitrw$par[-c(1:40000),],2,mean))
}
acc
learn_mean
proposal <- list(var = fit$var,scale = 0.4)
fitrw <- rwmetrop(logpost = lupost,proposal = proposal,
                  start = t(fit$mode),100000,y,x)
(learn_mean <- apply(fitrw$par[-c(1:40000),],2,mean))
library(coda)
xyplot(mcmc(fitrw$par[-c(1:40000),]),col = 'black')

###################################
####   r2openBUGs
###################################
library(R2OpenBUGS)
model <- function(){
  for(i in 1:n){
    y0[i] ~ dbern(p[i])
    logit(p[i]) <- beta0 + beta1*GRE[i] + beta2*TOEFL[i] + beta3*UR[i] + beta4*SOP[i] + beta5*LOR[i] + beta6*CGPA[i] + beta7*RE[i]
  }
  beta0 ~ dnorm(0.0,1.0E-4)
  beta1 ~ dnorm(0.0,1.0E-4)
  beta2 ~ dnorm(0.0,1.0E-4)
  beta3 ~ dnorm(0.0,1.0E-4)
  beta4 ~ dnorm(0.0,1.0E-4)
  beta5 ~ dnorm(0.0,1.0E-4)
  beta6 ~ dnorm(0.0,1.0E-4)
  beta7 ~ dnorm(0.0,1.0E-4)
}
filename <- file.path("C:/Users/lenovo/Desktop", "model.txt")
write.model(model, filename)

datamcmc <- as.list.data.frame(trainmcmc)
datamcmc$n <- 351
datamcmc$y0 <- as.numeric(datamcmc$y0)-1
#inits <- as.data.frame(t(coefficients(out)))
#colnames(inits) <- paste("beta",0:7,sep = '')
inits <- function(){
  list(beta0=1,beta1=1,beta2=1,beta3=1,beta4=1,beta5=1,beta6=1,beta7=1)
}
parameters <- c(paste("beta",0:7,sep = ''))

mcmc.sim <- bugs(data = datamcmc,inits = inits,parameters.to.save = parameters,
                 model.file = 'C:/Users/lenovo/Desktop/model.txt',
                 n.chains = 1,n.iter = 100000,n.burnin = 40000,bugs.seed = 3,
                 debug = T,codaPkg = T)
print(mcmc.sim)
library(coda)
codaproject <- read.bugs(mcmc.sim)

##############################################
######       random walk
##############################################
out <- glm(y0 ~ GRE+TOEFL+UR+SOP+LOR+CGPA+RE, data = trainmcmc,
           family=binomial(link = "logit"))
beta_init <- as.numeric(coefficients(out))
x <- as.matrix(cbind(1,trainmcmc[,-8]))
dimnames(x) <- NULL
y <- as.numeric(trainmcmc$y0)-1
(sigma <- as.matrix(diag(rep(16,8))))
N <- 60000
rw.Metropolis <- function(init,x,y,sigma,N){
  # sigma: covariance matrix of proposal distribution 
  # init: initial value
  # x: sample of independent variables
  # y: sample of response variable
  # N: size of random numbers required.
  require(MASS)
  est <- data.frame(t(init))
  u <- runif(N)
  k <- 0
  lupost <- function(beta,ind,res){
    eta <- ind %*% beta
    p <- 1/(1 + exp(-eta))
    logl <- sum(log(p[res==1])) + sum(log(1-p[res==0]))
    return(exp(logl+sum(dnorm(beta,0,100,log = TRUE))))
  }

  for(i in 2:N){
    xnew <- mvrnorm(1,as.numeric(est[i-1,]),sigma)
    if(u[i-1]<=(lupost(xnew,x,y)/lupost(as.numeric(est[i-1,]),x,y))) 
      est[i,] <- xnew 
    else{
      est[i,] <- est[i-1,]
      k <- k + 1
    }
  }
  return(list(est=est, k=k))
}
rw <- rw.Metropolis(beta_init,x,y,sigma,N)

#rate of candidate points rejected
print((N-c(rw1$k, rw2$k, rw3$k, rw4$k))/N)
par(mfrow=c(2,2)) #display 4 graphs together
refline <- qt(c(.025, .975), df=n)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
  plot(rw[,j], type="l",
       xlab=bquote(sigma == .(round(sigma[j],3))),
       ylab="X", ylim=range(rw[,j]))
  abline(h=refline)
}
par(mfrow=c(1,1)) #reset to default
a <- c(.05, seq(.1, .9, .1), .95)
Q <- qt(a, n)
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
mc <- rw[501:N, ]
Qrw <- apply(mc, 2, function(x) quantile(x, a))
print(round(cbind(Q, Qrw), 3))
xtable::xtable(round(cbind(Q, Qrw), 3)) #latex format

########################################
# An example model file is given in:
#######################################
model.file <- system.file(package="R2OpenBUGS", "model", "schools.txt")
# Let's take a look:
file.show(model.file)
# Some example data (see ?schools for details):
data(schools)
schools
J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ("J", "y", "sigma.y")
inits <- function(){
  list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
       sigma.theta=runif(1, 0, 100))
}
## or alternatively something like:
# inits <- list(
# list(theta=rnorm(J, 0, 90), mu.theta=rnorm(1, 0, 90),
# sigma.theta=runif(1, 0, 90)),
# list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
# sigma.theta=runif(1, 0, 100))
# list(theta=rnorm(J, 0, 110), mu.theta=rnorm(1, 0, 110),
# sigma.theta=runif(1, 0, 110)))
parameters <- c("theta", "mu.theta", "sigma.theta")
## Not run:
## You may need to specify "OpenBUGS.pgm"
## also you need write access in the working directory:
schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains=3, n.iter=5000)
print(schools.sim)
plot(schools.sim)
######################## End(Not run)   #########################

#############################################
####   MCMC 预测
#############################################
cof_mcmc <- c(-4.922,0.222,0.968,1.456,-0.195,-0.197,4.894,0.655)
# 提取训练集数据
test_mcmc <- data.frame(cbind(rep(1,nrow(testinla)),testinla[,-8]));colnames(test_mcmc)[1] <- 'intercept' 
result_mcmc <- apply(t(apply(test_mcmc,1,function(x) cof_mcmc*x)),1,sum)
# 计算预测概率
prob_mcmc <- exp(result_mcmc)/(1+exp(result_mcmc))
# 预测二分类结果
pre_mcmc <- factor(ifelse(prob_mcmc>0.8,1,0))
ref_mcmc <- factor(testinla$y0)
conM.mcmc <- confusionMatrix(pre_mcmc,ref_mcmc,positive = '1')
conM.mcmc


###############################################################
########   glm logistic regression
###############################################################
out <- glm(y0 ~ GRE+TOEFL+UR+SOP+LOR+CGPA+RE, data = trainmcmc,
           family=binomial(link = "logit"))
cof_glm <- as.numeric(coefficients(out))
# 提取训练集数据
test_glm <- data.frame(cbind(rep(1,nrow(testinla)),testinla[,-8]))
colnames(test_glm)[1] <- 'intercept' 
result_glm <- apply(t(apply(test_glm,1,function(x) cof_glm*x)),1,sum)
# 计算预测概率
prob_glm <- exp(result_glm)/(1+exp(result_glm))
# 预测二分类结果
pre_glm <- factor(ifelse(prob_glm>0.8,1,0))
ref_glm <- factor(testinla$y0)
conM.glm <- confusionMatrix(pre_glm,ref_glm,positive = '1')
conM.glm


####################################################################
###########    下面的方法都针对data数据集，即y为二分类
####################################################################
# 标准化
scalepre <- preProcess(train[,-c(3,7)],method = c('center','scale'))
train1 <- predict(scalepre,train)
test1 <- predict(scalepre,test)
# 转换为因子型变量
train1$RE <- factor(train1$RE);test1$RE <- factor(test1$RE)
train1$UR <- factor(train1$UR);test1$UR <- factor(test1$UR)
str(train1)

##############################  
###    RandomForest
##############################
library(randomForest)
# 确定随机森林的最优参数
p <- ncol(train1)-1
err <- rep(0,p)
B <- rep(0,p)
for (i in 1:p){
  set.seed(2020)
  rfmodel <- randomForest(y0 ~ .,data=train1,ntree=500,mtry=i,nodesize=1)#,maxnodes=2^10)
  err[i] <- min(rfmodel$err.rate[,1])
  B[i] <- which.min(rfmodel$err.rate[,1])
}
err
B
mtry.optimal <- which.min(err)
ntree.optimal <- B[which.min(err)]
c(mtry.optimal,ntree.optimal)
#用最优参数建模
set.seed(2020)
model.rf <- randomForest(y0 ~ .,data=train1,ntree=500,mtry=mtry.optimal,nodesize=1,importance=T)#,proximity=T,maxnodes=2^10)
c(min(model.rf$err.rate[,1]),which.min(model.rf$err.rate[,1]))#发现加了importance=T后err.rate结果有所变化啦！
#importance=T是为了能给出变量相对重要性；proximity=T是提供cases之间的亲疏程度，便于后面用MDSplot函数予以呈现
# 预测
pred.rf.prob <- predict(model.rf,test1,type = 'prob')
pred.rf <- factor(ifelse(pred.rf.prob[,2]>0.8,1,0));levels(pred.rf) <- c('1','2')
conM.RF <- confusionMatrix(pred.rf,test1[,8],positive="2")
conM.RF
conM.RF$byClass
# 作图
#MDSplot(model.rf,fac = train1$y0)
plot(1:7,err,xlab = 'mtry',type = 'b',ylab = 'OOB',main = 'OOB for different mtry')
plot(model.rf,col=c("red","blue","green"))#看树的个数与OOB错误率的关系,红色表示整体的OOB错误率，蓝色和绿色分别表示是“no”和"yes"两类的OOB错误率
legend("topright",c("Total","0","1"),lty=c(1,2,2),col=c("red","blue","green"))
#model.rf$importance#各变量的重要程度
varImpPlot(model.rf, main="Variable Importance Random Forest audit")#作图


##############################
###    XGboost
##############################
library(xgboost)
# xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx <- model.matrix(~.,data=train1[,-8])[,-1]
trainy <- as.numeric(as.factor(train1[,8]))-1 #将因变量转化为numeric,且取值为0和1
testx<- model.matrix(~.,data=test1[,-8])[,-1]
param <- list(seed=2020,objective="binary:logistic",subsample = 1,max_depth=5,
              colsample_bytree = 1,min_child_weight=.1,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=30)
#获得特征的真实名称
(names <- dimnames(data.matrix(trainx))[[2]])
#计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = model.xgb)
#变量重要性作图
xgb.plot.importance(importance_matrix)
#预测
pred.xgb <- predict(model.xgb,testx)
pred.xgboost <- ifelse(pred.xgb>0.8,1,0)
pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')
con.XB <- confusionMatrix(pred.xgboost,test1$y,positive = '2')
con.XB
con.XB$byClass


##############################
###    KNN
##############################
library(kknn)
#留一交叉验证确定最优参数 留一交叉 -> train.kknn,欧氏距离distance = 2
#ptm <- proc.time()    # 开始计算时间
model.best.kknn <- train.kknn(y0 ~ .,train1,kmax = 50,
                              kernel = c("rectangular","triangular","epanechnikov","biweight","triweight",
                                         "cos", "inv", "gaussian","rank","optimal"),distance=2,scale=T)
# 设定kmax为50，即从1到50进行挑选，kernel选取了帮助文档中所有出现的权重函数
plot(model.best.kknn)
#head(model.best.kknn$MISCLASS) # 显示错误率
model.best.kknn # 输出最优参数情况
# 用最优参数对测试集进行预测
model.test.kknn <- kknn(y0 ~ .,train1,test1[,-8],k=model.best.kknn$best.parameters$k,
                        scale=T,distance=2,kernel=model.best.kknn$best.parameters$kernel)
#proc.time() - ptm   # 结束计算时间
#table(testy,model.test.kknn$fitted.values) # 混淆矩阵
pre.knn <- factor(ifelse(model.test.kknn$prob[,2]>0.8,1,0));levels(pre.knn) <- c('1','2')
conM.KNN <- confusionMatrix(pre.knn,test1[,8],positive = '2') # 基于混淆矩阵评价模型
conM.KNN
conM.KNN$byClass
# ROC AUC
library(pROC)
KNN.roc <- roc(test1$y0,model.test.kknn$prob[,2],levels = c('1','2'))
KNN.roc$auc


#####################################################################
#########              模型效果比较 
#####################################################################
library(ROCR)
#pr.MC <- prediction(prob_mcmc,test1$y0)
pr.IN <- prediction(testprob,test1$y0)
pr.KN <- prediction(model.test.kknn$prob[,2],test1$y0)
pr.RF <- prediction(pred.rf.prob[,2],test1$y0)
pr.XG <- prediction(pred.xgb,test1$y0)
pred11 <- performance(pr.IN,'auc');pred12 <- performance(pr.KN,'auc')
pred13 <- performance(pr.RF,'auc');pred14 <- performance(pr.XG,'auc')
#pred15 <- performance(pr.MC,'auc')
unlist(pred11@y.values);unlist(pred12@y.values)
unlist(pred13@y.values);unlist(pred14@y.values)
#unlist(pred15@y.values) #AUC值

#敏感度-特异度曲线
#perf.MC.ss <- performance(pr.MC,"sens","spec")
perf.IN.ss <- performance(pr.IN,"sens","spec")
perf.KN.ss <- performance(pr.KN,"sens","spec")
perf.RF.ss <- performance(pr.RF,"sens","spec")
perf.XG.ss <- performance(pr.XG,"sens","spec")
par(bg='grey92')
plot(perf.IN.ss,col=1)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
#plot(perf.MC.ss,col=6,add=T)
plot(perf.KN.ss,col=3,add=T)
plot(perf.RF.ss,col=4,add=T)
plot(perf.XG.ss,col=5,add=T)
legend("bottomleft",c("INLA","KNN","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)

ssdata <- data.frame("x" = c(perf.IN.ss@x.values[[1]],perf.KN.ss@x.values[[1]],
                           perf.RF.ss@x.values[[1]],perf.XG.ss@x.values[[1]]),
                     "y" = c(perf.IN.ss@y.values[[1]],perf.KN.ss@y.values[[1]],
                             perf.RF.ss@y.values[[1]],perf.XG.ss@y.values[[1]]),
                     "Method" = rep(c("INLA","KNN",'RF',"XG"),times = c(150,12,69,94)))
ggplot(ssdata,aes(x=x,y=y,col=Method)) + geom_line() + labs(x = 'Sensitivity',y = 'Specificity')

#查全率-查准率曲线
#perf.MC.pr <- performance(pr.MC,"prec","rec")
perf.IN.pr <- performance(pr.IN,"prec","rec")
perf.KN.pr <- performance(pr.KN,"prec","rec")
perf.RF.pr <- performance(pr.RF,"prec","rec")
perf.XG.pr <- performance(pr.XG,"prec","rec")
par(bg='grey92')
plot(perf.IN.pr,col=1)
plot(perf.KN.pr,col=3,add=T)
plot(perf.RF.pr,col=4,add=T)
plot(perf.XG.pr,col=5,add=T)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomleft",c("INLA","KNN","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)

prdata <- data.frame("x" = c(perf.IN.pr@x.values[[1]],perf.KN.pr@x.values[[1]],
                             perf.RF.pr@x.values[[1]],perf.XG.pr@x.values[[1]]),
                     "y" = c(perf.IN.pr@y.values[[1]],perf.KN.pr@y.values[[1]],
                             perf.RF.pr@y.values[[1]],perf.XG.pr@y.values[[1]]),
                     "Method" = rep(c("INLA","KNN",'RF',"XG"),times = c(150,12,69,94)))
ggplot(prdata,aes(x=x,y=y,col=Method)) + geom_line() + labs(x = "Recall",y = 'Precision')

#ROC
#perf.MC.roc <- performance(pr.MC,"tpr","fpr")
perf.IN.roc <- performance(pr.IN,"tpr","fpr")
perf.KN.roc <- performance(pr.KN,"tpr","fpr")
perf.RF.roc <- performance(pr.RF,"tpr","fpr")
perf.XG.roc <- performance(pr.XG,"tpr","fpr")
par(bg='grey92')
plot(perf.IN.roc,col=1)
plot(perf.KN.roc,col=3,add=T)
plot(perf.RF.roc,col=4,add=T)
plot(perf.XG.roc,col=5,add=T)
abline(0,1,lty = 4)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomright",c("INLA","KNN","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)

rocdata <- data.frame("x" = c(perf.IN.roc@x.values[[1]],perf.KN.roc@x.values[[1]],
                              perf.RF.roc@x.values[[1]],perf.XG.roc@x.values[[1]]),
                      "y" = c(perf.IN.roc@y.values[[1]],perf.KN.roc@y.values[[1]],
                              perf.RF.roc@y.values[[1]],perf.XG.roc@y.values[[1]]),
                      "Method" = rep(c("INLA","KNN",'RF',"XG"),times = c(150,12,69,94)))
ggplot(rocdata,aes(x=x,y=y,col=Method)) + geom_line() + 
  labs(x = 'False positive rate',y = 'True positive rate') + 
  scale_color_discrete(breaks = c('INLA','KNN','RF','XG'),
                       labels = c('INLA:0.985','KNN:0.978','RF:0.982','XG:0.977'))

#提升图
#perf.MC.lr <- performance(pr.MC,"lift","rpp")
perf.IN.lr <- performance(pr.IN,"lift","rpp")
perf.KN.lr <- performance(pr.KN,"lift","rpp")
perf.RF.lr <- performance(pr.RF,"lift","rpp")
perf.XG.lr <- performance(pr.XG,"lift","rpp")
par(bg='grey92')
plot(perf.IN.lr,col=1)
plot(perf.KN.lr,col=3,add=T)
plot(perf.RF.lr,col=4,add=T)
plot(perf.XG.lr,col=5,add=T)
abline(h = seq(0,4,.5),v = seq(0,1,.2),col = 'white')
legend("topright",c("INLA","KNN","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)

lrdata <- data.frame("x" = c(perf.IN.lr@x.values[[1]],perf.KN.lr@x.values[[1]],
                             perf.RF.lr@x.values[[1]],perf.XG.lr@x.values[[1]]),
                     "y" = c(perf.IN.lr@y.values[[1]],perf.KN.lr@y.values[[1]],
                             perf.RF.lr@y.values[[1]],perf.XG.lr@y.values[[1]]),
                     "Method" = rep(c("INLA","KNN",'RF',"XG"),times = c(150,12,69,94)))
ggplot(lrdata,aes(x=x,y=y,col=Method)) + geom_line() + labs(x = 'Rate of positive predictions',y = 'Lift value')








#qq <- function(x){
#  return(names(which.max(table(x))))
#}
#x <- rbind(rep(c('red','green','blue'),c(4,5,3)),rep(c('red','green','blue'),c(3,4,5)))
#re <- apply(x,1,qq)
#re

















