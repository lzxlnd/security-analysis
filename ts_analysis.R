#library('forecast') #调出"tseries"包  
#library('tseries') #调出"forecast"包  


set.seed(1)  #设定编号为1的随机数种子，目的是下次重复时生成同样的随机数  

setwd("C:/Users/xinjun li/Desktop/证券市场与投资")

source("portfolio.R")

# y<-apply(x,1,function(k)weighted.mean(k,wei[,1],na.rm = TRUE))

z<-as.data.frame(matrix(nrow = 2,ncol = ncol(x)))
names(z)<-names(x)

for(i in 1:ncol(x)){
  y<-ts(na.omit(x[,i]))
  ret_ar<-ar(y,method = 'ols', order.max = 3)
  z[,i]<-predict(ret_ar,y,n.ahead=2,se.fit = FALSE)
  seqplot.ts(y,z[,i])
}


pre<-apply(z,1,function(k)weighted.mean(k,wei[,1]))
pre


# 
# mean(y) #计算均值E(yt)  
# var(y) #计算方差Var(yt)  
# 
# 
# plot.ts(y, col="blue", main="y变量的时间序列图", xlab="t", ylab="y")  
# 
# 
# adf.test(y, alternative="stationary")  #adf单根验定  
# 
# 
# Acf(y, main='y-AC')  #作自相关图  
# Pacf(y, main='y-PAC')  #作偏自相关图  
# 
# y.arima<- Arima(y, order=c(1,1,0)) #用Arima指令进行时间序列回归，arima中的order参数是order(p,d,q），由(e)结果看出p=1  
# summary(y.arima)  
