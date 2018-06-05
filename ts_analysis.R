#library('forecast') #����"tseries"��  
#library('tseries') #����"forecast"��  


set.seed(1)  #�趨���Ϊ1����������ӣ�Ŀ�����´��ظ�ʱ����ͬ���������  

setwd("C:/Users/xinjun li/Desktop/֤ȯ�г���Ͷ��")

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
# mean(y) #�����ֵE(yt)  
# var(y) #���㷽��Var(yt)  
# 
# 
# plot.ts(y, col="blue", main="y������ʱ������ͼ", xlab="t", ylab="y")  
# 
# 
# adf.test(y, alternative="stationary")  #adf�����鶨  
# 
# 
# Acf(y, main='y-AC')  #�������ͼ  
# Pacf(y, main='y-PAC')  #��ƫ�����ͼ  
# 
# y.arima<- Arima(y, order=c(1,1,0)) #��Arimaָ�����ʱ�����лع飬arima�е�order������order(p,d,q������(e)�������p=1  
# summary(y.arima)  