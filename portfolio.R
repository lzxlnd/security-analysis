library(ggplot2)  
library(mcga)    #遗传算法的包   
library(dplyr)



set.seed(1)


setwd("C:/Users/xinjun li/Desktop/证券市场与投资")

source("textile.R")   #此文件用于处理数据和选取公司,返回值x为dataframe



n<-as.numeric(ncol(x)) #n是选取公司数
r0<-1.015^(7/365)-1   #无风险利率

mu<-sapply(1:n,function(k) mean(x[,k],na.rm = TRUE))
#mu是每家公司的期望回报率

va<-cov(x,use = "complete.obs")
#n家公司的协方差矩阵

slop<-function(z){            #计算斜率，z是权重向量
  y<-as.matrix(z)/sum(z)
  sigma<-sqrt(t(y)%*%va%*%y)  #计算方差
  sl<-(mu%*%y-r0)/sigma
  return(sl)
}


si_mu<-function(z){           #计算投资组合的期望回报和方差
  y<-as.matrix(z)/sum(z)
  sigma<-sqrt(t(y)%*%va%*%y)
  return(c(sigma,mu%*%y))
}

U<-function(x,A=1){   #定义效用函数，其中x=c(sigma,mu)
  x[2]-1/2*A*x[1]^2
}




# 运行遗传算法
pop=1000
m <- mcga(   popsize=pop,        #种群大小
             chsize=n, 
             minval=-1, 
             maxval=1, 
             maxiter=1000,        #迭代次数
             crossprob=.9, 
             mutateprob=0.01, 
             evalFunc=function(z){-slop(z)})


wei0<-sapply(1:pop,function(k)as.matrix(m$population[k,])/sum(m$population[k,]))    #存储population权重

opt<-optim(as.matrix(m$population[1,])/sum(m$population[1,]),function(z){-slop(z)})



# 运行遗传算法

m <- mcga(   popsize=pop,        #种群大小
             chsize=n, 
             minval=-1, 
             maxval=1, 
             maxiter=1,        #迭代次数
             crossprob=.9, 
             mutateprob=0.01, 
             evalFunc=function(z){-slop(z)})


wei<-sapply(1:pop,function(k)as.matrix(m$population[k,])/sum(m$population[k,]))    #存储population权重

wei<-cbind(opt$par,wei)
pop<-pop+1

# 最优化的个体结果
sl<- slop(wei[,1])


#存储population中的点
dot<-sapply(1:pop,function(k) si_mu(wei[,k]))
dot<-data.frame(t(dot))
dot<-cbind(dot,data.frame(c("optimal",rep("portfolio",pop-1))))
names(dot)<-c("risk","return","class")

#存储原始数据点（各公司的数据）
xdot<-sapply(1:n,function(k)c(sqrt(va[k,k]),mu[k]))
xdot<-data.frame(t(xdot))
xdot<-cbind(xdot,data.frame(rep("stock",n)))
names(xdot)<-c("risk","return","class")
dot<-rbind(dot[dot$class=="portfolio",],xdot,dot[1,])


#作图
plot<-ggplot(data = dot,aes(x=dot$risk,y=dot$return))+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+xlab("risk")+ylab("return")+ggtitle("投资组合")+xlim(0,.2)+ylim(-.03,.05)  

plot<-plot+geom_abline(intercept = r0,slope = sl,color="brown",size=1)+geom_point(aes(color=class,shape=class,label=dot$class),size=2)


plot            #画图

sl              #斜率
wei[,1]         #最优解的权重
si_mu(wei[,1])  #最优解点的位置(方差,期望回报率)

A=10
maxu<-optimise(function(s){U(c(s,sl*s+r0),A)},c(0,.2),maximum = TRUE)
names(maxu)<-c("sigma","maxU")
maxu

mpoint<-data.frame(t(c(maxu$sigma,sl*maxu$sigma+r0)))

f=function(z){maxu$maxU+1/2*A*z^2}

plot+stat_function(fun=f,col="orange",size=1.2,linetype=4)+geom_point(data=mpoint,aes(x=mpoint$X1,y=mpoint$X2))+geom_text(aes(x=.08,y=.05,label="indifference curve"),color="orange")   

#以下用于描述投资组合可能边界
dot0<-sapply(1:pop,function(k) si_mu(wei0[,k]))
dot0<-data.frame(t(dot0))
dot0<-cbind(dot0,data.frame(c("optimal",rep("portfolio",pop-1))))
names(dot0)<-c("risk","return","class")

q<-quantile(dot0$risk,c(seq(0,.8,.05),seq(.81,1,.01)))#seq(0,1,.025))#,
bond<-dot0[dot0$risk==min(dot0$risk),-3]

for (i in 2:length(q)) {
  m<-max(dot0$return[dot0$risk>=q[i-1]&dot0$risk<=q[i]])
  bond<-rbind(bond,c(dot0$risk[dot0$return==m],m))
  
}
#bond<-rbind(bond,dot0[dot0$risk==min(dot0$risk),-3])

plot+geom_path(data=bond,aes(x=bond$risk,y=bond$return),color="yellow",size=1)#method = "lm",formula=y~sqrt(x),se=FALSE,

return(x)
return(wei)
