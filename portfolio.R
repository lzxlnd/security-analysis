library(ggplot2)  
library(mcga)    #�Ŵ��㷨�İ�   
library(dplyr)



set.seed(1)


setwd("C:/Users/xinjun li/Desktop/֤ȯ�г���Ͷ��")

source("textile.R")   #���ļ����ڴ������ݺ�ѡȡ��˾,����ֵxΪdataframe



n<-as.numeric(ncol(x)) #n��ѡȡ��˾��
r0<-1.015^(7/365)-1   #�޷�������

mu<-sapply(1:n,function(k) mean(x[,k],na.rm = TRUE))
#mu��ÿ�ҹ�˾�������ر���

va<-cov(x,use = "complete.obs")
#n�ҹ�˾��Э�������

slop<-function(z){            #����б�ʣ�z��Ȩ������
  y<-as.matrix(z)/sum(z)
  sigma<-sqrt(t(y)%*%va%*%y)  #���㷽��
  sl<-(mu%*%y-r0)/sigma
  return(sl)
}


si_mu<-function(z){           #����Ͷ����ϵ������ر��ͷ���
  y<-as.matrix(z)/sum(z)
  sigma<-sqrt(t(y)%*%va%*%y)
  return(c(sigma,mu%*%y))
}

U<-function(x,A=1){   #����Ч�ú���������x=c(sigma,mu)
  x[2]-1/2*A*x[1]^2
}




# �����Ŵ��㷨
pop=1000
m <- mcga(   popsize=pop,        #��Ⱥ��С
             chsize=n, 
             minval=-1, 
             maxval=1, 
             maxiter=1000,        #��������
             crossprob=.9, 
             mutateprob=0.01, 
             evalFunc=function(z){-slop(z)})


wei0<-sapply(1:pop,function(k)as.matrix(m$population[k,])/sum(m$population[k,]))    #�洢populationȨ��

opt<-optim(as.matrix(m$population[1,])/sum(m$population[1,]),function(z){-slop(z)})



# �����Ŵ��㷨

m <- mcga(   popsize=pop,        #��Ⱥ��С
             chsize=n, 
             minval=-1, 
             maxval=1, 
             maxiter=1,        #��������
             crossprob=.9, 
             mutateprob=0.01, 
             evalFunc=function(z){-slop(z)})


wei<-sapply(1:pop,function(k)as.matrix(m$population[k,])/sum(m$population[k,]))    #�洢populationȨ��

wei<-cbind(opt$par,wei)
pop<-pop+1

# ���Ż��ĸ�����
sl<- slop(wei[,1])


#�洢population�еĵ�
dot<-sapply(1:pop,function(k) si_mu(wei[,k]))
dot<-data.frame(t(dot))
dot<-cbind(dot,data.frame(c("optimal",rep("portfolio",pop-1))))
names(dot)<-c("risk","return","class")

#�洢ԭʼ���ݵ㣨����˾�����ݣ�
xdot<-sapply(1:n,function(k)c(sqrt(va[k,k]),mu[k]))
xdot<-data.frame(t(xdot))
xdot<-cbind(xdot,data.frame(rep("stock",n)))
names(xdot)<-c("risk","return","class")
dot<-rbind(dot[dot$class=="portfolio",],xdot,dot[1,])


#��ͼ
plot<-ggplot(data = dot,aes(x=dot$risk,y=dot$return))+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+xlab("risk")+ylab("return")+ggtitle("Ͷ�����")+xlim(0,.2)+ylim(-.03,.05)  

plot<-plot+geom_abline(intercept = r0,slope = sl,color="brown",size=1)+geom_point(aes(color=class,shape=class,label=dot$class),size=2)


plot            #��ͼ

sl              #б��
wei[,1]         #���Ž��Ȩ��
si_mu(wei[,1])  #���Ž���λ��(����,�����ر���)

A=10
maxu<-optimise(function(s){U(c(s,sl*s+r0),A)},c(0,.2),maximum = TRUE)
names(maxu)<-c("sigma","maxU")
maxu

mpoint<-data.frame(t(c(maxu$sigma,sl*maxu$sigma+r0)))

f=function(z){maxu$maxU+1/2*A*z^2}

plot+stat_function(fun=f,col="orange",size=1.2,linetype=4)+geom_point(data=mpoint,aes(x=mpoint$X1,y=mpoint$X2))+geom_text(aes(x=.08,y=.05,label="indifference curve"),color="orange")   

#������������Ͷ����Ͽ��ܱ߽�
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