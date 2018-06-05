
library(dplyr)



tex<-na.omit(read.csv("C:/Users/xinjun li/Desktop/֤ȯ�г���Ͷ��/��֯/TRDweek.csv")) #�������� 

tex$Stkcd<-as.factor(tex$Stkcd)    #�޸��и�ʽΪfactor
tex$Trdwnt<-as.factor(tex$Trdwnt) 
summary(tex)     #�鿴����

#samp_com<-sample(levels(tex$Stkcd),15) #random sample 15 companies
# OR
avg<-tapply(tex$Wretnd,tex$Stkcd,mean)

samp_com<-which(avg>=0)%>%levels(tex$Stkcd)[.]
#samp_com<-tail(order(avg),n=15)%>%levels(tex$Stkcd)[.] #ѡȡ��ֵ����n֧��Ʊ

ndate<-levels(tex$Trdwnt)     #�洢��������



x<-data.frame(matrix(as.numeric(NA),nrow = length(ndate),ncol = length(samp_com)))    #�����������ڴ洢ɸѡ����

names(x)<-samp_com       #����x����
row.names(x)<-ndate      #����x����
#samp_com<-as.factor(samp_com)
#ndate<-as.factor(ndate)

for (i in 1:length(ndate)) {
  for (j in 1:length(samp_com)) {
    if(any(tex$Stkcd==samp_com[j]&tex$Trdwnt==ndate[i])) #�ж��Ƿ��������ɸѡ���ں͹�Ʊ���������
      x[i,j]<-tex$Wretnd[tex$Stkcd==samp_com[j]&tex$Trdwnt==ndate[i]]
  }
}

#��һ��ɾѡ��Ʊ
y=0
for (i in 1:ncol(x)) {
  if(sum(is.na(x[,i]))>nrow(x)/3)
    if(y==0)
      y=-i
    else
      y<-c(y,-i)
}

if(y!=0)x<-x[y]
sort(apply(x,2,function(k)mean(k,na.rm = TRUE)),decreasing = TRUE)
sort(apply(x,2,function(k)var(k,na.rm = TRUE)))

samp_com<-names(x)
samp_com

# y=0
# for (i in 1:nrow(x)) {
#   if(all(is.na(x[i,])))
#     if(y==0)
#       y=-i
#     else
#       y<-c(y,-i)
# }
# x<-x[y,]

return(x)  #return x as data frame
