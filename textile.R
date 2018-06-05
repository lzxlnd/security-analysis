
library(dplyr)



tex<-na.omit(read.csv("C:/Users/xinjun li/Desktop/证券市场与投资/纺织/TRDweek.csv")) #读入数据 

tex$Stkcd<-as.factor(tex$Stkcd)    #修改列格式为factor
tex$Trdwnt<-as.factor(tex$Trdwnt) 
summary(tex)     #查看数据

#samp_com<-sample(levels(tex$Stkcd),15) #random sample 15 companies
# OR
avg<-tapply(tex$Wretnd,tex$Stkcd,mean)

samp_com<-which(avg>=0)%>%levels(tex$Stkcd)[.]
#samp_com<-tail(order(avg),n=15)%>%levels(tex$Stkcd)[.] #选取均值最大的n支股票

ndate<-levels(tex$Trdwnt)     #存储所有日期



x<-data.frame(matrix(as.numeric(NA),nrow = length(ndate),ncol = length(samp_com)))    #建立矩阵，由于存储筛选数据

names(x)<-samp_com       #命名x的列
row.names(x)<-ndate      #命名x的行
#samp_com<-as.factor(samp_com)
#ndate<-as.factor(ndate)

for (i in 1:length(ndate)) {
  for (j in 1:length(samp_com)) {
    if(any(tex$Stkcd==samp_com[j]&tex$Trdwnt==ndate[i])) #判断是否存在满足筛选日期和股票代码的数据
      x[i,j]<-tex$Wretnd[tex$Stkcd==samp_com[j]&tex$Trdwnt==ndate[i]]
  }
}

#进一步删选股票
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

