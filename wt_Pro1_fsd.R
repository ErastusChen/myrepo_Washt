data0<-IOTXUSDT.trades.2021.06
qty<-data0[,3]
# 定义一个函数，用于获取某个数最高位的数字
fun <- function(a){
  while(a >= 10){a=a%/%10}
  a
}
list<-c(0,0,0,0,0,0,0,0,0)
inte<-c(1,2,3,4,5,6,7,8,9)
for(i in inte){
  for(value in qty){
    if(fun(value)==i){
      list[i]=list[i]+1}
  }
}
fre<-list/length(qty)
# 绘制IOTX/第一有效数字分布的条形图与散点图
barplot(fre,names.arg=inte,xlab="First Significant Digit",ylab="IOTX/USDT",col="#99CCFF",
        main="Figure  1.  First significant digit Distribution and Benford's Law",border="black")
b_law<-c(0.301,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046) 
plot(inte,b_law,main="Figure  1.  First significant digit Distribution and Benford's Law",
     xlab="First Significant Digit",ylab="IOTX/USDT", pch=19) # 本福特定律散点分布

library(ggplot2)
p<-ggplot(data=fsd,aes(x=inte,y=fre,color="#99CCFF"))
p+geom_bar(stat='identity')+labs(x="First Significant Digit",y="IOTX/USDT",
           title="Figure  1.  First significant digit Distribution and Benford's Law")
#+xlim(seq(1,9))+ylim(seq(1,9))
# +geom_point(position="jitter",col=2,pch=16,cex=1)
par(new=TRUE)
p+geom_point(stat='identity')+labs(x="First Significant Digit",y="IOTX/USDT")
#+xlim(seq(1,9))+ylim(seq(1,9))
# 卡方检验——是否遵从本福特定律
chisq.test(fre,b_law) # 卡方检验可能不准确
fisher.test(fre,b_law) #使用fisher检验