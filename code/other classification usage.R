# 加载包
library(dplyr)
library(stringr)
library(reshape)

 # data process
resample <- read.csv('resample.csv',encoding='UTF-8') # data = resample
profit <- read.csv('month_profit_resample.csv',encoding='UTF-8')
ts<-substring(profit$ts,1,10)
prof<-profit$cash_profit
asset_id<-profit$asset_id
re<-cbind(ts,prof,asset_id)
re<-data.frame(re)

sum(is.na(re[,2])) # 统计re中含有NA值的个数
re<-na.omit(re) # 剔除缺失值

# ------------------------------ 其他行业分类 -------------------------------- #

##  提取数据

# ------------------------------------ 中信证券行业分类
profit <- read.csv('month_profit_resample.csv',encoding='UTF-8')
ts<-substring(profit$ts,1,10)
prof<-profit$cash_profit
asset_id<-profit$asset_id
re<-cbind(ts,prof,asset_id)
re<-data.frame(re)

sum(is.na(re[,2])) # 统计re中含有NA值的个数
re<-na.omit(re) # 剔除缺失值

class<-filter(resample,resample$classification_id=='P0212')# choose the portfolio 在中信证券行业分类下（zx240102）
class$industry_id <- substring(class$industry_id,3,9) # 选取数字


sid<-intersect(re$asset_id,class$asset_id)
class<-class[which(class$asset_id %in% sid),] 
re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集

ind_4<-substring(class$industry_id,1,4)
ind_2<-substring(class$industry_id,1,2)
class<-cbind(class,ind_4,ind_2)    # 得到四位和二位下的行业分类

data<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间

# -------------- 中信证券行业分类2.0版本（从2018.12月开始，数据太少，放弃）


# ----------------- 中证行业分类
profit <- read.csv('month_profit_resample.csv',encoding='UTF-8')
ts<-substring(profit$ts,1,10)
prof<-profit$cash_profit
asset_id<-profit$asset_id
re<-cbind(ts,prof,asset_id)
re<-data.frame(re)

sum(is.na(re[,2])) # 统计re中含有NA值的个数
re<-na.omit(re) # 剔除缺失值

class <- filter(resample,resample$classification_id=='P0206')# choose the portfolio 在中证行业分类下有四级行业分类（8个数字）从2012-05月开始


sid<-intersect(re$asset_id,class$asset_id)
class<-class[which(class$asset_id %in% sid),] 
re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集

ind_6 <- substring(class$industry_id,1,6)
ind_4<-substring(class$industry_id,1,4)
ind_2<-substring(class$industry_id,1,2)
class<-cbind(class,ind_6,ind_4,ind_2)    # 得到六位、四位和二位下的行业分类

data<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间


# --------------------------- 证监会行业分类2012年版
profit <- read.csv('month_profit_resample.csv',encoding='UTF-8')
ts<-substring(profit$ts,1,10)
prof<-profit$cash_profit
asset_id<-profit$asset_id
re<-cbind(ts,prof,asset_id)
re<-data.frame(re)

sum(is.na(re[,2])) # 统计re中含有NA值的个数
re<-na.omit(re) # 剔除缺失值

class <- filter(resample,resample$classification_id=='P0207')# choose the portfolio 证监会分类下只有两类--字母和数字
sid<-intersect(re$asset_id,class$asset_id)
class<-class[which(class$asset_id %in% sid),] 
re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集

ind1 <- substring(class$industry_id,1,1)
class<-cbind(class,ind1)    # 得到三位分类
data_1<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间




## 删除数据后对ST股和借壳上市的股票进行处理


# 剔除2012-2019年间的ST股
# spt<-read.csv('SPT_Trdchg.csv',header = F,quote = "")
a<-sort(spt[,1])
index<-duplicated(a)
a_del<-a[!index] # not delete ,only print the stock id 
a<-a_del[-(908:957)]
a<-substring(a,2,7)
a<-as.integer(a)
a<-na.omit(a)
data_new<-filter(data_1,!(data_1$asset_id %in% a))  

#t <- data_new %>% filter(ts<'2019-02' & ts >'2012-06')
#b <- data %>% filter(ts<'2019-02' & ts >'2012-06')
#nrow(b)-nrow(t)


# 剔除在2012-2019年间借壳上市的股票
# eripo<-read.csv('ER_IPO.csv')
a<-eripo %>% filter(LatestBackdoorDate<'2019-02'&LatestBackdoorDate>'2012')
data_new<-filter(data_new,!(data_new$asset_id %in% a)) 

#length(a)

#b <- data_new %>% filter(ts<'2019-02' & ts >'2012-06')
#nrow(t)-nrow(b)


# 剔除收益过大或过小的股票,对数据赋值logprof
data_new$logprof<-log(1+as.numeric(data_new$prof))
data_new<-data_new %>% filter(logprof>=-1 & logprof<=1)


#a <- data_new %>% filter(ts<'2019-02' & ts >'2012-06')
#nrow(b)-nrow(a)


# ----------------- 运行 -------------------- #
return_20<-list()
return_21<-list()
#ldata<-data
#ldata$prof<-log(1+as.numeric(ldata$prof))

for(k in 1:60)
{
  print(k)
  # 中信证券行业分类从 2015-08-01 开始，num选择 5--六位；7--四位 ； 8--两位
  # 中证行业分类num 有三种选择：num = 5 --8位 ； 7--六位 ； 8--四位 ； 9--两位
  # 证监会只有两种，三位分类（5）和 一位分类（7）
  # 申银万国行业分类2014版，num选择 5--六位；7--四位 ； 8--两位
  re<-Data_Process(data_new,num = 9,mon = k)
  cat('observations:',nrow(re),'\n')
  cat('firms are:',nrow(re)/21,'\n')
  re_20<-Regression(re,1,k)   # condition = 1 - the first 20 months  -- i th month
  re_21<-Regression(re,2,k)
  return_20[[k]]<-re_20
  return_21[[k]]<-re_21
}


# 按顺序合并
a<-c()
for(i in 1:60)
{
  if(i == 1)
    a<-return_21[[i]]
  else
    a<-rbind(a,return_21[[i]])       
}

a$logre_ind <- log(1+a$re_ind)
a$logre_market <- log(1+a$re_market)

write.csv(a,'/lanec2_home/zhangx/R/zz 2 digits for last 20 month.csv')


summary(a)

b<-c()
for(i in s)
{
  b<-append(b,sd(a[,i]))
}

# 中证 11，21，22，18，19，20
# 证监会 9，20，19，16，17，18
# 申银万国 10，20，21，17，18，19
cor(a[,c(9,20,19,16,17,18)],method = 'pearson')
cor(a[,c(9,20,19,16,17,18)],method = 'spearman')

par(mfrow=c(3,3))
h<-hist(a$beta2,col='blue')
xfit<-seq(min(a$beta2),max(a$beta2),length=40)
yfit<-dnorm(xfit,mean=mean(a$beta2),sd=sd(a$beta2))
yfit<-yfit*diff(h$mids[1:2])*length(a$beta2)
lines(xfit,yfit,col='red',lwd=2)

h<-hist(a$beta1,col='blue')
xfit<-seq(min(a$beta1),max(a$beta1),length=40)
yfit<-dnorm(xfit,mean=mean(a$beta1),sd=sd(a$beta1))
yfit<-yfit*diff(h$mids[1:2])*length(a$beta1)
lines(xfit,yfit,col='red',lwd=2)

x<-a$logprof
h<-hist(x,col='blue',main='logprof')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-log(1+a$re_ind)
h<-hist(x,col='blue',main='log re_ind')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-log(1+a$re_market)
h<-hist(x,col='blue',main='log re_market')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-a$res2a
h<-hist(x,col='blue',main='res_2a')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-a$res2b
h<-hist(x,col='blue',main='res_2b')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-a$MktReturn
h<-hist(x,col='blue',main='MKtReturn')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-a$IndReturn
h<-hist(x,col='blue',main='IndReturn')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)


# 只取最后一个月的数据
aa<-data.frame()
for(i in 1:60)         # 60个窗口
{
  l<-nrow(return_20[[i]])/20         # l 个公司
  for(j in 1:l)
  {
    if(j==1)
      a<-return_20[[i]][(20*j),] 
    else
      a<-rbind(a,return_20[[i]][(20*j),])
  }
  aa <-rbind(aa,a)
}

aa$logre_ind <- log(1+aa$re_ind)
aa$logre_market <- log(1+aa$re_market)

summary(aa)

 # 中证 11，21，22，18，19，20
 # 证监会 9，20，19，16，17，18
 # 申银万国 10，20，21，17，18，19
cor(aa[,c(9,20,19,16,17,18)],method = 'pearson')
cor(aa[,c(9,20,19,16,17,18)],method = 'spearman')



# write.csv(aa,'test.csv')

par(mfrow=c(3,3))
h<-hist(aa$beta2,col='blue')
xfit<-seq(min(a$beta2),max(a$beta2),length=40)
yfit<-dnorm(xfit,mean=mean(a$beta2),sd=sd(a$beta2))
yfit<-yfit*diff(h$mids[1:2])*length(a$beta2)
lines(xfit,yfit,col='red',lwd=2)

h<-hist(aa$beta1,col='blue')
xfit<-seq(min(a$beta1),max(a$beta1),length=40)
yfit<-dnorm(xfit,mean=mean(a$beta1),sd=sd(a$beta1))
yfit<-yfit*diff(h$mids[1:2])*length(a$beta1)
lines(xfit,yfit,col='red',lwd=2)

x<-aa$logprof
h<-hist(x,col='blue',main='logprof')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-log(1+aa$re_ind)
h<-hist(x,col='blue',main='log re_ind')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-log(1+aa$re_market)
h<-hist(x,col='blue',main='log re_market')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-aa$res2a
h<-hist(x,col='blue',main='res_2a')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-aa$res2b
h<-hist(x,col='blue',main='res_2b')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-aa$MktReturn
h<-hist(x,col='blue',main='MKtReturn')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)

x<-aa$IndReturn
h<-hist(x,col='blue',main='IndReturn')
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit,yfit,col='red',lwd=2)




# ------summary 的值写成表格的形式 ---------- #

#----计算summary------#
# 中证 11，21，22，18，19，20，17，16，15，14
# 证监会 9，20，19，16，17，18，15，14，13，12
# 申银万国 10，20，21，17，18，19，16，15，14，13
# 中信证券行业分类
#s <- c(12,22,23,15,16,17,18,19,20,21)
#s <- c(10,20,21,13,14,15,16,17,18,19)
s <- c(11,21,22,14,15,16,17,18,19,20)

a<-read.csv('zx 2 digits for first 20 month.csv')
# 只在申银万国行业分类下使用
#a$logre_ind <- log(1+a$re_ind)
#a$logre_market <- log(1+a$re_market)

sum <- summary(a)[c(1,3,4,6),s]

b<-c()
for(i in s)
{
  b<-append(b,sd(a[,i]))
}

b<-rep(b,each = 4)
#dt <- data.frame(sum)
#dt$sd = b

aa <- data.frame(sum)
aa$sd = b
aa$Var1 = 'zx2'

dt <- cbind(dt,aa)

write.csv(dt,'test.csv')
