#--------------------------- 按月回归，分别处理 ------------------------------#

#  按月进行回归
#  每个月剔除的数据不同
#  首先计算（1）,在此基础上重新计算2a,2b # nope    First we do the data precessing and then the regression
#  步骤：剔除不连续的数据--->data_less---剔除行业发生变更的数据--->剔除最后一个月行业内股票数小于5的observations--->回归
# install.packages('reshape')

# 加载包
library(dplyr)
library(stringr)
library(reshape)
#------------------------------- data processing -------------------------------------#
#data<-read.csv('resample.csv',encoding='UTF-8'). # data = resample
#profit<-read.csv('month_profit_resample.csv',encoding='UTF-8')
#ts<-substring(profit$ts,1,10)
#prof<-profit$cash_profit
#asset_id<-profit$asset_id
#re<-cbind(ts,prof,asset_id)
#re<-data.frame(re)

#sum(is.na(re[,2])) # 统计re中含有NA值的个数
#re<-na.omit(re) # 剔除缺失值


#class<-filter(data,data$classification_id=='P0212')# choose the portfolio 在申银万国2014年修订版下

#sid<-intersect(re$asset_id,class$asset_id)
#class<-class[which(class$asset_id %in% sid),] 
#re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集

#ind_4<-substring(class$industry_id,1,4)
#ind_2<-substring(class$industry_id,1,2)
#class<-cbind(class,ind_4,ind_2)    # 得到四位和二位下的行业分类

#data<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间


# ----------------------- delete those who is ST stocks ----------------- # 
# test whether there are stocks need to be remained
#spt<-read.csv('SPT_Trdchg.csv',header = F,quote = "")
#fix(spt)
#spt<-spt[-1,]
#names(spt)[1]<-'asset_id'
#names(spt)[2]<-'asset_name1'
#ames(spt)[3]<-'asset_name2'
#ames(spt)[5]<-'chdate'
#a<-spt[,c(1,2,3,4,5)]

# deal with the data pattern
#a<-sort(spt$asset_id)
#index<-duplicated(a)
#a_del<-a[!index] # not delete ,only print the stock id 
#a<-a_del[-(908:957)]
#a<-substring(a,2,7)
#a<-as.integer(a)
#a<-na.omit(a)

#data_new<-filter(data,!(data$asset_id %in% a))   # delete those stocks


#eripo<-read.csv('ER_IPO.csv')
#a<-eripo %>% filter(LatestBackdoorDate<'2019-02'&LatestBackdoorDate>'2012')
#data_new<-filter(data_new,!(data_new$asset_id %in% a))
# prepare
# 2012.06-2018.12(20个月的数据，算上最后一个月的预测窗口，应该是到2019.01)      60 months for calculate in total


# --------- 该部分看 summary_dataprocess.R 部分的函数定义，以该文件的函数为准 --------- #

##  TAKE THE FIRST MONTH FOR EXAMPLE 
# delete less than 21 months observations

Delete_missing21<-function(dt,mon)
{
  num=0
  dtsets<-dt[which(dt$ts<end_times[mon] & dt$ts>start_times[mon]),]
  D<-dtsets
  dt_div<-split(dtsets,dtsets$asset_id)
  for(i in 1:length(dt_div))
  {
    l<-nrow(dt_div[[i]])
    if(l<21)
    {
      num=num+1
      D<-filter(D,!(D$asset_id %in% dt_div[[i]]$asset_id))
    }
  }
  cat('observations of the data_full:',nrow(dtsets),'\n')
  cat('firms of data_full',length(dt_div),'\n')
  cat('missing observations:',nrow(dtsets)-nrow(D),'\n')
  cat('missing firms:',num,'\n')
  return(D) 
}
 
data_less<-Delete_missing21(data,1)

# TEST   test whether every stock in the FIRST window all has the 21 months observations

# test whether every stock in the FIRST window all has the 21 months observations 
tests<-split(data_less,data_less$asset_id)  

for(i in 1:length(tests))
  {
   lid<-nrow(tests[[i]])
   if(lid<21)
    print(i) # print those who still lack 21 months observations 'the order' not the stock id 
  }
  
# figure out those who has changed industry
c<-c()
 for(i in 1:length(tests))  
   {
    t<-duplicated(tests[[i]]$industry_id)
    for(j in 1:20)
      {
      if(t[j+1]==FALSE)
        c<-append(c,tests[[i]]$asset_id[1])
      }
  }
c<-sort(c)
index<-duplicated(c)
c_del<-c[!index] # not delete ,only print the stock id (stored in the c_del)

cat('those who has changed their industry are: ',c_del) # 
cat('the number of these stocks is : ',length(c_del))   # 273
cat('observations of data_less: ',nrow(data_less))  # 42693(right)

# delete those who has changed their industry
data_less_change<-data_less %>% filter(!(asset_id %in% c_del))
cat('observations of those who has changed the industry:',nrow(data_less)-nrow(data_less_change))


########################### calculate the industry those who has less than 5 firms 
# indusrty_id/class_num=5; ind_4/class_num=7; ind_2/class_num=8

calculate_lessthan5<-function(dt,num,mon)
{
  numbers=0
  less<-c()
  I<-c()
  a<-dt[which(dt$ts==end_times_true[mon]),]
  s<-split(a,a[,num])
  
  for(i in 1:length(s))  # 计算该时间full_times[i]下有多少个行业  length(S)=578
  {
    if(nrow(s[[i]])<5)
    {
      I<-append(I,i)
      numbers=numbers+1
      less<-append(less,s[[i]][,num][1])
    }
  }
  cat('number of those who has less than 5 firms :',numbers,'\n')  # 78
  cat('those industry who lack 5 firms are:' ,'\n',less,'\n')
  return(less)
}
lessthan5id<-calculate_lessthan5(data_less_change,1,5)

# delete those who has less than 5 firms
data_use<-data_less_change %>% filter(!(industry_id %in% lessthan5id))
nrow(data_less)-nrow(data_use)
nrow(data_less_change)-nrow(data_use)



#---- Calculate industry-level and market-level ------#

# calculate the industry-level return.
# 使用函数计算
# indusrty_id/ind_num=5; ind_4/ind_num=7; ind_2/ind_num=8

# data_full<-data %>% filter(ts>'2012-06' & ts<'2019-02') # includiing data from 2012-06-01 to 2019-01-01

Calculate_industryReturn<-function(obs,num,mon)
  # 每一只股票，每一个月的industry—level 都不一样
{
  MI<-split(obs,obs[,num])
  for(i in 1:length(MI)) # 第i个行业
  {
    print(i)
    a<-MI[[i]]$asset_id
    index<-duplicated(a)
    a<-a[!index]
    for(j in 1:21)
    {
      b<-MI[[i]] %>% filter(ts == full_times[mon:(mon+20)][j]) # 第i个行业 第j月的数据集
      for(k in 1:length(a))
        b$re_ind[k]<-mean(as.numeric(b[-k,]$prof))
      if(j==1)
        B<-b
      else
        B<-rbind(B,b)
    }
    if(i==1)
      return<-B
    else
      return<-rbind(return,B)
  }
  return(return)
}

Re_Ind<-Calculate_industryReturn(data_use,5,1) # 数据集

# Calculate the market-level return.exclude the firm itself
# The function
Calculate_marketReturn<-function(obs,num,mon) # 第mon个窗口的计算
{
  MI<-split(obs,obs$ts)
  for(i in 1:21)
  {
    print(i)
    for(j in 1:nrow(MI[[i]]))
      MI[[i]]$re_market[j]=mean(as.numeric(MI[[i]][-j,]$prof)) # 第i个月 第j个股票的市场收益
    if(i==1)
      b<-MI[[i]]
    else
      b<-rbind(b,MI[[i]])
  }
  return(b)
}


# 生成的最终数据。注意名称不要发生改变 ，不对改data.frame做任何处理

#re_indmarket<-Calculate_marketReturn(Re_Ind,5,1)

 re_indmarket<- Data_Process(data,5,1) # 第mon个月，按照num进行划分得到的结果


write.csv(re_indmarket,'data_include_indmarket.csv')

# ---------------------------------不取log----------------------------------------- #

# 按月回归 按股票回归 取log回归 #

# 选取前20个月的数据进行回归

# regeression(2a)

# ---FUNCTION--- #

#a<-Regression(re_indmarket,1)

#----------------------------------暂不运行，可以使用函数运行------------------------------#
#residuals<-Regression2a(re_indmarket,2) # 存储数据
#beta<-Regression2b(re_indmarket,2)

# cbind the data.frame

# condition = 1
#re_indmarket_20<-re_indmarket %>% filter(!(ts=='2014-02-01'))
#re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
#re_indmarket_20<-cbind(re_indmarket_20,residuals,beta) # condition = 1 (both Regression 2a and Regression 2b)

#MktRe<-(re_indmarket_20$beta2)*(re_indmarket_20$re_market)
#IndRe<-(re_indmarket_20$beta1)*(re_indmarket_20$re_ind)
#IdiosRe<-as.numeric(re_indmarket_20$prof)-MktRe-IndRe
#re_indmarket_20<-cbind(re_indmarket_20,MktReturn=MktRe,IndReturn=IndRe,IdiosReturn=IdiosRe)
#write.csv(re_indmarket_20,'first window for the first 20 months.csv')


# condition = 2
#re_indmarket_21<-re_indmarket %>% filter(!(ts=='2012-06-01'))
#re_indmarket_21<-re_indmarket_21[order(re_indmarket_21$asset_id),]
#re_indmarket_21<-cbind(re_indmarket_21,residuals,beta) # condition = 2 (both Regression 2a and Regression 2b)

#MktRe<-(re_indmarket_21$beta2)*(re_indmarket_21$re_market)
#IndRe<-(re_indmarket_21$beta1)*(re_indmarket_21$re_ind)
#IdiosRe<-as.numeric(re_indmarket_21$prof)-MktRe-IndRe
#re_indmarket_21<-cbind(re_indmarket_21,MktReturn=MktRe,IndReturn=IndRe,IdiosReturn=IdiosRe)
#write.csv(re_indmarket_21,'first window for the last 20 months.csv')

# description
#re_indmarket_20$prof<-as.numeric(re_indmarket_20$prof)
#class(re_indmarket_20$prof)
#cor(re_indmarket_20[,c(9,15,16,17)])
#----------------------------------------------------------------------------------------#



# ----------取log(自然对数)
#ldata<-data
#ldata$prof<-log(1+as.numeric(ldata$prof))
#re<-Data_Process(data,5,1)


#re_20<-Regression(re,1)
# re_20$IdiosReturn<-log(1+re_20$IdiosReturn)
#re_20$MktReturn<-log
#re_21<-Regression(re,2)

#write.csv(re_20,'fist window outcome of first 20 month--based on log.csv')

# description
#summary(re_20)
#for(i in c(9,12,13,14,15,16,17))  # prof,residuals,beta1,beta2,Mkt,Ind,Idios
#  print(sd(re_20[,i]))

#cor(re_20[,c(9,15,16,17)])

#-----------先计算，再取log（自然对数）
#re<-Data_Process(data,5,1)

#re_20<-Regression(re,1)
#re_21<-Regression(re,2)
#write.csv(re_20,'return.csv')



##############################################################################################


# ------- 申银万国行业分类下 ----------- #

#------------------------------- data processing -------------------------------------#
data<-read.csv('resample.csv',encoding='UTF-8') # data = resample
profit<-read.csv('month_profit_resample.csv',encoding='UTF-8')
ts<-substring(profit$ts,1,10)
prof<-profit$cash_profit
asset_id<-profit$asset_id
re<-cbind(ts,prof,asset_id)
re<-data.frame(re)

sum(is.na(re[,2])) # 统计re中含有NA值的个数
re<-na.omit(re) # 剔除缺失值


class<-filter(data,data$classification_id=='P0211')# choose the portfolio 在申银万国2014年修订版下

sid<-intersect(re$asset_id,class$asset_id)
class<-class[which(class$asset_id %in% sid),] 
re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集

ind_4<-substring(class$industry_id,1,4)
ind_2<-substring(class$industry_id,1,2)
class<-cbind(class,ind_4,ind_2)    # 得到四位和二位下的行业分类

data_1<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间

# 得到2012- 2019年间的所有数据
data_1 <- data_1 %>% filter(ts >= '2012-06' & ts < '2019-02') 

# 计算所需要的firms和observations
observations <- nrow(data_1)
cat('初始计算observations为：',observations,'\n')

a <- split(data_1,data_1$asset_id)
firms <- length(a)
cat('初始计算firms为：',firms,'\n')


# ---- 剔除2012-2019年间的ST股
spt<-read.csv('SPT_Trdchg.csv',header = F,quote = "")
a<-sort(spt[,1])
index<-duplicated(a)
a_del<-a[!index] # not delete ,only print the stock id 
a<-a_del[-(908:957)]
a<-substring(a,2,7)
a<-as.integer(a)
a<-na.omit(a)  # length(a) = 902
data_new<-filter(data_1,!(data_1$asset_id %in% a))  # 删除 182539 个observations

# 计算剔除2012-2019年间的ST股后的observations和firms
a <- observations - nrow(data_new)
observations <- nrow(data_new)
cat('剔除st股后的observations差额：',a)

a <- split(data_new,data_new$asset_id)
b <- firms - length(a)
firms <- length(a)
cat('剔除st股后的firms差额：',b,'\n')



# ----- 剔除在2012-2019年间借壳上市的股票
eripo<-read.csv('ER_IPO.csv')
a<-eripo %>% filter(LatestBackdoorDate<'2019-02'&LatestBackdoorDate>'2012')   # length(a) = 40
data_new<-filter(data_new,!(data_new$asset_id %in% a))  # 392496-392496

# 剔除在2012-2019年间借壳上市的股票后的observations和firms
a <- observations - nrow(data_new)
observations <- nrow(data_new)
cat('剔除在2012-2019年间借壳上市的股票后的observations差额：',a,'\n')

a <- split(data_new,data_new$asset_id)
b <- firms - length(a)
firms <- length(a)
cat('剔除在2012-2019年间借壳上市的股票后的firms差额',b,'\n')



# ----- 剔除收益过大或过小的股票
data_new$logprof<-log(1+as.numeric(data_new$prof))
data_new<-data_new %>% filter(logprof>=-1 & logprof<=1)   

# 剔除在2012-2019年间收益过大或过小的observations和firms
a <- observations - nrow(data_new)
observations <- nrow(data_new)
cat('剔除在2012-2019年间收益过大或过小的observations差额：',a,'\n')

a <- split(data_new,data_new$asset_id)
b <- firms - length(a)
firms <- length(a)
cat('剔除在2012-2019年间收益过大或过小的firms差额：',b,'\n')

cat('最终参与计算的observations：',observations)
cat('最终参与计算的firms：',firms)


# ---------------------------------------- 循环 ------------------------------------- #
# 存储数据
return_20<-list()
return_21<-list()
#ldata<-data
#ldata$prof<-log(1+as.numeric(ldata$prof))
Stock0 <- c()
Stock1 <-c()
Stock2 <-c()
Stockd <-c()
# industry_id------num=5
# ind_4------------num=7
# ind_2------------num=8

for(k in 1:60)
{
  print(k)
  # mon = 1 对应2012-06 至 2014-02 之间的数据
  resultProcess <- Data_Process(data_new,num = 8,mon = k)
  re<-resultProcess$re_indmarket
  
  stock0 = resultProcess$stock0
  stock1 = resultProcess$stock1
  stock2 = resultProcess$stock2
  stockd = resultProcess$stockd
  
  Stock0 <- union(Stock0,stock0)
  Stock1 <- union(Stock1,stock1)
  Stock2 <- union(Stock2,stock2)
  Stockd <- union(Stockd,stockd)
  
  cat('firms are:',nrow(re)/21,'\n')
  cat('observations:',nrow(re),'\n')
  
  re_20<-Regression(re,1,k)   # condition = 1 - the first 20 months  -- i th month
  #re_21<-Regression(re,2,k)
  return_20[[k]]<-re_20
  #return_21[[k]]<-re_21
}

cat('missing21_stocks length is:',length(Stock0) - length(Stock1),'\n')
cat('indchange_stocks length is:',length(Stock1) - length(Stock2),'\n')
cat('less5_stocks length is:',length(Stock2) - length(Stockd),'\n')

# 输出数据
#write.csv(return_20[[1]],'1st window for the first 20 months.csv')
#write.csv(return_20[[2]],'2nd window for the first 20 months.csv')
#write.csv(return_20[[3]],'3rd window for the first 20 months.csv')

#for(i in 4:60)
#{
#  write.csv(return_20[[i]],sprintf('%dth window for the first 20 months.csv',i))
#}


# 合并数据
for(i in 1:60)
{
  if(i == 1)
    a<-return_20[[i]]
  else
    a<-rbind(a,return_20[[i]])          # 按顺序合并，60个窗口
}

write.csv(a,'4 digits for last 20 month.csv')

a$logre_ind <- log(1+a$re_ind)
a$logre_market <- log(1+a$re_market)

summary(a)

cor(a[,c(9,19,20,12,13,14,15,16,17,18)],method = 'pearson')
cor(a[,c(10,20,21,17,18,19)],method = 'spearman')



# 绘图看分布
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
#hist(a$beta2,col='blue')
#rug(jitter(a$beta2))
#lines(density(a$beta2))


# 只取最后1个月的数据
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

cor(aa[,c(10,20,21,17,18,19)],method = 'pearson')
cor(aa[,c(10,20,21,17,18,19)],method = 'spearman')
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

# ------------------------- 取log产生的缺失值 ------------------------ #
#for(i in 1:60)
#  if(sum(is.na(return_20[[i]])) != 0)
#    print(i)
#  print(sum(is.na(return_20[[i]][18]))) # 5，6，7，25～44 有缺失值

#re_indmarket_20<-re %>% filter(!(ts == end_times_true[5]))
#re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
#y<-log(1+re_indmarket_20$re_ind[1:20])
#x<-log(1+re_indmarket_20$re_market[1:20])
#res2a<-lm(y~x)$residuals
#y<-log(1+as.numeric(re_indmarket_20$prof[1:20]))
#x1<-res2a[1:20]
#x2<-log(1+re_indmarket_20$re_market[1:20])
#beta0<-rep(lm(y~x1+x2)$coefficients[1])
#beta1<-rep(lm(y~x1+x2)$coefficients[2])
#beta2<-rep(lm(y~x1+x2)$coefficients[3])


# ---------------------------------------- predict 5a,5b,5c,5d,5e ----------------------------------------- #
## 申银万国行业分类
dig6.first.sy <- read.csv('6 digits for first 20 month.csv') # 完整数据
dig6.last.sy <- read.csv('6 digits for last 20 month.csv') # 完整数据

aa$logre_ind <- log(1+aa$re_ind)
aa$logre_market <- log(1+aa$re_market)

dig4.first.sy <- read.csv('4 digits for first 20 month.csv') # 完整数据
dig4.last.sy <- read.csv('4 digits for last 20 month.csv') # 完整数据

dig2.first.sy <- read.csv('2 digits for first 20 month.csv')
dig2.last.sy <- read.csv('2 digits for last 20 month.csv')


# Function -- 从完整数据中筛选每一窗口最后一个月的数据，写成数据框的形式
select_everywind <- function(dt)
{
  l <- nrow(dt)/20
  #print(l)
  a <- data.frame()
  for(i in 1:l)
  {
    #print(i)
    a <- rbind(a,dt[(20*i),])
  }
  return(a)
}


dig6.first.sy.reg <- select_everywind(dig6.first.sy)  # 2014.01 --- 2018.12
#dig6.last.sy.reg <- select_everywind(dig6.last.sy)    # 2014.02 --- 2019.01

#dig4.first.sy.reg <- select_everywind(dig4.first.sy)
#dig4.last.sy.reg <- select_everywind(dig4.last.sy)

#dig2.first.sy.reg <- select_everywind(dig2.first.sy)
#dig2.last.sy.reg <- select_everywind(dig2.last.sy)


## 
# define function
Regression5 <- function(firstdt,lastdt)
  # usage: fisrtdt -- digx.first.sy.reg ; lastdt -- digx.last.sy.reg
{
  # 数据处理
  # step1 对数据进行处理（dig6.first.sy.reg 和 dig6.last.sy.reg 不同（由于beta删除后数据出现误差））
  a <- split(firstdt,firstdt$ts)
  b <- split(lastdt,lastdt$ts)
  
  #a <- split(dig6.first.sy.reg,dig6.first.sy.reg$ts)
  #b <- split(dig6.last.sy.reg,dig6.last.sy.reg$ts)
  #for(i in 1:60)
  #{
  #  sid <- intersect(a[[i]]$asset_id,b[[i]]$asset_id)
  #  a[[i]] = filter(a[[i]],a[[i]]$asset_id %in% sid)
  #  b[[i]] = filter(b[[i]],b[[i]]$asset_id %in% sid)
  #}
  
  #for(j in 1:60)
  #{
  #  if(j==1){
  #    aa <- a[[j]]
  #    bb <- b[[j]]
  #  }
  #  else{
  #    aa <- rbind(aa,a[[j]])
  #    bb <- rbind(bb,b[[j]])
  #  }
  #}

  # regression 5a
  beta0.5a <- c() ; beta1.5a <- c() ; t.5a <- c() ; r.5a <- c()
  beta0.5b <- c() ; beta1.5b <- c() ; beta2.5b <- c() ; beta3.5b <- c() ; t.5b <- c() ; r.5b <- c()
  beta0.5c <- c() ; beta1.5c <- c() ; t.5c <- c() ; r.5c <- c()
  beta0.5d <- c() ; beta1.5d <- c() ; t.5d <- c() ; r.5d <- c()
  beta0.5e <- c() ; beta1.5e <- c() ; t.5e <- c() ; r.5e <- c()
  
  #y1<-c()
  #x1<-c()
  #for(j in 1:60)
  #{
  #  y1<-append(y1,b[[j]]$MktReturn)
  #  x1<-append(x1,a[[j]]$MktReturn)
  #}
  #aa<-data.frame(x=x1,y=y1)
  
  for(j in 1:60)
  {
    # 提取变量
   y <- b[[j]]$logprof ; y1 <- b[[j]]$MktReturn ; y2 <- b[[j]]$IndReturn ; y3 <- b[[j]]$IdiosReturn
   x <- a[[j]]$logprof ; x1 <- a[[j]]$MktReturn ; x2 <- a[[j]]$IndReturn ; x3 <- a[[j]]$IdiosReturn
   
   # 回归，提取系数
   beta0.5a <- append(beta0.5a,coefficients(lm(y~x))[1])
   beta1.5a <- append(beta1.5a,coefficients(lm(y~x))[2])
   #t.5a <- append(t.5a,summary(lm(y~x))$coefficients[,'t value'])
   r.5a <-append(r.5a,summary(lm(y~x))$r.square)
   
   beta0.5b <-append(beta0.5b,coefficients(lm(y~x1+x2+x3))[1])
   beta1.5b <-append(beta1.5b,coefficients(lm(y~x1+x2+x3))[2])
   beta2.5b <-append(beta2.5b,coefficients(lm(y~x1+x2+x3))[3])
   beta3.5b <-append(beta3.5b,coefficients(lm(y~x1+x2+x3))[4])
   #t.5b <- append(t.5b,summary(lm(y~x1+x2+x3))$coefficients[,'t value'])
   r.5b <-append(r.5b,summary(lm(y~x1+x2+x3))$r.square)
   
   beta0.5c <- append(beta0.5c,coefficients(lm(y1~x1))[1])
   beta1.5c <- append(beta1.5c,coefficients(lm(y1~x1))[2])
   #t.5c <- append(t.5c,summary(lm(y1~x1))$coeffecients[,'t value'])
   r.5c <-append(r.5c,summary(lm(y1~x1))$r.square)
   
   beta0.5d <- append(beta0.5d,coefficients(lm(y2~x2))[1])
   beta1.5d <- append(beta1.5d,coefficients(lm(y2~x2))[2])
   #t.5d <- append(t.5d,summary(lm(y2~x2))$coeffecients[,'t value'])
   r.5d <-append(r.5d,summary(lm(y2~x2))$r.square)
   
   beta0.5e <- append(beta0.5e,coefficients(lm(y3~x3))[1])
   beta1.5e <- append(beta1.5e,coefficients(lm(y3~x3))[2])
   #t.5e <- append(t.5e,summary(lm(y3~x3))$coeffecients[,'t value'])
   r.5e <-append(r.5e,summary(lm(y3~x3))$r.square)
  }
  
  #cat('beta0.5a:',length(beta0.5a),'\n','beta1.5a:',length(beta1.5a),'\n','r.5a:',length(r.5a),'\n',
  #    'beta0.5b:',length(beta0.5b),'\n','beta1.5b:',length(beta1.5b),'\n','beta2.5b:',length(beta2.5b),'\n','beta3.5b:',length(beta3.5b),'r.5b:',length(r.5b),'\n',
  #    'beta0.5c:',length(beta0.5c),'\n','beta1.5c:',length(beta1.5c),'\n','r.5c:',length(r.5c),'\n',
  #   'beta0.5d:',length(beta0.5d),'\n','beta1.5d:',length(beta1.5d),'\n','r.5d:',length(r.5d),'\n',
  #   'beta0.5e:',length(beta0.5e),'\n','beta1.5e:',length(beta1.5e),'\n','r.5e:',length(r.5e),'\n')
  
  df <- data.frame(beta0.5a,beta1.5a,r.5a,
                   beta0.5b,beta1.5b,beta2.5b,beta3.5b,r.5b,
                   beta0.5c,beta1.5c,r.5c,
                   beta0.5d,beta1.5d,r.5d,
                   beta0.5e,beta1.5e,r.5e)
  return(df)
}

## Application
s <- c(1,2,4,5,6,7,9,10,12,13,15,16)

reg <- Regression5(firstdt = select_everywind(dt = read.csv('6 digits for first 20 month.csv')), 
                   lastdt = select_everywind(dt = read.csv('6 digits for last 20 month.csv')))

sum <- summary(reg)[c(1,3,4,6),]

b <- c()
for(i in 1:17)
{
  b<-append(b,sd(reg[,i]))
}
b <- rep(b,each = 4)
#dt <- as.data.frame(sum)
#dt$sd <- b
#dt$Var1 = 'sy6'

sum <- as.data.frame(sum)
sum$sd <- b
sum$Var1 = 'zx2'
dt <- cbind(dt,sum)

#write.csv(aa,'/lanec2_home/zhangx/R/regression/reg5c.csv')


# ------------------------ 采用滚动窗口计算 ------------------------ #

#（每一个大窗口20个月的数据，其中以10个月为起端开始计算每个小窗口的参数，取平均值）
# 2a,2b & 3a,3b,3c 要重新算
# 直接调用函数??(暂时不)

## --------- Instructions
# mon = 1 对应2012-06 至 2014-02 之间的数据
# 这里对应的是一个大窗口

# 中信证券行业分类从 2015-08-01 开始，num选择 5--六位；7--四位 ； 8--两位
# 中证行业分类num 有三种选择：num = 5 --8位 ； 7--六位 ； 8--四位 ； 9--两位
# 证监会只有两种，三位分类（5）和 一位分类（7）
# 申银万国行业分类2014版，num选择 5--六位；7--四位 ； 8--两位

result = list()

#for(k in 1:60)
{
  cat('k=',k,'\n')
  
  # step 1 : 得到第k个大窗口对应的数据
  resultProcess <- Data_Process(data_new,num = 5,mon = k)
  re<-resultProcess$re_indmarket # 得到一个大窗口(总共21个月）对应的处理后的data.frame
  
  # step 2: 对一个大窗口进行分割，得到小窗口，并且分别回归
  # 最后一个月作为out-of-sample
  # Res2a <- c() ; Beta1 <- c() ; Beta2 <- c()
  # MktRe <- c() ; IndRe <- c() ; IdiosRe <- c()
  
  for(i in 1:10)
  {
    #print(i)
    # 共有10个小窗口，窗口长度逐渐增加
    # k 表示第k个大窗口，i表示第i个小窗口
    re23 <- re %>% filter(ts >=full_times[k] & ts <= full_times[k+8+i])
    re23 <- re23[order(re23$asset_id),]
    #df <- split(re23,re23$asset_id)
    # regression  ,firm-specific
    l = nrow(re23)/(9+i)
    
    for(j in 1:l)
      # 对股票进行循环
    {
      # 第j个股票
      #a = re23[(10*j-9):(10*j),]
      a = re23[((9+i)*(j-1)+1):((9+i)*(j-1)+9+i),]
      # regression 2a
      y = log(1+a$re_ind)
      x = log(1+a$re_market)
      res2a = lm(y~x)$residuals
      #Res2a<-append(Res2a,res2a) # 得到第i个小窗口对应的所有股票的res2a
      
      # regression 2b
      y1 = a$logprof
      x1 = res2a
      x2 = log(1+a$re_market)
      beta1<-rep(lm(y~x1+x2)$coefficients[2],each = (9+i))
      beta2<-rep(lm(y~x1+x2)$coefficients[3],each = (9+i))
      
      #Beta1 <- append(Beta1,beta1)
      #Beta2 <- append(Beta2,beta2)
      
      # regression 3abc
      mktre = beta2 * log(1+a$re_market)
      indre = beta1 * res2a
      idiosre = a$logprof - mktre - indre
      
      #MktRe <- append(MktRe,mktre)
      #IndRe <- append(IndRe,indre)
      #IdiosRe <- append(IdiosRe,idiosre)
      output = data.frame(Res2a = res2a,Beta1 = beta1,Beta2 = beta2,
                          MktRe = mktre,IndRe = indre,IdiosRe = idiosre)
      output = cbind(a,output)
      
      if(j == 1)
         out = output
      else
         out = rbind(out,output) # 得到第i个小窗口的结果
    }
    #result = data.frame(Res2a,Beta1,Beta2,MktRe,IndRe,IdiosRe)
    # result = cbind(a,result)
    
    # 得到第i（q）个小窗口对应的回归结果
    result[[i]] <- out
    # result[[i]] <- result[[i]][order(result[[i]]$ts),]
  }
  
  # regression 5abcde
  for(q in 1:9)
  {
    # Regression 5a
    y = result[[q+1]] %>% filter(ts > full_times[k])
    y = y$logprof
    x = result[[q]]$logprof
    alpha0_a = lm(y~x)$coefficients[1]
    alpha1_a = lm(y~x)$coefficients[2]
    
    # Regression 5b
    y = result[[q+1]] %>% filter(ts > full_times[k])
    y = y$logprof
    x1 = result[[q]]$MktRe
    x2 = result[[q]]$IndRe
    x3 = result[[q]]$IdiosRe
    alpha0_b = lm(y~x1+x2+x3)$coefficients[1]
    alpha1_b = lm(y~x1+x2+x3)$coefficients[2]
    alpha2_b = lm(y~x1+x2+x3)$coefficients[3]
    alpha3_b = lm(y~x1+x2+x3)$coefficients[4]
    
    # Regresion 5c
    y = result[[q+1]] %>% filter(ts > full_times[k])
    y = y$MktRe
    x = result[[q]]$MktRe
    alpha0_c = lm(y~x)$coefficients[1]
    alpha1_c = lm(y~x)$coefficients[2]
    
    # Regeression 5d
    y = result[[q+1]] %>% filter(ts > full_times[k])
    y = y$IndRe
    x = result[[q]]$IndRe
    alpha0_d = lm(y~x)$coefficients[1]
    alpha1_d = lm(y~x)$coefficients[2]
    
    # Regeression 5e
    y = result[[q+1]] %>% filter(ts > full_times[k])
    y = y$IdiosRe
    x = result[[q]]$IdiosRe
    alpha0_e = lm(y~x)$coefficients[1]
    alpha1_e = lm(y~x)$coefficients[2]
    
    out = data.frame(q = q,
                     alpha0_a,alpha1_a,
                     alpha0_b,alpha1_b,alpha2_b,alpha3_b,
                     alpha0_c,alpha1_c,
                     alpha0_d,alpha1_d,
                     alpha0_e,alpha1_e)
    if(q == 1)
      out5 = out
    else
      out5 = rbind(out5,out)
    
    # 计算均值
    out5 = colMeans(out5)
  }
  
  if(k == 1)
    reg5 = cbind(k,out5)
  else 
    reg5 = rbind(reg5,cbind(k,out5))
}




###############################################################
# ---------- 调用函数 --------------- #
## usage
# classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
#                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始，mon从39开始
#                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
#                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位

## 申银万国行业分类
data_new <- Data_P(classification = 'P0212')

#data_new.sy = data_new
data_new_zj = data_new
data_new_zz = data_new
data_new_zx = data_new
# test --- firms.begin.all <- unique(data_new$asset_id)
#zzreg2 = Data_Process(dt = data_new,num = 9,mon = 1)
#data_final = syreg5$re_indmarket

# 六位
for(k in 39:60)
{
  print(k)
  syreg6 <- Regression5_mean(dataset = data_new,num = 7,mon = k )
  if(k == 39)
  {
    syreg6_alpha = syreg6$reg
    syreg6_data = syreg6$data
  }
  else
  {
    syreg6_alpha <- rbind(syreg6_alpha,syreg6$reg)
    syreg6_data <- rbind(syreg6_data,syreg6$data)
  }
}

#zzreg2_alpha = zzreg4_alpha
#zzreg4_alpha = zzreg6_alpha

write.csv(syreg6_data,'zx2.data-change.csv')
#zjreg3_alpha = syreg6_alpha

#zzreg2_alpha = zzreg4_alpha
#zzreg6_alpha = zzreg4_alpha
#zzreg8_alpha = zzreg6_alpha

#----- summary for table2 panelA and panelB ---- #
a <- syreg6_data

summary(a$logprof) ; sd(a$logprof)
summary(a$MktRe) ; sd(a$MktRe)
summary(a$IndRe) ; sd(a$IndRe)
summary(a$IdiosRe) ; sd(a$IdiosRe)
summary(a$Beta2) ; sd(a$Beta2) # market beta
summary(a$Beta1) ; sd(a$Beta1) # industry beta

## zz
#cor(a[,c(11,17,18,19)],method = 'pearson')
#cor(a[,c(11,17,18,19)],method = 'spearman')

#zj
#cor(a[,c(9,15,16,17)],method = 'pearson')
#cor(a[,c(9,15,16,17)],method = 'spearman')

cor(a[,c(10,16,17,18)],method = 'pearson')
cor(a[,c(10,16,17,18)],method = 'spearman')


absind <- abs(a$IndRe) ; absmarket = abs(a$MktRe) ; absidios = abs(a$IdiosRe)
absall = absind + absmarket + absidios
indnew = absind/absall ; idiosnews = absidios/absall ; marketnews = absmarket/absall
summary(marketnews) ; sd(marketnews)
summary(indnew) ; sd(indnew)
summary(idiosnews) ; sd(idiosnews)

#write.csv(syreg5,'syreg6.csv')
# 四位
#syreg7 <- Regression5_mean(dataset = data_new,num = 7)
#syreg4_alpha <- syreg7$reg
#syreg4_data <- syreg7$data
#write.csv(syreg4_data,'sy4.data')
#write.csv(syreg7,'syreg4.csv')
# 两位
#syreg8 <- Regression5_mean(dataset = data_new,num = 8)
#syreg2_alpha <- syreg8$reg
#syreg2_data <- syreg8$data
#write.csv(syreg2_data,'sy2.data')
#write.csv(syreg8,'syreg2.csv')

## 重复使用
# calculate the mean
#class(syreg6_alpha)
sya0 <- syreg6_alpha[,2][seq(2,2126,36)] 
sya1 <- syreg6_alpha[,2][seq(3,2127,36)]
syb0 <- syreg6_alpha[,2][seq(4,2128,36)]  ; syb1 <- syreg6_alpha[,2][seq(5,2129,36)]
syb2 <- syreg6_alpha[,2][seq(6,2130,36)]  ; syb3 <- syreg6_alpha[,2][seq(7,2131,36)]
syc0 <- syreg6_alpha[,2][seq(8,2132,36)]  ; syc1 <- syreg6_alpha[,2][seq(9,2133,36)]
syd0 <- syreg6_alpha[,2][seq(10,2134,36)] ; syd1 <- syreg6_alpha[,2][seq(11,2135,36)]
sye0 <- syreg6_alpha[,2][seq(12,2136,36)] ; sye1 <- syreg6_alpha[,2][seq(13,2137,36)]
r.5a <- syreg6_alpha[,2][seq(14,2138,36)] ; r.5b <- syreg6_alpha[,2][seq(15,2139,36)] 
r.5c <- syreg6_alpha[,2][seq(16,2140,36)] ; r.5d <- syreg6_alpha[,2][seq(17,2141,36)] 
r.5e <- syreg6_alpha[,2][seq(18,2142,36)]

## zx
#sya0 <- syreg6_alpha[,2][seq(20,776,36)] 
#sya1 <- syreg6_alpha[,2][seq(21,777,36)]
#syb0 <- syreg6_alpha[,2][seq(22,778,36)]  ; syb1 <- syreg6_alpha[,2][seq(23,779,36)]
#yb2 <- syreg6_alpha[,2][seq(24,780,36)]  ; syb3 <- syreg6_alpha[,2][seq(25,781,36)]
#syc0 <- syreg6_alpha[,2][seq(26,782,36)]  ; syc1 <- syreg6_alpha[,2][seq(27,783,36)]
#syd0 <- syreg6_alpha[,2][seq(28,784,36)] ; syd1 <- syreg6_alpha[,2][seq(29,785,36)]
#sye0 <- syreg6_alpha[,2][seq(30,786,36)] ; sye1 <- syreg6_alpha[,2][seq(31,787,36)]
#r.5a <- syreg6_alpha[,2][seq(32,788,36)] ; r.5b <- syreg6_alpha[,2][seq(33,789,36)] 
#r.5c <- syreg6_alpha[,2][seq(34,790,36)] ; r.5d <- syreg6_alpha[,2][seq(35,791,36)] 
#r.5e <- syreg6_alpha[,2][seq(36,792,36)]

summary(sya0) ; sd(sya0)   
summary(sya1); sd(sya1)  
summary(r.5a); sd(r.5a)   

summary(syb0) ; sd(syb0)   
summary(syb1) ; sd(syb1) 
summary(syb2) ; sd(syb2)   
summary(syb3) ; sd(syb3) 
summary(r.5b) ; sd(r.5b) 

summary(syc0); sd(syc0)   
summary(syc1) ; sd(syc1) 
summary(r.5c); sd(r.5c)   

summary(syd0) ; sd(syd0)   
summary(syd1); sd(syd1) 
summary(r.5d); sd(r.5d)

summary(sye0) ; sd(sye0)   
summary(sye1); sd(sye1) 
summary(r.5e); sd(r.5e)


## 中信行业分类  从 2015-08-01 开始
data_new_zx <- Data_P(classification = 'P0212')
# 六位
zxreg6 <- Regression5_mean(dataset = data_new_zx,num = 5)
# 四位
zxreg4 <- Regression5_mean(dataset = data_new_zx,num = 7)
# 两位
zxreg2 <- Regression5_mean(dataset = data_new_zx,num = 8)

## 中证行业分类
data_new_zz <- Data_P(classification = 'P0206')
# 八位
#zzreg8 <- Regression5_mean(dataset = data_new_zz,num = 5)
#write.csv(zzreg8,'zzreg8.csv')
# 六位
#zzreg6 <- Regression5_mean(dataset = data_new_zz,num = 7)
#write.csv(zzreg6,'zzreg6.csv')
# 四位
#zzreg4 <- Regression5_mean(dataset = data_new_zz,num = 8)
#write.csv(zzreg4,'zzreg4.csv')
# 两位
zzreg2 <- Regression5_mean(dataset = data_new_zz,num = 9)
write.csv(zzreg2,'zzreg2.csv')
## 证监会分类
data_new_zj <- Data_P(classification = 'P0207')
# 三位
#zjreg3 <- Regression5_mean(dataset = data_new_zj,num = 5)
#write.csv(zjreg3,'zjreg3.csv')
# 一位
#zjreg1 <- Regression5_mean(dataset = data_new_zj,num = 7)
#write.csv(zjreg1,'zjreg1.csv')



