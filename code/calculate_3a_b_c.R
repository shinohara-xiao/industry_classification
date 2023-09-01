#------------------------------上接calculate_beta_2a------------------------------------------#

#------------------------------计算（3a）（3b）（3c）------------------------------------------#

#--- (3a) Calculate the MarketReturn
#---------- x:Re_Market(79*1)
#---------- y:different firms of the market return should be different。--FIRM SPECIFIC--
#             differnet times        。。。        should be different。--TIME SPECIFIC--
#---------- beta: beta2对应的系数

for(i in 1:length(beta)) # 620
{
  n<-nrow(beta[[i]])/60
  if(i==1)
    num<-n
  else
    num<-num+n
}

# 提取beta2
beta2<-c()
for(i in 1:length(MS))
{
  l<-length(beta[[i]][,3])
  if(i==1)
    beta2<-beta[[1]][,3]
  else
    beta2<-append(beta2,beta[[i]][,3])
}

# 提取ReMarket
Re_Market_var<-rep(Re_Market[20:79],times=length(beta2)/60)

# 计算MarketRE
MarketRE<-beta2*Re_Market_var

# 写成数据框，提取股票和时间
time<-rep(true_times,times=length(beta2)/60)
for(i in 1:length(beta))
{
  if(i==1)
    assetID<-beta[[i]]$stockid
  else
    assetID<-append(assetID,beta[[i]]$stockid)
}


#--（3b）--calculate the Industry—level Return
#------------  y
#------------  x Re_Ind 
# 提取beta1
beta1<-c()
for(i in 1:length(MS))
{
  l<-length(beta[[i]][,2])
  if(i==1)
    beta1<-beta[[1]][,2]
  else
    beta1<-append(beta1,beta[[i]][,2])  # length(beta1=39960,stock_number=666)
} # 行业i对应几只股票从2014.1-2018.12之间60个月的数据


#-------------------------------ERROR------------------------------------------#
# 缺了回归的beta值（错误）重新计算
# 回归的残差值
# 提取IndRE

IndRE<-c()
for(i in 1:length(Re_Ind))
{
  I<-rep(Re_Ind[[i]][3][20:79,],times=nrow(beta[[i]])/60)
  if(i==1)
    IndRE<-I
  else
    IndRE<-append(IndRE,I)
}



# 提取industry_id
for(i in 1:length(Re_Ind))
{
  I<-rep(Re_Ind[[i]][1][1,],each=nrow(beta[[i]]))  # 60个不同时间对应的行业收益值，60个行业id
  if(i==1)
    industryname<-I
  else
    industryname<-append(industryname,I)
}


# 提取firm-level return
S<-split(output,output$assetid)
firmRE<-c()
for(i in 1:length(MS))
{
  b<-split(MS[[i]],MS[[i]]$asset_id)
  for(j in 1:length(b))
  {
    firm<-b[[j]]$prof[20:79]
    firmRE<-append(firmRE,firm)
  }
}

IdioRE<-as.numeric(firmRE)-as.numeric(MarketRE)-as.numeric(IndRE)

output<-data.frame(ts=time,industryID=industryname,assetid=assetID, stockreturn=as.numeric(firmRE),
                   beta_1=beta1,beta_2=beta2,marketreturn=MarketRE,indreturn=IndRE,Idiosreturn=IdioRE)


write.csv(output,file='output.csv')

#------summary--------#
summary(output)
for(i in 1:6)
{
  print(sd(output[,i+3]))
}
# 由于剔除数据过多，可能行业中包含一只股票，回归时只有该只股票

cor(output[,c(4,7,8,9)],method='person') # 默认为pearson系数


