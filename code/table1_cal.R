# 计算table1
# 分别运算60个，两两取交集，得到的最终结果和前一个的差值

# ---------- 初始数据处理 --------- #

# classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
#                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始
#                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
#                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位


# 开始运行
  #data<-read.csv('resample.csv',encoding='UTF-8') # data = resample
  #profit<-read.csv('month_profit_resample.csv',encoding='UTF-8')
  #ts<-substring(profit$ts,1,10)
  #prof<-profit$cash_profit
  #asset_id<-profit$asset_id
  #re<-cbind(ts,prof,asset_id)
  #re<-data.frame(re)
  
  #sum(is.na(re[,2])) # 统计re中含有NA值的个数
  #re<-na.omit(re) # 剔除缺失值
  
  # -------------------------------- 从这里开始

    class <- data %>% filter(classification_id == 'P0207')# choose the portfolio 
    
    sid<-intersect(re$asset_id,class$asset_id)
    class<-class[which(class$asset_id %in% sid),] 
    re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集
    #sy
    #ind_4<-substring(class$industry_id,1,4)
    #nd_2<-substring(class$industry_id,1,2)
    
    #zz
    #ind_6<-substring(class$industry_id,1,6) 
    #ind_4<-substring(class$industry_id,1,4)
    #ind_2<-substring(class$industry_id,1,2)
    
    ## zx
    #ind_4<-substring(class$industry_id,3,6)
    #ind_2<-substring(class$industry_id,3,4)
    
    ## zj
    ind_1 <- substring(class$industry_id,1,1)
    
    class<-cbind(class,
                 #ind_6,           # for zz
                 #ind_4,ind_2
                 ind_1
    )    # 得到 other digits 下的行业分类
    
    data_1<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间
    
    # 得到2012- 2019年间的所有数据
    data_1 <- data_1 %>% filter(ts >= '2012-06' & ts < '2019-02') 
    
    # 计算所需要的firms和observations
    observations <- nrow(data_1)
    cat('初始计算observations为：',observations,'\n')
    
    a <- split(data_1,data_1$asset_id)
    firms <- length(a)
    cat('初始计算firms为：',firms,'\n')
  
  

# ----------- st 股 ------------------- #

#spt<-read.csv('SPT_Trdchg.csv',header = F,quote = "")
a<-sort(spt[,1])
index<-duplicated(a)
a_del<-a[!index] # not delete ,only print the stock id 
a<-a_del[-(908:957)]
a<-substring(a,2,7)
a<-as.integer(a)
a<-na.omit(a)  # length(a) = 902
data_new<-filter(data_1,!(data_1$asset_id %in% a))  

# 计算剔除2012-2019年间的ST股后的observations和firms
a <- observations - nrow(data_new)
observations <- nrow(data_new)
cat('剔除st股后的observations差额：',a,'\n')

a <- split(data_new,data_new$asset_id)
b <- firms - length(a)
firms <- length(a)
cat('剔除st股后的firms差额：',b,'\n')

# ----------- 借壳上市 ------------------ #

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


### ---------------------------------- 从这里开始要开始按月份逐月计算

# ----------- ipo < 1 year ------------- #

for(mon in 1:60)
{
  id<-eripo[which(eripo$IpoDate > IPO_times[mon]),] # 上市时间在该窗口开始时间一年以内股票,时间线较长，到2021年
  id<-id$Symbol
  dtset<-filter(data_new,!(data_new$asset_id %in% id))
  
  if(mon == 1)
    firmsdel.IPO = unique(data_new$asset_id[!(data_new$asset_id %in% dtset$asset_id)])
  else
    firmsdel.IPO = union(firmsdel.IPO,unique(data_new$asset_id[!(data_new$asset_id %in% dtset$asset_id)]))
    #cat('删除上市未满一年的股票：',length(firmsdel.IPO),'\n')
}

dtset <- data_new[!(data_new$asset_id %in% firmsdel.IPO),]
cat('删除上市未满一年的股票observations：',observations - nrow(dtset),'\n')
observations = nrow(dtset)

cat('删除上市未满一年的股票：',length(firmsdel.IPO),'\n')
firms = firms - length(firmsdel.IPO)

#data_new$logprof<-log(1+as.numeric(data_new$prof))




## ----------------------------- 开始有行业的区别

# ----------- 改变了行业的 ----------- #
tests<-split(dtset,dtset$asset_id) 

# classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
#                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始
#                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
#                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位

num = 7
c<-c()
for(i in 1:length(tests))  
{
  #print(i)
  t<-duplicated(tests[[i]][,num]) # changed, but not run yet
  for(j in 1:(length(t)-1))
  {
    if(t[j+1] == FALSE)
      c <- append(c,tests[[i]]$asset_id[1])
    #c <- append(c,tests[[i]][,num][1])
  }
}
c<-sort(c)
index<-duplicated(c)
c_del<-c[!index] # not delete ,only print the asset_id (stored in the c_del)
# delete those who has changed their industry
data_change<-dtset[!(dtset$asset_id %in% c_del),]

cat('改变了行业的observations数量；',observations - nrow(data_change),'\n')
cat('改变了行业的股票数 : ',firms - length(unique(data_change$asset_id)),'\n')  

observations = nrow(data_change)
firms = length(unique(data_change$asset_id))


# ------------- 少于5个--------------- #
# classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
#                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始
#                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
#                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位
for(mon in 1:60)
{
  lessthan5id = c()
  for(m in 1:21)
  {
    id <- calculate_lessthan5(data_change,num,mon = mon+m-1)
    if(m ==1)
      lessthan5id = id
    else
      lessthan5id = append(lessthan5id,id)
  }
}

lessthan5id = unique(lessthan5id)

data_change_5 <- data_change[!(data_change[,num] %in% lessthan5id),]

cat('剔除少于5个公司的数据的observations:',observations-nrow(data_change_5),'\n') 
observations = nrow(data_change_5)

cat('剔除少于5个公司的数据的firms:',firms - length(unique(data_change_5$asset_id)),'\n')
firms = length(unique(data_change_5$asset_id))


# -------------- logprof >1 ｜logprof <-1 -------------- #

data_change_5$logprof = log(1+as.numeric(data_change_5$prof))

data_change_5.log <- data_change_5 %>% filter(logprof >= -1 & logprof <= 1)   

# 剔除在2012-2019年间收益过大或过小的observations和firms
#observations <- nrow(data_new)
cat('剔除在2012-2019年间收益过大或过小的observations差额：',observations - nrow(data_change_5.log),'\n')
observations = nrow(data_change_5.log)

cat('剔除在2012-2019年间收益过大或过小的firms差额：',firms - length(unique(data_change_5.log$asset_id)),'\n')
firms = length(unique(data_change_5.log$asset_id))

# ---------------- 没有连续21个月的数据 -------------- #

for(mon in 1:60)
    {
  resultD <- Delete_missing21(data_change_5.log,mon) 
  data_use <- resultD$data  # dt=data （得到的是21个月数据)
  if(mon == 1)
    firmsdel = unique(data_use$asset_id)
  else
    firmsdel = intersect(firmsdel,unique(data_use$asset_id))
}

data_use = data_change_5.log %>% filter(asset_id %in% firmsdel)

cat('剔除没有连续21个月的数据observations：',observations - nrow(data_use),'\n')
#observations = nrow(data_use)
nrow(data_use)
cat('剔除没有连续21个月的数据的firms：',firms - length(unique(data_use$asset_id)),'\n')

length(unique(data_use$asset_id))
length(firmsdel)


