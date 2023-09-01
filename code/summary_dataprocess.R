library(dplyr)
library(stringr)
library(reshape)
library(readr)

# industry_id------num=5
# ind_4------------num=7
# ind_2------------num=8


end_times<-c('2014-03','2014-04','2014-05','2014-06','2014-07','2014-08','2014-09','2014-10',
             '2014-11','2014-12','2015-01','2015-02','2015-03','2015-04','2015-05','2015-06',
             '2015-07','2015-08','2015-09','2015-10','2015-11','2015-12','2016-01','2016-02',
             '2016-03','2016-04','2016-05','2016-06','2016-07','2016-08','2016-09','2016-10',
             '2016-11','2016-12','2017-01','2017-02','2017-03','2017-04','2017-05','2017-06',
             '2017-07','2017-08','2017-09','2017-10','2017-11','2017-12','2018-01','2018-02',
             '2018-03','2018-04','2018-05','2018-06','2018-07','2018-08','2018-09','2018-10',
             '2018-11','2018-12','2019-01','2019-02')

end_times_2<-c('2014-02','2014-03','2014-04','2014-05','2014-06','2014-07','2014-08','2014-09',
               '2014-10','2014-11','2014-12','2015-01','2015-02','2015-03','2015-04','2015-05',
               '2015-06','2015-07','2015-08','2015-09','2015-10','2015-11','2015-12','2016-01',
               '2016-02','2016-03','2016-04','2016-05','2016-06','2016-07','2016-08','2016-09',
               '2016-10','2016-11','2016-12','2017-01','2017-02','2017-03','2017-04','2017-05',
               '2017-06','2017-07','2017-08','2017-09','2017-10','2017-11','2017-12','2018-01',
               '2018-02','2018-03','2018-04','2018-05','2018-06','2018-07','2018-08','2018-09',
               '2018-10','2018-11','2018-12','2019-01')

end_times_true<-c('2014-02-01','2014-03-01','2014-04-01','2014-05-01','2014-06-01','2014-07-01',
                  '2014-08-01','2014-09-01','2014-10-01','2014-11-01','2014-12-01','2015-01-01', '2015-02-01',
                  '2015-03-01','2015-04-01','2015-05-01','2015-06-01','2015-07-01','2015-08-01','2015-09-01',
                  '2015-10-01','2015-11-01','2015-12-01','2016-01-01','2016-02-01','2016-03-01','2016-04-01',
                  '2016-05-01','2016-06-01','2016-07-01','2016-08-01','2016-09-01','2016-10-01','2016-11-01',
                  '2016-12-01','2017-01-01','2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01',
                  '2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01','2017-12-01','2018-01-01',
                  '2018-02-01','2018-03-01','2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01',
                  '2018-09-01','2018-10-01','2018-11-01','2018-12-01','2019-01-01')

start_times<-c('2012-06','2012-07','2012-08','2012-09','2012-10','2012-11','2012-12','2013-01',
               '2013-02','2013-03','2013-04','2013-05','2013-06','2013-07','2013-08','2013-09',
               '2013-10','2013-11','2013-12','2014-01','2014-02','2014-03','2014-04','2014-05',
               '2014-06','2014-07','2014-08','2014-09','2014-10','2014-11','2014-12','2015-01',
               '2015-02','2015-03','2015-04','2015-05','2015-06','2015-07','2015-08','2015-09',
               '2015-10','2015-11','2015-12','2016-01','2016-02','2016-03','2016-04','2016-05',
               '2016-06','2016-07','2016-08','2016-09','2016-10','2016-11','2016-12','2017-01',
               '2017-02','2017-03','2017-04','2017-05')

IPO_times<-c('2011-06','2011-07','2011-08','2011-09','2011-10','2011-11','2011-12','2012-01',
               '2012-02','2012-03','2012-04','2012-05','2012-06','2012-07','2012-08','2012-09',
               '2012-10','2012-11','2012-12','2013-01','2013-02','2013-03','2013-04','2013-05',
               '2013-06','2013-07','2013-08','2013-09','2013-10','2013-11','2013-12','2014-01',
               '2014-02','2014-03','2014-04','2014-05','2014-06','2014-07','2014-08','2014-09',
               '2014-10','2014-11','2014-12','2015-01','2015-02','2015-03','2015-04','2015-05',
               '2015-06','2015-07','2015-08','2015-09','2015-10','2015-11','2015-12','2016-01',
               '2016-02','2016-03','2016-04','2016-05')

start_times_true<-c('2012-06-01','2012-07-01','2012-08-01','2012-09-01','2012-10-01','2012-11-01','2012-12-01','2013-01-01',
                    '2013-02-01','2013-03-01','2013-04-01','2013-05-01','2013-06-01','2013-07-01','2013-08-01','2013-09-01',
                    '2013-10-01','2013-11-01','2013-12-01','2014-01-01','2014-02-01','2014-03-01','2014-04-01','2014-05-01',
                    '2014-06-01','2014-07-01','2014-08-01','2014-09-01','2014-10-01','2014-11-01','2014-12-01','2015-01-01',
                    '2015-02-01','2015-03-01','2015-04-01','2015-05-01','2015-06-01','2015-07-01','2015-08-01','2015-09-01',
                    '2015-10-01','2015-11-01','2015-12-01','2016-01-01','2016-02-01','2016-03-01','2016-04-01','2016-05-01',
                    '2016-06-01','2016-07-01','2016-08-01','2016-09-01','2016-10-01','2016-11-01','2016-12-01','2017-01-01',
                    '2017-02-01','2017-03-01','2017-04-01','2017-05-01')

full_times<-c('2012-06-01','2012-07-01','2012-08-01','2012-09-01','2012-10-01','2012-11-01','2012-12-01',
              '2013-01-01','2013-02-01','2013-03-01','2013-04-01','2013-05-01','2013-06-01','2013-07-01',
              '2013-08-01','2013-09-01','2013-10-01','2013-11-01','2013-12-01','2014-01-01','2014-02-01',
              '2014-03-01','2014-04-01','2014-05-01','2014-06-01','2014-07-01','2014-08-01','2014-09-01',
              '2014-10-01','2014-11-01','2014-12-01','2015-01-01', '2015-02-01','2015-03-01','2015-04-01',
              '2015-05-01','2015-06-01','2015-07-01','2015-08-01','2015-09-01', '2015-10-01','2015-11-01',
              '2015-12-01','2016-01-01','2016-02-01','2016-03-01','2016-04-01','2016-05-01','2016-06-01',
              '2016-07-01','2016-08-01','2016-09-01','2016-10-01','2016-11-01','2016-12-01','2017-01-01',
              '2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01', '2017-07-01','2017-08-01',
              '2017-09-01','2017-10-01','2017-11-01','2017-12-01','2018-01-01','2018-02-01','2018-03-01',
              '2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01','2018-09-01','2018-10-01',
              '2018-11-01','2018-12-01','2019-01-01')

true_times<-c('2014-01-01','2014-02-01','2014-03-01','2014-04-01','2014-05-01','2014-06-01','2014-07-01',
              '2014-08-01','2014-09-01','2014-10-01','2014-11-01','2014-12-01','2015-01-01', '2015-02-01',
              '2015-03-01','2015-04-01','2015-05-01','2015-06-01','2015-07-01','2015-08-01','2015-09-01',
              '2015-10-01','2015-11-01','2015-12-01','2016-01-01','2016-02-01','2016-03-01','2016-04-01',
              '2016-05-01','2016-06-01','2016-07-01','2016-08-01','2016-09-01','2016-10-01','2016-11-01',
              '2016-12-01','2017-01-01','2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01',
              '2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01','2017-12-01','2018-01-01',
              '2018-02-01','2018-03-01','2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01',
              '2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01','2018-09-01','2018-10-01',
              '2018-11-01','2018-12-01')



# ----------------------------- Data-Precessing ------------------------------ #
# Function Preapare

# 在data——process之前的处理
# 删除借壳上市，st股以及logprof > 1 or < -1 的outlier ，选择不同的行业分类
Data_P <- function(classification)
  # classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
  #                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始
  #                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
  #                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位
{
  data<-read.csv('resample.csv',encoding='UTF-8') # data = resample
  profit<-read.csv('month_profit_resample.csv',encoding='UTF-8')
  ts<-substring(profit$ts,1,10)
  prof<-profit$cash_profit
  asset_id<-profit$asset_id
  re<-cbind(ts,prof,asset_id)
  re<-data.frame(re)
  
  sum(is.na(re[,2])) # 统计re中含有NA值的个数
  re<-na.omit(re) # 剔除缺失值
  
  class<-filter(data,data$classification_id==classification)# choose the portfolio 
  
  sid<-intersect(re$asset_id,class$asset_id)
  class<-class[which(class$asset_id %in% sid),] 
  re<-re[which(re$asset_id %in% sid),]  # 两个数据集根据asset_id 取交集
  ## sy
  #ind_4<-substring(class$industry_id,1,4)
  #ind_2<-substring(class$industry_id,1,2)
  
  #zz
  #ind_6<-substring(class$industry_id,1,6) 
  #ind_4<-substring(class$industry_id,1,4)
  #ind_2<-substring(class$industry_id,1,2)
  
  ## zx
  ind_4<-substring(class$industry_id,3,6)
  ind_2<-substring(class$industry_id,3,4)
  
  ## zj
  #ind_1 <- substring(class$industry_id,1,1)
  
  class<-cbind(class,
               #ind_6,           # for zz
               ind_4,ind_2
               #ind_1
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
  cat('剔除st股后的observations差额：',a,'\n')
  
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
  #data_new <- data_new %>% filter(logprof>=-1 & logprof<=1)   
  
  # 剔除在2012-2019年间收益过大或过小的observations和firms
  #a <- observations - nrow(data_new)
  #observations <- nrow(data_new)
  #cat('剔除在2012-2019年间收益过大或过小的observations差额：',a,'\n')
  
  #a <- split(data_new,data_new$asset_id)
  #b <- firms - length(a)
  #firms <- length(a)
  #cat('剔除在2012-2019年间收益过大或过小的firms差额：',b,'\n')
  
  #cat('最终参与计算的observations：',observations,'\n')
  #cat('最终参与计算的firms：',firms,'\n')
  
  return(data_new)
}

Delete_missing21 <- function(dt,mon) # (data,1)
  # mon 表示的是窗口的个数
{
  dtsets <- dt[which(dt$ts < end_times[mon] & dt$ts > start_times[mon]),] # （共21个月数据）
  D <- dtsets
  dt_div <- split(dtsets,dtsets$asset_id)
  
  #firms <- length(dt_div) # 这一个窗口所包含的股票个数
  
  ## 提取初始数据的firms
  #firmsb <- c()
  #for(i in 1:length(dt_div))
  #    {
  #  firmsb <- append(firmsb,dt_div[[i]]$asset_id[1])
  #}
  firmsb <- unique(D$asset_id)
  
  # 遍历窗口，删除所有不满足21个月连续的股票
  for(i in 1:length(dt_div))  
  {
    l <- nrow(dt_div[[i]])
    if(l<21)
      D <- filter(D,!(D$asset_id %in% dt_div[[i]]$asset_id))
    else
      D <- D
  }
  
  a<-split(D,D$asset_id)
  
  # 提取处理后数据的firms( 删除不满21个月连续数据的股票)
  firms21 <- unique(D$asset_id)
  firmsdel.21 <- firmsb[!(firmsb %in% firms21)]
  #cat('observations of the data_full:',nrow(dtsets),'\n')
  #cat('firms of data_full',length(dt_div),'\n')
  #cat('missing observations:',firms-nrow(D),'\n')
  # t <- intersect(firmsb,firms_del)
  #cat('剔除没有连续21个月的数据后的firms:',length(a),'\n')
  #cat('剔除没有连续21个月的数据的firms:',length(firms21) - length(firmsb),'\n')
  
  #cat('剔除没有连续21个月的数据后的observations:',nrow(D),'\n')
  #cat('剔除没有连续21个月的数据的observations:',nrow(dtsets)-nrow(D),'\n')

  t <- data.frame(mon = mon,
                  operation = 'less than 21 months',
                  obs0.mon = nrow(dtsets),
                  firms0.mon = length(dt_div),
                  obs1.mon = nrow(D),
                  firms1.mon = length(a),
                  missobs = nrow(dtsets)-nrow(D),
                  missfirms = length(dt_div) - length(a)
                  #firms.delete21.mon = firms21
                  )
  write_csv(t,'record.csv',append = T,col_names = T) # 运行时第二类数据添加
  write_csv(as.data.frame(firmsdel.21),'record.csv',append = T,col_names = T)
  
  D <- list(data = D,firmsb = firmsb,firms21 = firms21)
  #cat('检验--数据集相减所得缺失股票数：',length(dt_div)-length(D),'\n')
  return(D) 
}

calculate_lessthan5<-function(dt,num,mon)    # 返回小于5个firm的行业id
{
  #numbers=0
  less<-c()
  #I<-c()
  a<-dt[which(dt$ts == full_times[mon]),]
  
  s<-split(a,a[,num]) 
  
  for(i in 1:length(s))  # 计算该时间full_times[i]下有多少个行业  
  {
    if(nrow(s[[i]])<5)
    {
      #I<-append(I,i)
      #numbers=numbers+1
      less<-append(less,s[[i]][,num][1]) # 行业id
    }
  }
  #cat('number of those who has less than 5 firms :',numbers,'\n')  # 78
  #cat('those industry who lack 5 firms are:' ,'\n',less,'\n')
  return(less)
}

Calculate_industryReturn<-function(obs,num,mon) # 每一只股票，每一个月的industry—level 都不一样
{
  MI<-split(obs,obs[,num])   # 按行业分割
  for(i in 1:length(MI)) # 第i个行业
  {
    #cat('i = ',i,'\n')
    # 去除重复的股票id，得到该行业分类下所有的股票id
    a<-unique(MI[[i]]$asset_id)
    #index<-duplicated(a)
    #a<-a[!index]
    for(j in 1:21)    # 得到的是一个行业下，一个窗口21个月不同股票对应的行业收益率 
    {
      #cat('j = ',j,'\n')
      b<-MI[[i]] %>% filter(ts == full_times[mon:(mon+20)][j]) # 第i个行业 第j月的数据集
      for(k in 1:nrow(b))
        b$re_ind[k]<-mean(as.numeric(b[-k,]$prof)) # 去除第 k 只股票对应的收益率
      if(j==1)
        B<-b
      else
        B<-rbind(B,b)
    }
    if(i==1)
      return<-B
    else
      # result <- rbind(return,B)
      return <- rbind(return,B)
    #print(sum(is.na(return)))
  }
  # return(result)
  return(return)
}

Calculate_marketReturn<-function(obs,num,mon) # 第mon个窗口的计算
{
  MI<-split(obs,obs$ts)
  # 第 i 个月
  for(i in 1:21)
  {
    # print(i)
    for(j in 1:nrow(MI[[i]]))
      MI[[i]]$re_market[j]=mean(as.numeric(MI[[i]][-j,]$prof)) # 第i个月 第j个股票的市场收益
    if(i == 1)
      B <- MI[[i]]
    else
      B <- rbind(B,MI[[i]])
  }
  return(B)
}


# Summary of the function
Data_Process<-function(dt,num,mon) # 第mon个月，按照num进行划分得到的结果
  # dt 选取 data_new --- 剔除st、借壳上市的股票、ln(1+re)过大或过小 后的数据集
{
  ## ------------------------- 剔除上市未满一年的股票
  id<-eripo[which(eripo$IpoDate > IPO_times[mon]),] # 上市时间在该窗口开始时间一年以内股票,时间线较长，到2021年
  id<-id$Symbol
  dtset<-filter(dt,!(dt$asset_id %in% id))
  
  
  if(mon == 1)
  {
    firmsdel.IPO = unique(dt$asset_id[!(dt$asset_id %in% dtset$asset_id)])
    cat('删除上市未满一年的股票：',length(firmsdel.IPO),'\n')
  }
  else
  {
    firmsdel.IPO = append(firmsdel.IPO,dt$asset_id[!(dt$asset_id %in% dtset$asset_id)])
    firmsdel.IPO = unique(firmsdel.IPO)
  }
  cat('删除上市未满一年的obs：',nrow(dt) - nrow(dtset),'\n')
  
  t <- data.frame(mon = mon,
                  operation = 'IPO < 1 year',
                  missobs.IPO = 'nrow(dt) - nrow(dtset)',
                  missobs.IPO.num = nrow(dt) - nrow(dtset),
                  missobs.IPO.1 = 'length(firmsdel.IPO)',
                  missobs.IPO.num = length(firmsdel.IPO)
                  #missfirms.IPO = length(unique(dt$asset_id[!(dt$asset_id %in% dtset$asset_id)]))
                  #firms.IPO.mon = id
  )
  write_csv(t,'record.csv')
  write_csv(as.data.frame(firmsdel.IPO),'record.csv',append = TRUE,col_names = T)
  #firms0 <- length(split(data$asset_id))
  #firms1 <- length(split(dtset$asset_id))
  #firms0 <- c()
  #for(i in 1:length(dt_div))
  #{
  #  firmsb <- append(firmsb,dt_div[[i]]$asset_id[1])
  #}
  
  #cat('nrow dtset:(122181)',nrow(dtset),'\n')
  
  # delete those who lack 21 month observations
  #resultD <- Delete_missing21(dtset,mon) 
  #data_less <- resultD$data  # dt=data （得到的是21个月数据)
  #cat('nrow(data_less)/21--1300',nrow(data_less)/21,'\n')
  #if(mon ==1)
  #{
  #  firmsb = resultD$firmsb
  #  firms21 = data_less$firms21
  #}
  #else
  #{
  #  firmsb <- append(firmsb,data_less$firmsb)
  #  firms21 <- append(firms21,data_less$firms21)
  #}
  
  #obsdel.21 <- nrow(data_new) - nrow(data_less) 
  #firmsdel.21 = length(unique(data_less$asset_id)) - length(unique(dtset$asset_id))
  
  #cat('剔除没有连续21个月的数据后的firms:',length(unique(dtset$asset_id)),'\n')
  #cat('剔除没有连续21个月的数据的firms:',firmsdel.21,'\n')
  #cat('剔除没有连续21个月的数据后的observations:',nrow(data_less),'\n')
  #cat('剔除没有连续21个月的数据的observations:',obsdel.21,'\n')
  # test whether every stock in the FIRST window all has the 21 months observations
  
  tests<-split(dtset,dtset$asset_id) 
  
  #for(i in 1:length(tests))
  #{
  #  print(nrow(tests[[i]]))
  #  lid<-nrow(tests[[i]])
  #  if(lid<21)
  #     cat('after Delete_missing21,there are still stocks who has less than 21 months observations:','\n',i,'\n') 
                             # print those who still lack 21 months observations 'the order' not the stock id 
  #}
  # print 输出
  #cat('firms of the data_less:',length(tests),'\n')
  
  ## ---------------------- figure out those who has changed industry
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
  
  cat('改变了行业的observations数量；',nrow(dtset) - nrow(data_change),'\n')
  cat('改变了行业的股票数 : ',length(unique(dtset$asset_id)) - length(unique(data_change$asset_id)),'\n')  
  
  # 统计发生了改变的stockid
  
  
  if(mon == 1)
    firmsdel.change <- c_del
  else
    firmsdel.change <- append(firmsdel.change,c_del)
  
  firmsdel.change = unique(firmsdel.change)
  
  t <- data.frame(mon = mon,
                  operation = 'industry has changed',
                  obs.ipo = nrow(dtset),
                  obs.change = nrow(data_change),
                  firms.ipo = length(unique(dtset$asset_id)),
                  firms.change = length(unique(data_change$asset_id)),
                  missobs.change.mon = nrow(dtset) - nrow(data_change),
                  missfirms.change.mon = length(c_del)
                  #firms.change.mon = c_del
  )
  write_csv(t,'record.csv',append = T)
  write_csv(as.data.frame(c_del),'record.csv',append = T,col_names = T)
  
  #cat('nrow of data_less change --25767:',nrow(data_less_change),'\n')
  
  #a <- split(data_less_change,data_less_change$asset_id)
  # 提取经过处理后的股票id
  #c_change <- c()
  #for(i in 1:length(a))
  #{
  #  c_change <- append(c_change,a[[i]]$asset_id[1])
  #}
  
  #cat('剔除行业发生改变的数据后的firms:',length(a),'\n')
  #cat('剔除行业发生改变的数据的firms差额:',length(a)-length(firms),'\n')
  
  #cat('剔除行业发生改变的数据后的observations:',nrow(data_less_change),'\n')
  #cat('剔除行业发生改变的数据的observations差额:',nrow(data_less)-nrow(data_less_change),'\n') 
  
  #t <- data.frame(mon = mon,
  #                operation = 'industry changed ',
  #                obs0 = '-',
  #                firms0 = '-',
  #                obs1 = nrow(data_less_change),
  #                firms1 = length(a),
  #                missobs = nrow(data_less)-nrow(data_less_change),
  #                missfirms = length(a)-length(firms)
  #)
  #write_csv(t,'record.csv',col_names = TRUE,append = TRUE)
  

  # cat('检验--改变了行业的股票数',length(tests)-length(a),'\n')
  #cat('those who has changed their industry are: ','\n',c_del,'\n') # 输出
  # cat('observations of data_less: ',nrow(data_less),'\n')
  
  # find out those who has less than 5 firms, in the 21 months
  #lessthan5id = c()
  #for(m in 1:21)
  #    lesstha5id <- append(lessthan5id,calculate_lessthan5(data_change,num,mon = mon+m-1))
  
  #lessthan5id = unique(lessthan5id)
   # 得到的是行业id
  
  
  ## ----------------------- delete those who has less than 5 firms
  lessthan5id = c()
  for(m in 1:21)
  {
    id <- calculate_lessthan5(data_change,num,mon = mon+m-1)
    if(m ==1)
      lessthan5id = id
    else
      lessthan5id = append(lessthan5id,id)
  }
  
  lessthan5id = unique(lessthan5id)
  
  data_change_5 <- data_change[!(data_change[,num] %in% lessthan5id),]
  
  stockid <- unique(data_change$asset_id)
  stockid1 <- unique(data_change_5$asset_id)
  firmsdel <- stockid[!(stockid %in% stockid1)]
  
  cat('剔除少于5个公司的数据的observations:',nrow(data_change)-nrow(data_change_5),'\n') 
  
  t <- data.frame(mon = mon,
                  operation = 'industry less than 5 firms',
                  firms.change = length(stockid),
                  firms.change.5 = length(stockid1),
                  obs.less.change = nrow(data_change),
                  obs.less.change.5  = nrow(data_change_5),
                  missobs = nrow(data_change)-nrow(data_change_5)
  )
  
  write_csv(t,'record.csv',append = T)
  
  # if(mon == 1)
  # {
  firmsdel.5 = firmsdel
  #  cat('剔除少于5个公司的数据的firms:',length(firmsdel.5),'\n')
  #}
  #else
  # firmsdel.5 = append(firmsdel.5,firmsdel)
  
  #firmsdel.5 = unique(firmsdel.5)
  #cat('剔除少于5个公司的数据的firms差额:',firnum - length(a),'\n')
  
  #cat('剔除少于5个公司的数据后的observations:',nrow(data_use),'\n')

  #write_csv(as.data.frame(firmsdel.5),'record.csv',append = TRUE,col_names = T)
  #cat('nrow of data_use:--24087',nrow(data_use),'\n')
  #firnum <- length(a)
  #a <- split(data_use,data_use$asset_id)
  #firms0 <- firms_del
  #firmsd <- c()  
  #for(i in 1:length(a))
  #{
  #  firmsd <- append(firmsd,a[[i]]$asset_id[1])
  #}
  # a <- length(firms0) - length(firms_del)
  #cat('少于5个公司的行业股票数：',length(a)-length(b),'\n')
  # data-(delete missing)-->data_less--(delete change)-->data_less_change--(delete 5)-->data_use
  # cat('observations difference of data_less and data_use:',nrow(data_less)-nrow(data_use),'\n')
  #cat('在删除21个月和行业发生改变的基础上，删除firms少于5个的observations:',nrow(data_less_change)-nrow(data_use),'\n')
 
  
  
  ## -------------------- 剔除收益过大或过小的股票
  #data_use$logprof<-log(1+as.numeric(data_use$prof))
  data_change_5.log <- data_change_5 %>% filter(logprof >= -1 & logprof <= 1)   
  
  # 剔除在2012-2019年间收益过大或过小的observations和firms
  obsdel.log <- nrow(data_change_5) - nrow(data_change_5.log)
  #observations <- nrow(data_new)
  cat('剔除在2012-2019年间收益过大或过小的observations差额：',obsdel.log,'\n')
  
  firms0 <- unique(data_change_5$asset_id)
  firms1 <- unique(data_change_5.log$asset_id)
  firmsdel.log <- firms0[!(firms0 %in% firms1)]
  cat('剔除在2012-2019年间收益过大或过小的firms差额：',length(firmsdel.log),'\n')
  
  cat('最终参与计算的observations：',nrow(data_change_5.log),'\n')
  cat('最终参与计算的firms：',length(firms1),'\n')
  
  t <- data.frame(mon = mon,
                  operation = 'logprof large or small',
                  firmsdel.log = length(firmsdel.log),
                  obsdel.log = length(obsdel.log),
                  obs.use = nrow(data_change_5.log),
                  firms.use = length(firms1)
  )
  
  write_csv(t,'record.csv',append = T)
  write_csv(as.data.frame(firmsdel.log),'record.csv',append = TRUE,col_names = T)
  
  
  ## ------------------------------- 删除小于21个月的数据
  resultD <- Delete_missing21(data_change_5.log,mon) 
  data_use <- resultD$data  # dt=data （得到的是21个月数据)
  #cat('nrow(data_less)/21--1300',nrow(data_less)/21,'\n')
  #if(mon ==1)
  #{
  #  firmsb = resultD$firmsb
  #  firms21 = data_less$firms21
  #}
  #else
  #{
  #  firmsb <- append(firmsb,data_less$firmsb)
  #  firms21 <- append(firms21,data_less$firms21)
  #}
  
  obsdel.21 <- nrow(data_change_5.log) - nrow(data_use) 
  firmsdel.21 = length(unique(data_use$asset_id)) - length(unique(data_change_5.log$asset_id))
  
  cat('剔除没有连续21个月的数据后的firms:',length(unique(data_change_5.log$asset_id)),'\n')
  cat('剔除没有连续21个月的数据的firms:',firmsdel.21,'\n')
  
  cat('剔除没有连续21个月的数据后的observations:',nrow(data_use),'\n')
  cat('剔除没有连续21个月的数据的observations:',obsdel.21,'\n')
  # test whether every stock in the FIRST window all has the 21 months obser
  
  
  # Calculate the industry-level return.exclude the firm itself
  Re_Ind <- Calculate_industryReturn(data_use,num,mon) # 数据集
  #print('Re_Ind')
  #cat('nrow of Re_Ind---567:',nrow(Re_Ind),'\n')
  # Calculate the market-level return.exclude the firm itself
  re_indmarket <- Calculate_marketReturn(Re_Ind,num,mon)
  
  
  firmsdel.IPO = unique(firmsdel.IPO)
  firmsdel.21 <- firmsb[!(firmsb %in% firms21)]
  firmsdel.change <- unique(firmsdel.change)
  #firmsdel.5 = unique(firmsdel.5)
  
  result <- list(re_indmarket = re_indmarket,
                 firmsdel.IPO,firmsdel.21,firmsdel.change,firmsdel.5)
  # write.csv(re_indmarket,'data_include_indmarket.csv')
  
  return(result)
}

# usage
# re_indmarket<-Data_Process(data,5,1) --得到经过处理用于回归的数据，该数据包括第1个窗口的所有数据
# re_4<-Data_Process(data,7,1)
# re_2<-Data_Process(data,8,1)


#-----------------------------------Regression----------------------------------#
#Regression2a<-function(dt,condition,mon) # dt = re_indmarket(for example),condition = before/after
{
  if(condition == 1)
  {
    re_indmarket_20<-dt %>% filter(!(ts == end_times_true[mon]))# 筛出最后一个月的数据
    re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
    for(i in 1:(nrow(dt)/21))
    {
      #print(i)
      y<-log(1+re_indmarket_20$re_ind[(20*(i-1)+1) : (20*i)])
      x<-log(1+re_indmarket_20$re_market[(20*(i-1)+1) : (20*i)])
      if(i==1)
      {
        #print(y)
        #print(x)
        #lim<-lm(y~x)
        #print(summary(lim))
        res<-lm(y~x)$residuals
        #print(res)
        #b0<-lm(y~x)$coefficients[1]
        #b1<-lm(y~x)$coefficients[2]
      }
      else
      {
        res<-append(res,lm(y~x)$residuals)
        #b0<-append(b0,lm(y~x)$coefficients[1])
        #b1<-append(b1,lm(y~x)$coefficients[2])
      }
    }
    #b<-data.frame(b0,b1)
    #write.csv(b,'b.csv')
    return(res)
  }
  else
  {
    re_indmarket_20<-dt %>% filter(!(ts==start_times_true[mon]))# 筛出第一个月的数据
    re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
    for(i in 1:(nrow(dt)/21))
    {
      y<-log(1+re_indmarket_20$re_ind[(20*(i-1)+1) : (20*i)])
      x<-log(1+re_indmarket_20$re_market[(20*(i-1)+1) : (20*i)])
      if(i==1)
        res<-lm(y~x)$residuals
      else
        res<-append(res,lm(y~x)$residuals)
    }
    return(res)
  }
} # condition = 1 represents the first 20 months

#Regression2b<-function(dt,condition,mon)
{
  residuals<-Regression2a(dt,condition,mon)
  if(condition == 1)
  {
    re_indmarket_20<-dt %>% filter(!(ts == end_times_true[mon]))# 筛出最后一个月的数据
    re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
    for(j in 1:(nrow(dt)/21))
    {
      #print(i)
      y<-log(1+as.numeric(re_indmarket_20$prof[(20*(j-1)+1) : (20*j)]))
      x1<-residuals[(20*(j-1)+1) : (20*j)]    # residuals = Regression2a(dt,condition)
      x2<-log(1+re_indmarket_20$re_market[(20*(j-1)+1) : (20*j)])
      #print(sum(is.na(y)))
      #print(sum(is.na(x1)))
      #print(sum(is.na(x2)))
      if(j==1)
      {
        #print(y)
        #print(x1)
        #print(x2)
        beta1<-rep(lm(y~x1+x2)$coefficients[2],each = 20)
        beta2<-rep(lm(y~x1+x2)$coefficients[3],each = 20)
        #print(summary(lm(y~x1+x2)))
        #print(lm(y~x1+x2)$residuals)
      }
      else
      {
        beta1<-append(beta1,rep(lm(y~x1+x2)$coefficients[2],each = 20))
        beta2<-append(beta2,rep(lm(y~x1+x2)$coefficients[3],each = 20))
      }
    }
    beta<-data.frame(beta1=beta1,beta2=beta2) 
    return(beta)
  }
  else
  {
    re_indmarket_20<-dt %>% filter(!(ts == start_times_true[mon]))# 筛出第一个月的数据
    re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
    for(j in 1:(nrow(dt)/21))
    {
      y<-log(1+as.numeric(re_indmarket_20$prof[(20*(j-1)+1) : (20*j)]))
      x1<-residuals[(20*(j-1)+1) : (20*j)]
      x2<-log(1+re_indmarket_20$re_market[(20*(j-1)+1) : (20*j)])
      if(j==1)
      {
        beta1<-rep(lm(y~x1+x2)$coefficients[2],each = 20)
        beta2<-rep(lm(y~x1+x2)$coefficients[3],each = 20)
        print(lm(y~x1+x2)$residuals)
      }
      else
      {
        beta1<-append(beta1,rep(lm(y~x1+x2)$coefficients[2],each = 20))
        beta2<-append(beta2,rep(lm(y~x1+x2)$coefficients[3],each = 20))
      }
    }
    beta<-data.frame(beta1=beta1,beta2=beta2) 
    return(beta)
  }
}

# ------- regression 2 & regression 3 ------- #
# dt = DataProcess $ indmarket
Regression<-function(dt,condition,mon)
{
  if(condition == 1)
  {
    re_indmarket_20<-dt %>% filter(!(ts == end_times_true[mon]))# 筛出最后一个月的数据
    re_indmarket_20<-re_indmarket_20[order(re_indmarket_20$asset_id),]
    for(i in 1:(nrow(dt)/21))
    {
      #print(i)
      y<-log(1+re_indmarket_20$re_ind[(20*(i-1)+1) : (20*i)])    # 20个月为一个窗口（包括自己，向前推20个月）
      x<-log(1+re_indmarket_20$re_market[(20*(i-1)+1) : (20*i)])
      if(i==1)
      {
        #print(y)
        #print(x)
        #lim<-lm(y~x)
        #print(summary(lim))
        res2a<-lm(y~x)$residuals
        #print(res)
        #b0<-lm(y~x)$coefficients[1]
        #b1<-lm(y~x)$coefficients[2]
      }
      else
      {
        res2a<-append(res2a,lm(y~x)$residuals)
        #b0<-append(b0,lm(y~x)$coefficients[1])
        #b1<-append(b1,lm(y~x)$coefficients[2])
      }
    }
    for(j in 1:(nrow(dt)/21))
    {
      y<-log(1+as.numeric(re_indmarket_20$prof[(20*(j-1)+1) : (20*j)]))
      x1<-res2a[(20*(j-1)+1) : (20*j)]
      x2<-log(1+re_indmarket_20$re_market[(20*(j-1)+1) : (20*j)])
      if(j==1)
      {
        #beta0<-rep(lm(y~x1+x2)$coefficients[1],each = 20)
        beta1<-rep(lm(y~x1+x2)$coefficients[2],each = 20)
        beta2<-rep(lm(y~x1+x2)$coefficients[3],each = 20)
        #print(lm(y~x1+x2)$residuals)
        res_2b<-lm(y~x1+x2)$residuals
      }
      else
      {
        beta1<-append(beta1,rep(lm(y~x1+x2)$coefficients[2],each = 20))
        beta2<-append(beta2,rep(lm(y~x1+x2)$coefficients[3],each = 20))
        res_2b<-append(res_2b,lm(y~x1+x2)$residuals)
      }
    }
    beta<-data.frame(beta1=beta1,beta2=beta2,res2b=res_2b) 

    re_indmarket_20<-cbind(re_indmarket_20,res2a,beta)
    MktRe<-(re_indmarket_20$beta2)*log(1+(re_indmarket_20$re_market))
    IndRe<-(re_indmarket_20$beta1)*res2a
    IdiosRe<-log(1+(as.numeric(re_indmarket_20$prof)))-MktRe-IndRe
    #logMktRe<-log(1+MktRe)
    #logIndRe<-log(1+IndRe)
    #logIdiosRe<-log(1+IdiosRe)
    #logprof<-log(1+as.numeric(re_indmarket_20$prof))
    re_indmarket_20<-cbind(re_indmarket_20,MktReturn=MktRe,IndReturn=IndRe,IdiosReturn=IdiosRe)
    re_indmarket_20<-re_indmarket_20 %>% filter(beta1>-3 & beta1<3 & beta2>-3 & beta2<3)
    # write.csv(re_indmarket_20,'first window for the first 20 months.csv')
    return(re_indmarket_20)
  }
  else
  {
    re_indmarket_21<-dt %>% filter(!(ts==start_times_true[mon]))
    re_indmarket_21<-re_indmarket_21[order(re_indmarket_21$asset_id),]
    
    for(i in 1:(nrow(dt)/21))
    {
      y<-log(1+re_indmarket_21$re_ind[(20*(i-1)+1) : (20*i)])
      x<-log(1+re_indmarket_21$re_market[(20*(i-1)+1) : (20*i)])
      if(i==1)
        res2a<-lm(y~x)$residuals
      else
        res2a<-append(res2a,lm(y~x)$residuals)
    }
    for(j in 1:(nrow(dt)/21))
    {
      y<-log(1+as.numeric(re_indmarket_21$prof[(20*(j-1)+1) : (20*j)]))
      x1<-res2a[(20*(j-1)+1) : (20*j)]
      x2<-log(1+re_indmarket_21$re_market[(20*(j-1)+1) : (20*j)])
      if(j==1)
      {
        beta1<-rep(lm(y~x1+x2)$coefficients[2],each = 20)
        beta2<-rep(lm(y~x1+x2)$coefficients[3],each = 20)
        #print(lm(y~x1+x2)$residuals)
        res_2b<-lm(y~x1+x2)$residuals
      }
      else
      {
        beta1<-append(beta1,rep(lm(y~x1+x2)$coefficients[2],each = 20))
        beta2<-append(beta2,rep(lm(y~x1+x2)$coefficients[3],each = 20))
        res_2b<-append(res_2b,lm(y~x1+x2)$residuals)
      }
    }
    beta<-data.frame(beta1=beta1,beta2=beta2,res2b=res_2b ) 
    
    re_indmarket_21<-cbind(re_indmarket_21,res2a,beta) # condition = 2 (both Regression 2a and Regression 2b)
    MktRe<-(re_indmarket_21$beta2)*log(1+(re_indmarket_21$re_market))
    IndRe<-(re_indmarket_21$beta1)*res2a
    IdiosRe<-log(1+(as.numeric(re_indmarket_21$prof)))-MktRe-IndRe
    #logMktRe<-log(1+MktRe)
    #logIndRe<-log(1+IndRe) #
    #logIdiosRe<-log(1+IdiosRe) 
    #logprof<-log(1+as.numeric(re_indmarket_21$prof))
    re_indmarket_21<-cbind(re_indmarket_21,MktReturn=MktRe,IndReturn=IndRe,IdiosReturn=IdiosRe)
    re_indmarket_21<-re_indmarket_21 %>% filter(beta1>-3 & beta1<3 & beta2>-3 & beta2<3)   # 导致最后剩下的数据不一致
    # write.csv(re_indmarket_21,'first window for the last 20 months.csv')
    return(re_indmarket_21)
  }
}

# usage
# re<-Data_Process(data,5,1)
# re_20<-Regression(re,1)
# re_21<-Regression(re,2)
# Function -- 从完整数据中筛选每一窗口最后一个月的数据，写成数据框的形式

# ------- regression 5  ------- #
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

# ？？？？更改待续
# 暂时还没有运行；以文件（processing_by_month.R中的函数为准）
# usage: classification = 'zz','sy','zj' , digits = 2,4,6,8 ; 2,4,6 ; 1,3
Regression5 <- function(classsification,digits)   # 尝试用switch函数解决问题--
{
  if(classification == 'sy')
  {
    if(digits == 6)
    {
      f <- read.csv('6 digits for first 20 month.csv') # 完整数据
      l <- read.csv('6 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else if (digits == 4)
    {
      f <- read.csv('4 digits for first 20 month.csv') # 完整数据
      l <- read.csv('4 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else
    {
      f <- read.csv('2 digits for first 20 month.csv') # 完整数据
      l <- read.csv('2 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    print('data load success !')
  }
  else if(classification == 'zz')
  {
    if(digits == 8)
    {
      f <- read.csv('zz 8 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zz 8 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else if(digits == 6)
    {
      f <- read.csv('zz 6 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zz 6 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else if (digits == 4)
    {
      f <- read.csv('zz 4 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zz 4 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else
    {
      f <- read.csv('zz 2 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zz 2 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    print('data load success !')
  }
  else if(classification == 'zj')
  {
    if(digits == 3)
    {
      f <- read.csv('zj 3 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zj 3 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
    else if(digits == 1)
    {
      f <- read.csv('zj 1 digits for first 20 month.csv') # 完整数据
      l <- read.csv('zj 1 digits for last 20 month.csv') # 完整数据
      firstdt <- select_everywind(f)  # 2014.01 --- 2018.12
      lastdt <- select_everywind(l)  
    }
  }
  # 数据处理
  # step1 对数据进行处理（dig6.first.sy.reg 和 dig6.last.sy.reg 不同（由于beta删除后数据出现误差））
  a <- split(firstdt,firstdt$ts)
  b <- split(lastdt,lastdt$ts)
  for(i in 1:60)
  {
    sid <- intersect(a[[i]]$asset_id,b[[i]]$asset_id)
    a[[i]] = filter(a[[i]],a[[i]]$asset_id %in% sid)
    b[[i]] = filter(b[[i]],b[[i]]$asset_id %in% sid)
  }
  
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


## --------------------------------- 采用滚动窗口计算 ---------------------------------- ##

#（每一个大窗口20个月的数据，其中以10个月为起端开始计算每个小窗口的参数，取平均值）
# 2a,2b & 3a,3b,3c 要重新算
# 直接调用函数
Regression5_mean <- function(dataset,num,mon)
  # classification = 'P0211' -- 申银万国行业分类  num选择 5--六位；7--四位 ； 8--两位
  #                  'P0212' -- 中信证券行业分类  num选择 5--六位；7--四位 ； 8--两位, 从 2015-08-01 开始
  #                  'P0206' -- 中证行业分类      num选择 5 --八位 ； 7--六位 ； 8--四位 ； 9--两位
  #                  'P0207' -- 证监会行业分类    num选择 5 --三位 ；7-- 一位
{
  # data_new = Data_P(classification = 'P0211') # 调用函数
  # Regression 
  #for(k in 1:60)
  #{
  #  cat('k=',k,'\n')
    # step 1 : 得到第k个大窗口对应的数据
    resultProcess <- Data_Process(dt = dataset,num = num,mon)
    re<-resultProcess$re_indmarket # 得到一个大窗口(总共21个月）对应的处理后的data.frame
    #result = list()
    # step 2: 对一个大窗口进行分割，得到小窗口，并且分别回归
    # 最后一个月作为out-of-sample
    # Res2a <- c() ; Beta1 <- c() ; Beta2 <- c()
    # MktRe <- c() ; IndRe <- c() ; IdiosRe <- c()
    result <- list()
    for(i in 1:10)
    {
      #print(i)
      # 共有10个小窗口，窗口长度逐渐增加
      # mon表示第mon个大窗口，i表示第i个小窗口
      re23 <- re %>% filter(ts >=full_times[mon] & ts <= full_times[mon+8+i]) # 窗口逐渐拉长
      re23 <- re23[order(re23$asset_id),]
      #for(i in 1:870)
      #{
      #  if(nrow(a[[i]]) < 16)
      #    print(i)
      #}
      #df <- split(re23,re23$asset_id)
      # regression  ,firm-specific
      l = nrow(re23)/(9+i)
      
      for(j in 1:l)
        # 对股票进行循环
      {
         #print(j)
        # 第j个股票
        #a = re23[(10*j-9):(10*j),]
        a = re23[((9+i)*(j-1)+1):((9+i)*(j-1)+9+i),]
        
        # 防止后续删除logprof和连续21个月的数据导致该行业只有一只股票，造成计算时的数据缺失
        if(NaN %in% a$re_ind )
          a$re_ind = as.numeric(a$prof)
        
        # regression 2a
        y = log(1+a$re_ind)
        x = log(1+a$re_market)
        res2a = lm(y~x)$residuals
        #r.2a = summary(lm(y~x))$r.square
        #Res2a<-append(Res2a,res2a) # 得到第i个小窗口对应的所有股票的res2a
        
        # regression 2b
        y1 = a$logprof
        x1 = res2a
        x2 = log(1+a$re_market)
        beta1<-rep(lm(y1~x1+x2)$coefficients[2],each = (9+i))
        beta2<-rep(lm(y1~x1+x2)$coefficients[3],each = (9+i))
        #r.2b = summary(lm(y1~x1+x2))$r.square
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
        #output = cbind(a,output)
        if(j == 1)
          out = output
        else
          out = rbind(out,output) # 得到第i个小窗口的结果
      }
      result[[i]] <- cbind(re23,out) # 第 i 个小窗口对应的回归结果
      #result = data.frame(Res2a,Beta1,Beta2,MktRe,IndRe,IdiosRe)
      # result = cbind(a,result)
      # 得到第i（q）个小窗口对应的回归结果
      #result[[i]] <- out
      # result[[i]] <- result[[i]][order(result[[i]]$ts),]
    }
    
    outdata = result[[10]]
    
    # regression 5abcde
    for(q in 1:9)
    {
      #cat('q =:',q)
      # Regression 5a
      y = result[[q+1]] %>% filter(ts > full_times[mon])
      y = y$logprof
      x = result[[q]]$logprof
      alpha0_a = lm(y~x)$coefficients[1]
      alpha1_a = lm(y~x)$coefficients[2]
      r.5a = summary(lm(y~x))$r.square
      
      # Regression 5b
      y = result[[q+1]] %>% filter(ts > full_times[mon])
      y = y$logprof
      x1 = result[[q]]$MktRe
      x2 = result[[q]]$IndRe
      x3 = result[[q]]$IdiosRe
      alpha0_b = lm(y~x1+x2+x3)$coefficients[1]
      alpha1_b = lm(y~x1+x2+x3)$coefficients[2]
      alpha2_b = lm(y~x1+x2+x3)$coefficients[3]
      alpha3_b = lm(y~x1+x2+x3)$coefficients[4]
      r.5b = summary(lm(y~x1+x2+x3))$r.square
      
      # Regresion 5c
      y = result[[q+1]] %>% filter(ts > full_times[mon])
      y = y$MktRe
      x = result[[q]]$MktRe
      alpha0_c = lm(y~x)$coefficients[1]
      alpha1_c = lm(y~x)$coefficients[2]
      r.5c = summary(lm(y~x))$r.square
      
      # Regeression 5d
      y = result[[q+1]] %>% filter(ts > full_times[mon])
      y = y$IndRe
      x = result[[q]]$IndRe
      alpha0_d = lm(y~x)$coefficients[1]
      alpha1_d = lm(y~x)$coefficients[2]
      r.5d = summary(lm(y~x))$r.square
      
      # Regeression 5e
      y = result[[q+1]] %>% filter(ts > full_times[mon])
      y = y$IdiosRe
      x = result[[q]]$IdiosRe
      alpha0_e = lm(y~x)$coefficients[1]
      alpha1_e = lm(y~x)$coefficients[2]
      r.5e = summary(lm(y~x))$r.square
      
      out = data.frame(quater = q,
                       alpha0_a,alpha1_a,
                       alpha0_b,alpha1_b,alpha2_b,alpha3_b,
                       alpha0_c,alpha1_c,
                       alpha0_d,alpha1_d,
                       alpha0_e,alpha1_e,
                       r.5a,r.5b,r.5c,r.5d,r.5e)
      if(q == 1)
        out5 = out
      else
        out5 = rbind(out5,out) # 得到的是第mon个月的所有回归数据
      # 计算均值,一个大窗口对应的小窗口
      #out5.m = colMeans(out5)
    }
    out5 = colMeans(out5) # 第mon个月的数据求平均
    #print('out5')
    
    #print('out5')
    #if(mon == 1)
    if(mon == 39) # zx
      #reg5.m = cbind(k,out5.m)
      reg5 = cbind(mon,out5)
    else
      #reg5.m = rbind(reg5.m,cbind(k,out5.m))
      reg5 = rbind(reg5,cbind(mon,out5))
    
    last = list(reg = reg5
                #,data = re
                ,data = outdata)
    return(last)
}





#-----------------------------将其写成包的形式---------------------------------#
package.skeleton("secondp",list = c("Delete_missing21","calculate_lessthan5","Calculate_industryReturn","Calculate_marketReturn","Data_Process"),
                 path = "E:/ProgramData/MyRPacka")



