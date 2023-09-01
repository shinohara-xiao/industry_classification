setwd('/Users/shinohara_xiao/Documents/asset regression')
install.packages('dplyr')
install.packages('stringr')
library(dplyr)
library(stringr)

data<-read.csv('resample.csv',encoding='UTF-8'). # data = resample
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

data<-merge(class,re,by=c('ts','asset_id')) # 得到整体数据集  包括所有的时间

# 计算2014-2018有多少个股票 No.stocks
data_1<-data[which(data$ts<'2019' & data$ts>'2014'),] # 为了计算2014-2018年包含的股票数

AID<-data_1$asset_id
AID<-sort(AID)
index<-duplicated(AID)
AID<-AID[!index]

# 计算初始observations 和 num of stocks
length(AID)
nrow(data_1)

#------------------------------------------------------------------------------------------------#
#  公司i所在的行业j在第q月的return（industry-level）（行业数*行业内的股票个数）---y变量
#  除去公司i以外，股票池中所有股票在第q月的return（股票数*1）---x变量

# 计算除公司i以外股票池中所有股票return
#------------------------------------————————————————————————————------------------------#

#  data      是申银万国行业分类下包括所有的数据集
#  class     是申银万国分类‘P0211’下的所有数据
#  data_1    是2014-2018年之间所有的完整数据集  （用来统计14-18年间的所有股票数,是dt_1的基础）
#  dt_1      是2014-2018年之间 删除所有重复asset_id 之后的数据集（用来计算少于5个的行业）

#——————————————————————————————————————————————————————————————————————————————————————————# 

# 剔除行业分类中包含小于5个公司的行业并且剔除intersect后缺少的股票ID

# 连续21个月内有缺失值的数据 2014年02月-2018年12月 (2012年4月-2019年1月) 

# industry_id,class_num=5   ind_4  calss_num=7,    ind2  class_num=8


#2021.9.26 会议
#-------------------------------------------------------------------------------------------------------------#

# Step1： 从observation的角度来看，假设每个月有y个行业，一个行业有x个公司，总共有x*y个observation
# Step2: 预处理：剔除小于等于5的行业，y1代表每个月<=5个公司的行业，所以总共是sum（y1），公司数应该取交集
# Step3：y-y1-y2，为什么会报错？？？？


# 在第一步和第二步之后，应该优先计算行业平均，计算ROA,踢掉ROA
# 每个月股票的收益，每个月行业，每个月MArket的收益，删掉缺失值，剔除标准：连续21个月的数据
# 再进行一次筛除，剔除后再进行回归（剔除indusrty，相应的剔除beta）
# 每个月有两个集合（行业，firms）；第一步剃掉industry，第二步剃掉firms，第三步再踢掉没有连续的行业和firms
# 剩下的股票和行业再进行回归

# 文章可能太过于关注firms，但是没有观察到industry
# 所以，行业的影响关系应该是非常大的

# 反向证明！！！！：
#           如果行业合成的时候存在问题，必定的假设：第二步如果是正确的，第三步踢掉的行业的股票应该都被踢掉,
#           所以第三步还有股票剩余的话，第二步处理有问题
# Table_1 添加一行

# 第三步应该踢不掉任何observations和firms。

# 10月7号，把2a，2b结果整出来。
# 发消息的时候给个例子

#-------------------------------------------------------------------------------------------------------------#
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
start_times<-c('2012-06','2012-07','2012-08','2012-09','2012-10','2012-11','2012-12','2013-01',
               '2013-02','2013-03','2013-04','2013-05','2013-06','2013-07','2013-08','2013-09',
               '2013-10','2013-11','2013-12','2014-01','2014-02','2014-03','2014-04','2014-05',
               '2014-06','2014-07','2014-08','2014-09','2014-10','2014-11','2014-12','2015-01',
               '2015-02','2015-03','2015-04','2015-05','2015-06','2015-07','2015-08','2015-09',
               '2015-10','2015-11','2015-12','2016-01','2016-02','2016-03','2016-04','2016-05',
               '2016-06','2016-07','2016-08','2016-09','2016-10','2016-11','2016-12','2017-01',
               '2017-02','2017-03','2017-04','2017-05')
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
              '2018-11-01','2018-12-01')
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



# ------------------------------------------------------------------------------------#

# 剔除行业中少于5个公司的行业
# 数据只要以股票为主，group_by 的话会比较奇怪，可以用count确认一下有没有下于5的行业出现
# 其他的方法
# data %>% count_dt()
# tidyfst 包
# data %>% group_by(date, industry_id) %>% filter(n() >= 5)



#---------------------------------------------------------------------------------------------#
# Calculate less tha 5 and get the dataset data_del
#------Function-----#
M<-data

Delete_lessthan5<-function(dtset,class_num)
{
   for(i in 1:length(full_times))     # true 改成full_times
   {
     print(full_times[i])
     test_dt<-filter(dtset,dtset$ts==full_times[i]) # 选取该时间下的数据子集
     S<-split(test_dt,test_dt[,class_num])
     for(j in 1:length(S))  # 计算该时间full_times[i]下有多少个行业
     {
       if(nrow(S[[j]])<5)
         M<-filter(M,!(M[,class_num] %in% S[[j]][,class_num]))
       else
         M<-M
     }
   }
  return(M)
}

#----Application------#
data_del<-Delete_lessthan5(data,5)    # group_by 函数，count()函数

#data_del_1<-data_del %>% filter(ts<'2019' &ts>'2014')

#data_del_four<-Delete_lessthan5(data,7)
#data_del_four_1<-data_del_four %>% filter(ts<'2019' &ts>'2014')

#data_del_two<-Delete_lessthan5(data,8)
#data_del_two_1<-data_del_two %>% filter(ts<'2019' &ts>'2014')


#---------------------计算丢失的个数(initial-less than 5)------------------------------#

# industry_id,class_num=5       
obs<-nrow(data_1)-nrow(data_del_1)
a1<-split(data_1,data_1$asset_id)
a2<-split(data_del_1,data_del_1$asset_id)
company<-length(a1)-length(a2)
cat('missing observations:',obs,'missing company:',company)

# ind_4  calss_num=7
obs<-nrow(data_1)-nrow(data_del_four_1)
a1<-split(data_1,data_1$asset_id)
a2<-split(data_del_four_1,data_del_four_1$asset_id)
company<-length(a1)-length(a2)
cat('missing observations:',obs,'missing company:',company)

# ind2  class_num=8
obs<-nrow(data_1)-nrow(data_del_two_1)
b1<-split(data_1,data_1$asset_id)
b2<-split(data_del_two_1,data_del_two_1$asset_id)
company<-length(b1)-length(b2)
cat('missing observations:',obs,'missing company:',company)



# data_del %>% count_dt(data)


# 在这一步中可以选择先不要运行这一步
# data_1_del<-data_del[which(data_del$ts<'2019' & data_del$ts>'2012-06'),] 

# nrow(data_1)-nrow(data_1_del)




#-------------------------------------------------------------------------------------------------------------#
# 删除没有连续21个月的数据的observations

# 此时M可以作为data_1_del


# 删除在end_time 和 start_time 之间的数据少于21个月的所有股票以及observations
#----Function------#
Delete_missing<-function(dtsets)
{
  D<-dtsets
  for(i in 1:length(end_times))
  {
    dt<-dtsets[which((dtsets$ts<end_times[i])&(dtsets$ts>start_times[i])),]
    dt_div<-split(dt,dt$asset_id)
    print(end_times[i])
    for(j in 1:length(dt_div))
    {
      l<-nrow(dt_div[[j]])
      if(l<21)
        D<-filter(D,!(D$asset_id %in% dt_div[[j]]$asset_id))
    }
  }
  return(D) 
}


#---Application------#
data_delete<-Delete_missing(data_del)


#---------------calculate intersect stocks missing----------------------------#

#-------Function---------#
Calculate_missing<-function(dtsets,end_time,start_time){
  
  test<-dtsets[which(dtsets$ts<end_time & dtsets$ts>start_time),]
  ast_div<-split(test,test$asset_id)
  j<-0
  stock<-c()
  for(i in 1:length(ast_div))
  {
    l<-nrow(ast_div[[i]])
    if(l<21)
    {
      stock[j+1]<-ast_div[[i]]$asset_id[1]
      j=j+1
    }
  }
  return(stock)
}

#-------- Application---------#
for(k in 1:length(end_times))
{
  print(k)
  stocks<-Calculate_missing(data_del,end_times[k],start_times[k])      #某一段时间内所有缺失股票
  if(k==1)
    StockID<-stocks
  else
    StockID<-intersect(StockID,stocks)
}
#---------------------------------------------------------------------------------------------------#

#  重新进行筛选
# 在 data_delete 的基础上，删除行业
# test<-data_del[which(ts>'2012-06' & ts<'2019'),]
# missing_observations<-nrow(data_del)-nrow(data_delete)
# data_1_delete<-data_delete[which(data_delete$ts>'2014' & data_delete$ts<'2019'),]
# missing_frims<-length(StockID)
# 得到的data_1_delete ,data_delete 都是经过删除处理后的数据
# --------------------------------------------------------------------------------------------#
#计算Re_Ind
# Re_Ind[[i]]是第i个行业，20120.2-2018.12月之间的industry_prof的情况


# 尝试使用append函数来计算Re_Ind
# 每一列都是一个行业的2012.06-2018.12之间的平均收益，对应的列为MI[[i]]多对应的prof
# 计算Re_Ind 函数
# indusrty_id/ind_num=5; ind_4/ind_num=7; ind_2/ind_num=8
# 选取data_delete 

# 使用函数，append()函数进行连接，暂时不使用
# -----------------------------------------------------------------------------------#
# ind_profit<-list()
# Calculate_industry_profit<-function(dataset,ind_num){
  
#  MI<-split(data_delete,data_delete[,ind_num])
#  for(i in 1:length(MI))
#  {
#    for(j in 1:length(full_times))
#    {
#      test<-filter(MI[[i]],MI[[i]]$ts==full_times[j])
#      mean_ind<-mean(as.numeric(test$prof.x))
#      if(j==1)
#        ind_return<-mean_ind
#      else
#        ind_return<-append(ind_return,mean_ind,after=length(ind_return))
#    }
#   ind_profit[[i]]<-ind_return
#  }
# }

# industry_return<-Calculate_industry_profit(data_delete,5)
 
#----------------------------------计算Re_Ind----------------------------------------------------#

#data_delete_s<-data_delete[which(data_delete$ts>'2012-06' & data_delete$ts<'2019'),]
#data_del_s<-data_del[which(data_del$ts>'2012-06' & data_del$ts<'2019'),]

# 使用函数计算
# indusrty_id/ind_num=5; ind_4/ind_num=7; ind_2/ind_num=8

#-------------------function------------------#
  Calculate_return_ind<-function(dtsets,num)
  {
  MI<-split(dtsets,dtsets[,num])
  Re_Ind<-list()
  for(i in 1:length(MI))
  {
    print(i)
    industry_prof<-c()
    ts<-c()
    
    for(j in 1:length(full_times))
    {
      industry_id<-rep(MI[[i]][num][1,],each=length(full_times))
      ts[j]<-full_times[j]
      industry_prof[j]<-mean(as.numeric(filter(MI[[i]],MI[[i]]$ts==full_times[j])$prof))
    }
    test<-data.frame(industry_id,ts,industry_prof)
    Re_Ind[[i]]<-test 
  }
    return(Re_Ind)
  }
  
#-------------application----------------------#
 Re_Ind<-Calculate_return_ind(data_delete_MSA,5)

 # 计算是否有缺失值    by now,there are 11 industries have the NAN,(ex:12th,210402)
for(i in 1:length(Re_Ind))
{
  su<-sum(is.na(Re_Ind[[i]][3]))
  print(su)
}

###########################################################################
# 有缺失值，删除不完整的行业，进行再一步的处理
 data_delete_M<-data_delete %>% filter(!(industry_id=='630401'|industry_id=='640103'|industry_id=='630203'
                                         |industry_id=='630101'|industry_id=='450302'|industry_id=='420801'))

 
# data_delete_MS<-data_delete_M[which(data_delete_M$ts>'2012-06' & data_delete_M$ts<'2019'),]
 
# Re_Ind<-Calculate_return_ind(data_delete_MSa,5) # 最终计算得出的Re_Ind()
 
# for(i in 1:length(Re_Ind))
# {
#   sum<-sum(is.na(Re_Ind[[i]][3]))
#   print(sum)
# }
 
 #############
 # calculate the missing records
 data_delete_1<-data_delete %>% filter(ts>'2014' & ts<'2019')
 obs<-nrow(data_del_1)-nrow(data_delete_1)
 a1<-split(data_del_1,data_del_1$asset_id)
 a2<-split(data_delete_1,data_delete_1$asset_id)
 company<-length(a1)-length(a2)
 cat('missing obs:',obs,'missing com:',company)
 


 
#####################################################################################################################
 #-----------------------------------------------其他分类下数据-----------------------------------------------------#
 # 计算删除后的数据缺失
 
 #-----------------------------------------------------------------------------------------------------#
 # --------------------------------------------------------------# 四分类下的计算
 # 计算缺失值
 data_delete_four<-Delete_missing(data_del_four)
 data_delete_four_1<-data_delete_four %>% filter(ts<'2019' & ts>'2014')
 
 for(i in 1:length(Re_four_Ind))
 {
   su<-sum(is.na(Re_four_Ind[[i]][3]))
   print(sum)
 }
 
 obs<-nrow(data_del_four_1)-nrow(data_delete_four_1)
 a1<-split(data_del_four_1,data_del_four_1$asset_id)
 a2<-split(data_delete_four_1,data_delete_four_1$asset_id)
 company<-length(a1)-length(a2)
 cat('missing obs:',obs,'missing com:',company)
 
 # calculate return_ind & Re_market
 data_delete_four_s<-data_delete_four[which(data_delete_four$ts>'2012-06' & data_delete_four$ts<'2019'),]
 Re_four_Ind<-Calculate_return_ind(data_delete_four_s,7)
 Re_Market<-c()
 for(i in 1:length(full_times))
 {
   pro<-(filter(data_delete_four_s,data_delete_four_s$ts==full_times[i]))$prof.x
   Re_Market[i]<-mean(as.numeric(pro))
 }
 sum(is.na(Re_Market))
 length(Re_Market)  
 
 nrow(data_delete_four_1)
 
 ########################################
 # -------------------------------------------#二分类下的计算
 data_delete_two<-Delete_missing(data_del_two)
 data_delete_two_1<-data_delete_two %>% filter(ts<'2019' & ts>'2014')
 
 data_delete_two_s<-data_delete_two[which(data_delete_two$ts>'2012-06' & data_delete_two$ts<'2019'),]
 Re_two_Ind<-Calculate_return_ind(data_delete_two_s,7)
 for(i in 1:length(Re_two_Ind))
 {
   su<-sum(is.na(Re_two_Ind[[i]][3]))
   print(sum)
 }
 
 obs<-nrow(data_del_two_1)-nrow(data_delete_two_1)
 a1<-split(data_del_two_1,data_del_two_1$asset_id)
 a2<-split(data_delete_two_1,data_delete_two_1$asset_id)
 company<-length(a1)-length(a2)
 cat('missing obs:',obs,'missing com:',company)
 
 # calculate return_ind & Re_market
 
 Re_Market<-c()
 for(i in 1:length(full_times))
 {
   pro<-(filter(data_delete_four_s,data_delete_four_s$ts==full_times[i]))$prof.x
   Re_Market[i]<-mean(as.numeric(pro))
 }
 sum(is.na(Re_Market))
 length(Re_Market)  
 
 
#---------------------------------------------(2a) Regression------------------------------------------------------#
# 
 data_delete_Ma<-data_delete_M %>% filter(!(industry_id=='420601'))
 data_delete_MSa<-data_delete_MS %>% filter(!(industry_id=='420601'))
 
 # 检查是否每一只股票都有连续21个月的数据

 # 删除没有连续数据
 tests<-split(data_delete_MSa,data_delete_MSa$asset_id)
 for(i in 1:length(tests))
 {
   lid<-nrow(tests[[i]])
   if(lid<79)
     print(i)
 }
 testnum<-c('78','111','142','214','276','326','351','357','362','436',
            '479','509','579','584','589','603','623')
 tests[[78]]$asset_id[1]   # 759
 tests[[111]]$asset_id[1]  # 922
 tests[[142]]$asset_id[1]  # 2058
 tests[[214]]$asset_id[1]  # 2352
 tests[[276]]$asset_id[1]  # 2595
 tests[[326]]$asset_id[1]  # 300286
 tests[[351]]$asset_id[1] # 600057
 tests[[357]]$asset_id[1]
 tests[[362]]$asset_id[1]
 tests[[436]]$asset_id[1]
 tests[[479]]$asset_id[1]
 tests[[509]]$asset_id[1]
 tests[[579]]$asset_id[1]
 tests[[584]]$asset_id[1]
 tests[[589]]$asset_id[1] 
 tests[[603]]$asset_id[1] 
 tests[[623]]$asset_id[1]
 
 delstockid<-c('759','922','2058','2352','2595','300286','600057','600077','600089','600361','600550',
               '600692','601116','601179','601222','601567','601933')
 data_delete_MSA<-data_delete_MSa %>% filter(!(asset_id %in% delstockid ))
 
 # 删除行业前后发生变化的
C<-c(0)
a<-split(data_delete_MSA,data_delete_MSA$industry_id)
for(i in 1:length(a))
{
  b<-split(a[[i]],a[[i]]$asset_id)
  for(j in 1:length(b))
  {
    n<-nrow(b[[j]])
    if(n<79)
      C<-cbind(C,(b[[j]]$asset_id[1]))
  }
}

C<-sort(C)
index<-duplicated(C)
C_del<-C[!index]


data_delete_MSA2<-data_delete_MSA %>% filter(!(asset_id %in% as.character(C_del)))
 #---------------------------------重新计算--------------------------------#
 Re_Ind<-Calculate_return_ind(data_delete_MSA2,5)

 #-----------------------------------------------------------------------#
 # 计算Re_Market  实际计算的应该是2014年1月-2018年12月
 #                        往前推  2012年6月-2017年5月
 
 
 Re_Market<-c()
 for(i in 1:length(full_times))
 {
   pro<-(filter(data_delete_MSA2,data_delete_MSA2$ts==full_times[i]))$prof
   Re_Market[i]<-mean(as.numeric(pro))
 }
 
 sum(is.na(Re_Market))
 length(Re_Market)   # 从2012。06-2018.12月共79个月，所以总共79个月的市场平均收益
 
 #---------------开始回归-------------------#
 
# 每一次循环（i），就是对应行业i从2014.1-2018.12月共60个月的回归残差，长度1200
 
MS<-split(data_delete_MSA2,data_delete_MSA2$industry_id)
residual<-c()
res_data<-list()
for(i in 1:length(MS))
{
  print(i)
  for(j in 1:length(true_times))
  {
    # print(true_times[j])
    y<-Re_Ind[[i]][3][j:(j+19),]
    x<-Re_Market[j:(j+19)]
    lim<-lm(y~x)
    res<-lim$residuals
    if(j==1)
      residual<-res
    else
      residual<-append(residual,res)  # residual 是一个向量(1200个数据)
  }
   res_data[[i]]<-residual   #行业i在所有 quater q的回归残差
}

# ------------------------计算是否有缺失值
for(i in 1:length(res_data))
{
  TF<-all(is.na(res_data[[i]]))
  if(TF==TRUE)
    print(i)    # 没有缺失值
}

#-------------------------计算是否长度都为1200
for(i in 1:length(res_data))
{
  lres<-length(res_data[[i]])
  if(lres!=1200)
    print(i)      # 正常
}

for(i in 1:length(Re_Ind))
{
  ana<-all(is.na(Re_Ind[[i]][3]))
  if(ana==TRUE)
    print(i)   #正常
}


#Re_Ind[[74]]  # 最后有6个缺失值（2018-07，-08，-09，-10，-11，-12）
# industry_prof[j]<-mean(as.numeric((filter(MI[[i]],MI[[i]]$ts==full_times[j]))$prof.x))
#nrow(MI[[74]])  #行业最后有6个缺失值（2018-07，-08，-09，-10，-11，-12）
#sss<-split(MI[[74]],MI[[74]]$asset_id)
#lsss<-length(sss)       # 只包含一个股票，删除该行业和该股票
# 删除该行业


#------------------------------------------(2b)regression-----------------------------------------------#
# 变量命名回顾：
# data_delete_M<-data_delete %>% filter(!(industry_id=='630401'|industry_id=='640103'|industry_id=='630203'
#                                        |industry_id=='630101'|industry_id=='450302'|industry_id=='420801'))


# data_delete_MS<-data_delete_M[which(data_delete_M$ts>'2012-06' & data_delete_M$ts<'2019'),]

#----对应的行业i为MS[[i]][5][1]----#
#---修改待定----#

#MS<-split(data_delete_MSA2,data_delete_MSA2$industry_id)
beta<-list()

for(i in 1:length(MS))
   {
     cat('i=',i)
     stock_ID<-split(MS[[i]],MS[[i]]$asset_id)  # 获取行业i中所有的股票ID
     
     for(l in 1:length(stock_ID))          # 求取所有股票所有区间q的return，先求第一个股票所有区间，再对股票进行遍历循环
    {
       for(j in 1:60)
        {
          g<-data_delete_MSA %>% filter(asset_id %in% stock_ID[[l]][2][1,]) %>% filter(ts>start_times[j] & ts<end_times_2[j])
         # 筛选data_delte_MS中，股票id=行业i中第l个股票，时间在qj的所有收益
          y<-as.numeric(g$prof)
          q1<-20*(j-1)+1
          q2<-20*(j-1)+20
          x1<-res_data[[i]][q1:q2] # length(res_data[[i]]=1200=60*20)
          x2<-Re_Market[j:(j+19)]
          lim<-lm(y~x1+x2)
          if(j==1)
             b<-lim$coefficients[c(2,3)]
          else
             b<-rbind(b,lim$coefficients[c(2,3)])  
         }  # 该循环结束，计算得到的b是行业i的股票l在所有周期（60个）得到的beta
            assetid<-rep(stock_ID[[l]][2][1,],each=60)
            betaa<-data.frame(stockid=assetid,beta1=b[,1],beta2=b[,2])
       # 将行业i对应的股票l在所有区间的beta存储起来
       # 第一行对应的就是第一个q内回归得到的beta
       # betaa 是 行业i的股票l在所有周期（60个）得到的beta 及其对应的asset_id名称
     if(l==1)
      bet<-betaa
     else
      bet<-rbind(bet,betaa)  # 得到是行业i内所有股票（l取所有遍历）在所有周期内对应的beta值
   }
      beta[[i]]<-bet # 行业i内所有股票
 }

# 判断股票选取是否正确，是否存在缺失和遗漏
for(i in 1:length(beta))
{
  if((nrow(beta[[i]])/60)!=nrow(MS[[i]])/79)
    print(i)
}

# 输出最后结果数据
write.csv(data_delete_MSA,'data_delete_MSA2.csv')
write.csv(Re_Ind,'Re_Ind.csv') # 列表输出结果可能不对

# 输出res_data
residuals<-c()
for(i in 1:length(res_data))
{
  residuals<-append(residuals,res_data[[i]])
}
