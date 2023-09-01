#-------------------------------calculate intersect stocks missing----------------------------#
data_del
data_del_four
data_del_two
#-------------------------------------------#
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

for(k in 1:length(end_times))
{
  print(k)
  stocks<-Calculate_missing(data_del,end_times[k],start_times[k])              # 某一段时间内所有缺失股票
  if(k==1)
    StockID<-stocks
  else
    StockID<-intersect(StockID,stocks)
}



#-------------------------------------------数据定义汇总--------------------------------------------#

#data_del<-Delete_lessthan5(data,5)    # group_by 函数，count()函数
#data_del_1<-data_del %>% filter(ts<'2019' &ts>'2014')

#data_del_four<-Delete_lessthan5(data,7)
#data_del_four_1<-data_del_four %>% filter(ts<'2019' &ts>'2014')

#data_del_two<-Delete_lessthan5(data,8)
#data_del_two_1<-data_del_two %>% filter(ts<'2019' &ts>'2014')

data_delete<-Delete_missing(data_del)

#data_delete_s<-data_delete[which(data_delete$ts>'2012-06' & data_delete$ts<'2019'),]
#data_del_s<-data_del[which(data_del$ts>'2012-06' & data_del$ts<'2019'),]

data_delete_M<-data_delete %>% filter(!(industry_id=='630401'|industry_id=='640103'|industry_id=='630203'
                                        |industry_id=='630101'|industry_id=='450302'|industry_id=='420801'))

data_delete_Ma<-data_delete_M %>% filter(!(industry_id=='420601'))

data_delete_MS<-data_delete_M[which(data_delete_M$ts>'2012-06' & data_delete_M$ts<'2019'),]

data_delete_MSa<-data_delete_MS %>% filter(!(industry_id=='420601'))

                                         #data_delete_four<-Delete_missing(data_del_four)
#data_delete_four_1<-data_delete_four %>% filter(ts<'2019' & ts>'2014')


#-------------------------------最后使用的数据--------------------------------------------#
delstockid<-c('759','922','2058','2352','860','2595','300286','600057','600077','600089','600361','600550',
              '600692','601116','601179','601222','601567','601933')
data_delete_MSA<-data_delete_MSa %>% filter(!(asset_id %in% delstockid ))

data_delete_MSA  # from  2012-06 to  2018-12

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


data_delete_MSA2<-data_delete_MSA %>% filter(!(asset_id %in% as.character(C_del))) # The last used

