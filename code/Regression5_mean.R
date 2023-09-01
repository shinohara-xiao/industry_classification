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
      # print(j)
      # 第j个股票
      #a = re23[(10*j-9):(10*j),]
      a = re23[((9+i)*(j-1)+1):((9+i)*(j-1)+9+i),]
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
  
  # regression 5abcde
  for(q in 1:9)
  {
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
    out5 = colMeans(out5) # 第mon个月的数据求平均
  }
  
  if(mon == 1)
  {
    #reg5.m = cbind(k,out5.m)
    reg5 = cbind(mon,out5)
  }
  
  else 
  {
    #reg5.m = rbind(reg5.m,cbind(k,out5.m))
    reg5 = rbind(reg5,cbind(mon,out5))
  }
  
  last = list(reg = reg5,
              data = re)
  return(last)
}



