library(dplyr)
library(stringr)
library(reshape)

# ------------------------------ 读取数据 -------------------------------- #
sy6 <- read.csv('sy6.data-change.csv')
sy4 <- read.csv('sy4.data-change.csv')
sy2 <- read.csv('sy2.data-change.csv')

zz8 <- read.csv('zz8.data-change.csv')
zz6 <- read.csv('zz6.data-change.csv')
zz4 <- read.csv('zz4.data-change.csv')
zz2 <- read.csv('zz2.data-change.csv')

zx6 <- read.csv('zx6.data-change.csv')
zx4 <- read.csv('zx4.data-change.csv')
zx2 <- read.csv('zx2.data-change.csv')

zj3 <- read.csv('zj3.data-change.csv')
zj1 <- read.csv('zj1.data-change.csv')

# definition of the function
# 删除部分由于计算原因出现的重复数据
complicate <- function(dataset)
{
  D <-split(dataset,dataset$asset_id)
  for(i in 1:length(D))
  {
    test <- split(D[[i]],D[[i]]$ts)
    dat <- test[[1]]
    for(j in 2:length(test))
    {
      dat <- rbind(dat,test[[j]][1,])
    }
    if(i ==1)
      data = dat
    else
      data <-rbind(data,dat)
  }
  return(data)
}

sy6_d <- complicate(sy6) ; sy4_d <- complicate(sy4) ; sy2_d <- complicate(sy2)
zx6_d <- complicate(zx6) ; zx4_d <- complicate(zx4) ; zx2_d <- complicate(zx2)
zz8_d <- complicate(zz8) ; zz6_d <- complicate(zz6) ; zz4_d <- complicate(zz4) ; zz2_d <- complicate(zz2)
zj3_d <- complicate(zj3) ; zj1_d <- complicate(zj1) 



# ------------- equal 函数 ------------- #
# 回归保持数据长度一致
equal <- function(dataset1,dataset2)
{
  d1 <- split(dataset1,dataset1$asset_id)
  d2 <- split(dataset2,dataset2$asset_id)
  id1 <- c() ; id2 <-c()
  for(i in 1:length(d1))
  {
    if(nrow(d1[[i]]) < 20)
      id1 <- append(id1,d1[[i]]$asset_id[1])
  }
  for(i in 1:length(d2))
  {
    if(nrow(d2[[i]]) < 20)
      id2 <- append(id2,d2[[i]]$asset_id[1])
  }
  id <- union(id1,id2)
  #print(id)
  dataset1 <- filter(dataset1,!(dataset1$asset_id %in% id))
  dataset2 <- filter(dataset2,!(dataset2$asset_id %in% id))
  result <- list(set1 = dataset1,set2 = dataset2)
  return(result)
}

# -------------------------- 预测函数，计算beta系数 --------------------------- #

# ------------------------- 第一种预测 --------------------- # 
# TP use profit to predict profit
# every 20 months as a window,to predict next the 21 month data

# 计算系数
predict_calculate <- function(dataset)
{
  beta0 <-c() ; beta1 <-c()
  res <-c() ; r2 <-c()
  
  for(i in 1:58) # zx 下取39
  {
    # sy 只到2018年11月，78个月
    x <- dataset %>% filter(ts >= full_times[i] & ts <= full_times[i+19]) # 共19个月
    y <- dataset %>% filter(ts >= full_times[i+1] & ts <= full_times[i+20]) # 留出第21个月，用第20个月预测第21个月
    
    id1 <- x$asset_id ; id2 <- y$asset_id
    sid <- intersect(id1,id2)
    x <- x[which(x$asset_id %in% sid),] ; y <- y[which(y$asset_id %in% sid),]
    dat <- equal(x,y)
    dat1 <- dat$set1; dat2 <-dat$set2
    x = dat1$prof ; y = dat2$prof
    reg <- lm(y~x)
    
    beta0 <- append(beta0,reg$coefficients[1])
    beta1 <- append(beta1,reg$coefficients[2])
    #res <-append(res,reg$residuals)
    r2 <- append(r2,summary(reg)$r.square)
  }
  
  result <- data.frame(beta0,beta1,r2)
  return(result)
}

coef_sy6 <- predict_calculate(sy6_d) 
coef_sy4 <- predict_calculate(sy4_d) ; coef_sy2 <- predict_calculate(sy2_d)

# 时间不同
coef_zx6 <- predict_calculate(zx6_d) 
coef_zx4 <- predict_calculate(zx4_d) ; coef_zx2 <- predict_calculate(zx2_d)

coef_zz8 <-predict_calculate(zz8_d) ; coef_zz6 <- predict_calculate(zz6_d) 
coef_zz4 <- predict_calculate(zz4_d) ; coef_zz2 <- predict_calculate(zz2_d)

coef_zj3 <- predict_calculate(zj3_d) ; coef_zj1 <- predict_calculate(zj1_d) 

# --------- predict1 -------------- #
predict_func <- function(dataset1,dataset2) # dataset1 = data_new_classification ; dataset2 = coef_classx
{ # i = 21 + 58
  #profit <-c() ; predict <-c()
  
  error <-c() ; abserr <-c()
  
  for(i in 1:58)
  {
    dt <- dataset1 %>% filter(ts == full_times[i+20]) # 有很多股票
    true <- dataset1 %>% filter(ts == full_times[i+21])
    
    sid <- intersect(dt$asset_id,true$asset_id)
    dt <- dt[which(dt$asset_id %in% sid),]
    true <- true[which(true$asset_id %in% sid),]
    
    #profit <- append(profit,as.numeric(dt$prof))
    #predict <- append(predict,coef_sy6$beta0[i] + coef_sy6$beta[1]*profit)
    profit <- as.numeric(dt$prof) # 是一个向量
    predict <- dataset2$beta0[i] + dataset2$beta1[i]*profit
    true <- as.numeric(true$prof)
    
    error <- append(error, predict - true)
    abserr <- append(abserr,abs(predict - true))
  }
  
  result <- data.frame(error,abserr)
  return (result)
}

# --------------- summary ---------------------- #
# dataset1 = classx_d
## ------------------ sy
pre_sy6 <- predict_func(dataset1 = sy6_d
                        #data_new.sy
                        , dataset2 = coef_sy6)
#summary(pre_sy6$error) ; summary(pre_sy6$abserr)

pre_sy4 <- predict_func(dataset1 = sy4_d
                        #data_new.sy
                        , dataset2 = coef_sy4)
#summary(pre_sy4$error) ; summary(pre_sy4$abserr)

pre_sy2 <- predict_func(dataset1 = sy2_d
                        #data_new.sy
                        , dataset2 = coef_sy2)
#summary(pre_sy2$error) ;summary(pre_sy2$abserr)


## ------------------ zz
pre_zz8 <- predict_func(dataset1 = zz8_d
                        #data_new_zz
                        , dataset2 = coef_zz8)
#summary(pre_zz8$error) ; summary(pre_zz8$abserr)

pre_zz6 <- predict_func(dataset1 = zz6_d
                        #data_new_zz
                        , dataset2 = coef_zz6)
#summary(pre_zz6$error) ; summary(pre_zz6$abserr)

pre_zz4 <- predict_func(dataset1 = zz4_d
                        #data_new_zz
                        , dataset2 = coef_zz4)
#summary(pre_zz4$error) ; summary(pre_zz4$abserr)

pre_zz2 <- predict_func(dataset1 = zz2_d
                        #data_new_zz
                        , dataset2 = coef_zz2)
#summary(pre_zz2$error) ; summary(pre_zz2$abserr)

## ---------------- zj
pre_zj3 <- predict_func(dataset1 = zj3_d
                        #data_new_zj
                        , dataset2 = coef_zj3)
#summary(pre_zj3$error) ; summary(pre_zj3$abserr)

pre_zj1 <- predict_func(dataset1 = zj1_d
                        #data_new_zj
                        , dataset2 = coef_zj1)
#summary(pre_zj1$error) ; summary(pre_zj1$abserr)


## ---------------- zx
pre_zx6 <- predict_func(dataset1 = zx6_d
                        #data_new_zx
                        , dataset2 = coef_zx6)
#summary(pre_zx6$error) ; summary(pre_zx6$abserr)

pre_zx4 <- predict_func(dataset1 = zx4_d
                        #data_new_zx
                        , dataset2 = coef_zx4)
#summary(pre_zx4$error) ; summary(pre_zx4$abserr)

pre_zx2 <- predict_func(dataset1 = zx2_d
                        #data_new_zx
                        , dataset2 = coef_zx2)
#summary(pre_zx2$error) ; summary(pre_zx2$abserr)



# ----------------------------- 第二种预测 ---------------------------------- # 
# AP
predict2_calculate <- function(dataset)
{
  beta0 <-c() ; beta1 <-c() ; beta2 <-c() ; beta3 <-c()
  res <-c() ; r2 <-c()
  
  for(i in 1:58)
  {
    x <- dataset %>% filter(ts >= full_times[i] & ts <= full_times[i+19]) # 共20个月
    y <- dataset %>% filter(ts >= full_times[i+1] & ts <= full_times[i+20])
    
    id1 <- x$asset_id ; id2 <- y$asset_id
    sid <- intersect(id1,id2)
    x <- x[which(x$asset_id %in% sid),] ; y <- y[which(y$asset_id %in% sid),]
    dat <- equal(x,y)
    dat1 <- dat$set1; dat2 <-dat$set2
    x1 = dat1$MktRe ; x2 = dat1$IndRe ; x3 = dat1$IdiosRe
    y = dat2$prof
    reg <- lm(y~x1+x2+x3)
    
    beta0 <- append(beta0,reg$coefficients[1])
    beta1 <- append(beta1,reg$coefficients[2])
    beta2 <- append(beta2,reg$coefficients[3])
    beta3 <- append(beta3,reg$coefficients[4])
    #res <-append(res,reg$residuals)
    r2 <- append(r2,summary(reg)$r.square)
  }
  
  result <- data.frame(beta0,beta1,beta2,beta3,r2)
  return(result)
}

coef2_sy6 <- predict2_calculate(sy6_d) 
coef2_sy4 <- predict2_calculate(sy4_d) ; coef2_sy2 <- predict2_calculate(sy2_d)

coef2_zz8 <-predict2_calculate(zz8_d) ; coef2_zz6 <- predict2_calculate(zz6_d) 
coef2_zz4 <- predict2_calculate(zz4_d) ; coef2_zz2 <- predict2_calculate(zz2_d)

coef2_zj3 <- predict2_calculate(zj3_d) ; coef2_zj1 <- predict2_calculate(zj1_d) 

# 时间不同，mon从39开始，从 2015-08-01 开始
coef2_zx6 <- predict2_calculate(zx6_d) 
coef2_zx4 <- predict2_calculate(zx4_d) ; coef2_zx2 <- predict2_calculate(zx2_d)


# ----------- predict2 --------------- #

# x1 = dat1$MktRe ; x2 = dat1$IndRe ; x3 = dat1$IdiosRe

predict2_func <- function(dataset1,dataset2) # dataset1 = classx_d ; dataset2 = coef2_classx
{ # i = 21 + 58
  #profit <-c() ; predict <-c()
  
  error <-c() ; abserr <-c()
  
  for(i in 1:58)
  {
    dt <- dataset1 %>% filter(ts == full_times[i+20]) # 有很多股票
    true <- dataset1 %>% filter(ts == full_times[i+21])
    
    sid <- intersect(dt$asset_id,true$asset_id)
    dt <- dt[which(dt$asset_id %in% sid),]
    true <- true[which(true$asset_id %in% sid),]
    
    #profit <- append(profit,as.numeric(dt$prof))
    #predict <- append(predict,coef_sy6$beta0[i] + coef_sy6$beta[1]*profit)
    MktRe <- dt$MktRe ; IndRe <-dt$IndRe ; IdiosRe <- dt$IdiosRe
    predict <- dataset2$beta0[i] + dataset2$beta1[i] * MktRe + 
      dataset2$beta2[i] * IndRe + dataset2$beta3[i] * IdiosRe
    true <- as.numeric(true$prof)
    
    error <- append(error, predict - true)
    abserr <- append(abserr,abs(predict - true))
  }
  
  result <- data.frame(error,abserr)
  return (result)
}

# --------------- summary ---------------------- #
## ----------- sy
pre2_sy6 <- predict2_func(dataset1 = sy6_d, dataset2 = coef2_sy6)
#summary(pre2_sy6$error) ; summary(pre2_sy6$abserr)

pre2_sy4 <- predict2_func(dataset1 = sy4_d, dataset2 = coef2_sy4)
#summary(pre2_sy4$error) ; summary(pre2_sy4$abserr)

pre2_sy2 <- predict2_func(dataset1 = sy2_d, dataset2 = coef2_sy2)
#summary(pre2_sy2$error) ;summary(pre2_sy2$abserr)


## ---------- zz
pre2_zz8 <- predict2_func(dataset1 = zz8_d, dataset2 = coef2_zz8)
#summary(pre2_zz8$error) ; summary(pre2_zz8$abserr)

pre2_zz6 <- predict2_func(dataset1 = zz6_d, dataset2 = coef2_zz6)
#summary(pre2_zz6$error) ; summary(pre2_zz6$abserr)

pre2_zz4 <- predict2_func(dataset1 = zz4_d, dataset2 = coef2_zz4)
#summary(pre2_zz4$error) ; summary(pre2_zz4$abserr)

pre2_zz2 <- predict2_func(dataset1 = zz2_d, dataset2 = coef2_zz2)
#summary(pre2_zz2$error) ; summary(pre2_zz2$abserr)

## ----------- zj
pre2_zj3 <- predict2_func(dataset1 = zj3_d, dataset2 = coef2_zj3)
#summary(pre2_zj3$error) ; summary(pre2_zj3$abserr)

pre2_zj1 <- predict2_func(dataset1 = zj1_d, dataset2 = coef2_zj1)
#summary(pre2_zj1$error) ; summary(pre2_zj1$abserr)


## ------------- zx
pre2_zx6 <- predict2_func(dataset1 = zx6_d, dataset2 = coef2_zx6)
#summary(pre2_zx6$error) ; summary(pre2_zx6$abserr)

pre2_zx4 <- predict2_func(dataset1 = zx4_d, dataset2 = coef2_zx4)
#summary(pre2_zx4$error) ; summary(pre2_zx4$abserr)

pre2_zx2 <- predict2_func(dataset1 = zx2_d, dataset2 = coef2_zx2)
#summary(pre2_zx2$error) ; summary(pre2_zx2$abserr)


# ---------- 第三种预测 ----------------- #
# CP
predict3_calculate <-function(dataset)
{
  alpha0 <-c(); alpha1 <- c();
  beta0 <-c() ; beta1 <-c() ; 
  gam0 <-c(); gam1 <-c()
  
  for(i in 39:58)
  {
    x <- dataset %>% filter(ts >= full_times[i] & ts <= full_times[i+19]) # 共20个月
    y <- dataset %>% filter(ts >= full_times[i+1] & ts <= full_times[i+20])
    
    id1 <- x$asset_id ; id2 <- y$asset_id
    sid <- intersect(id1,id2)
    x <- x[which(x$asset_id %in% sid),] ; y <- y[which(y$asset_id %in% sid),]
    dat <- equal(x,y)
    dat1 <- dat$set1; dat2 <-dat$set2
    
    x1 = dat1$MktRe ; x2 = dat1$IndRe ; x3 = dat1$IdiosRe
    y1 = dat2$MktRe ; y2 = dat2$IndRe ; y3 = dat2$IdiosRe
    
    reg1 <- lm(y1~x1) ; reg2 <-lm(y2~x2); reg3 <- lm(y3~x3)
    
    alpha0 <- append(alpha0,reg1$coefficients[1]); alpha1 <- append(alpha1,reg1$coefficients[2])
    beta0 <- append(beta0,reg2$coefficients[1]); beta1 <- append(beta1,reg2$coefficients[2])
    gam0 <- append(gam0,reg3$coefficients[1]) ; gam1 <- append(gam1,reg3$coefficients[2])
    
  }
  
  result <- data.frame(alpha0,alpha1,beta0,beta1,gam0,gam1)
  return(result)
  
}

coef3_sy6 <- predict3_calculate(sy6_d) 
coef3_sy4 <- predict3_calculate(sy4_d) ; coef3_sy2 <- predict3_calculate(sy2_d)

coef3_zz8 <-predict3_calculate(zz8_d) ; coef3_zz6 <- predict3_calculate(zz6_d) 
coef3_zz4 <- predict3_calculate(zz4_d) ; coef3_zz2 <- predict3_calculate(zz2_d)

coef3_zj3 <- predict3_calculate(zj3_d) ; coef3_zj1 <- predict3_calculate(zj1_d) 

# 时间不同，mon从39开始，从 2015-08-01 开始
coef3_zx6 <- predict3_calculate(zx6_d) 
coef3_zx4 <- predict3_calculate(zx4_d) ; coef3_zx2 <- predict3_calculate(zx2_d)
#coef3_zx2 <- coef2_zx3

# --------------  predict 3 ----------------- #
predict3_func <- function(dataset1,dataset2)
  # dataset1 = classx_d ; dataset2 = coef2_classx
{
  error <-c() ; abserr <-c()
  
  for(i in 1:58)
  {
    dt <- dataset1 %>% filter(ts == full_times[i+20]) # 有很多股票
    true <- dataset1 %>% filter(ts == full_times[i+21])
    
    sid <- intersect(dt$asset_id,true$asset_id)
    dt <- dt[which(dt$asset_id %in% sid),]
    true <- true[which(true$asset_id %in% sid),]
    
    #profit <- append(profit,as.numeric(dt$prof))
    #predict <- append(predict,coef_sy6$beta0[i] + coef_sy6$beta[1]*profit)
    #MktRe <- as.numeric(dt$prof) # 是一个向量
    predict <- dataset2$alpha1[i]*dt$MktRe 
    + dataset2$beta1[i]*dt$IndRe 
    + dataset2$gam1[i] * dt$IdiosRe
    
    true <- as.numeric(true$prof)
    
    error <- append(error, predict - true)
    abserr <- append(abserr,abs(predict - true))
  }
  
  result <- data.frame(error,abserr)
  return (result)
}

pre3_sy6 <- predict3_func(dataset1 = sy6_d, dataset2 = coef3_sy6)
#summary(pre2_sy6$error) ; summary(pre2_sy6$abserr)

pre3_sy4 <- predict3_func(dataset1 = sy4_d, dataset2 = coef3_sy4)
#summary(pre2_sy4$error) ; summary(pre2_sy4$abserr)

pre3_sy2 <- predict3_func(dataset1 = sy2_d, dataset2 = coef3_sy2)
#summary(pre2_sy2$error) ;summary(pre2_sy2$abserr)


## ---------- zz
pre3_zz8 <- predict3_func(dataset1 = zz8_d, dataset2  = coef3_zz8)
#summary(pre2_zz8$error) ; summary(pre2_zz8$abserr)

pre3_zz6 <- predict3_func(dataset1 = zz6_d, dataset2 = coef3_zz6)
#summary(pre2_zz6$error) ; summary(pre2_zz6$abserr)

pre3_zz4 <- predict3_func(dataset1 = zz4_d, dataset2 = coef3_zz4)
#summary(pre2_zz4$error) ; summary(pre2_zz4$abserr)

pre3_zz2 <- predict3_func(dataset1 = zz2_d, dataset2 = coef3_zz2)
#summary(pre2_zz2$error) ; summary(pre2_zz2$abserr)

## ----------- zj
pre3_zj3 <- predict3_func(dataset1 = zj3_d, dataset2 = coef3_zj3)
#summary(pre2_zj3$error) ; summary(pre2_zj3$abserr)

pre3_zj1 <- predict3_func(dataset1 = zj1_d, dataset2 = coef3_zj1)


## ------------- zx
pre3_zx6 <- predict3_func(dataset1 = zx6_d, dataset2 = coef3_zx6)
#summary(pre2_zx6$error) ; summary(pre2_zx6$abserr)

pre3_zx4 <- predict3_func(dataset1 = zx4_d, dataset2 = coef3_zx4)
#summary(pre2_zx4$error) ; summary(pre2_zx4$abserr)

pre3_zx2 <- predict3_func(dataset1 = zx2_d, dataset2 = coef3_zx2)
#summary(pre2_zx2$error) ; summary(pre2_zx2$abserr)





# ------------------------------ % superior -------------------------------------- #

sup_calculate <- function(dataset1,dataset2) 
  # dataset1 = pre_classx TP; dataset2 = AP,CP
{ 
  num = 0
  diff <- dataset1$abserr - dataset2$abserr
  for(i in diff)
  {
    if(i > 0)
      num = num +1
  }
  sup = num/length(diff) 
  sup = sup*100
  print(sup)
}

# --------- sy
sup_calculate(pre_sy6,pre3_sy6)  
sup_calculate(pre_sy4,pre3_sy4) 
sup_calculate(pre_sy2,pre3_sy2)

# -------- zz
sup_calculate(pre_zz8,pre3_zz8)  
sup_calculate(pre_zz6,pre3_zz6)  
sup_calculate(pre_zz4,pre3_zz4) 
sup_calculate(pre_zz2,pre3_zz2)

# -------- zj
sup_calculate(pre_zj3,pre3_zj3)  
sup_calculate(pre_zj1,pre3_zj1)

# -------- zx
sup_calculate(pre_zx6,pre3_zx6)  
sup_calculate(pre_zx4,pre3_zx4) 
sup_calculate(pre_zx2,pre3_zx2)

# use the function
#beta0 <-c() ; beta1 <-c()
#res <-c() ; r2 <-c()
