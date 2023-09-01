a <- data.frame(v1 = c(1,2,3),v2 = c(3,4,5))
a <-c('1','1','2','3','4','3','2')
b<- c('2','3','4','2','3','4','2')
b <- a[!(a %in% b)]
t <- intersect(a,b)
b <-b[which(!(b %in% t))]
b <- data.frame(v1 = c(4,5,NA),v2 = c(6,7,NA))
c <- data.frame(v1 = c(7,8,9),v3 = c(9,10,11))
write_csv(b,'test.csv',col_name = TRUE,append = TRUE)
rbind(a,b)

a <- split(data_use,data_use$asset_id)
for(i in 1:length(a))
{
  lid <- nrow(a[[i]])
  if (lid != 21)
    print(i)
  
}
b <-data.frame(apply(b,2,function(x){
  x[is.na(x)] = mean(x,na.rm = T);x}))



