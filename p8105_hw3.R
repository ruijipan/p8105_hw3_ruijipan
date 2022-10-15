find.package('p8105.datasets')
# D:\Program Files\R\R-4.2.1\library\p8105.datasets

library(p8105.datasets)
library(tidyr)
data("instacart")


df = data.frame(instacart)
max(instacart$aisle)

t = table(instacart$aisle)
d = as.data.frame(t[t>10000])
d

library(ggplot2)
ggplot(data = d,mapping = aes(x = Var1, y = Freq)) + 
  geom_col(aes(color = Var1),alpha = 0.5) +
  coord_flip()

attach(df)

sort(t,decreasing = TRUE)

subset(df, aisle=='dog food care' | aisle=='packaged vegetables fruits' | aisle=='baking ingredients',
       c(product_name,order_number,aisle))
sub = subset(df, product_name=='Pink Lady Apples'| product_name=='Coffee Ice Cream', 
       c(product_name,order_hour_of_day,order_dow))
group_mean = aggregate(sub$order_hour_of_day,by=list(sub$product_name,sub$order_dow),mean)
names(group_mean) = c('product_name','order_dow','mean_hour')
group_mean

###########################################
UG = read.csv('./data/accel_data.csv')

UG$weekend = as.integer(UG$day %in% c('Saturday','Sunday'))
UG$weekday = as.integer(!(UG$day %in% c('Monday','Sunday')))


UG$activity_sumday = rowSums(UG[,-c(1:3)])
attach(UG)
UG[c(1:3,1444:1446)]

library(ggplot2)

ggplot(data = UG,mapping = aes(x = day_id, y = activity_sumday)) + 
  geom_col(aes(color = day),alpha = 0.5)

############################################
library(p8105.datasets)
data("ny_noaa")

ny_noaa$prcp == 'NA'

ny_noaa$prcp




subset(ny_noaa,prcp!=NA,c(id,date,prcp,snow))

subset(ny_noaa,tmax!=NA,c(id,date,prcp,snow))


sum(is.na(ny_noaa$tmax))

length(ny_noaa$id)
attach(ny_noaa)

ny_noaa$prcp <- NULL
names(ny_noaa)

ny_noaa = na.omit(ny_noaa)
length(ny_noaa$id)


ny_noaa$date = as.character(ny_noaa$date)
data = strsplit(x = ny_noaa$date,split = '-')
data = as.matrix(do.call(rbind, data))
ny_noaa$year = data[,1]
ny_noaa$month = data[,2]
ny_noaa$day = data[,3]



sub_maxtem = subset(ny_noaa,month=='01'|month=='07',c(year,month,tmax))
sub_maxtem = aggregate(sub_maxtem$tmax,by = list(sub_maxtem$month,sub_maxtem$year),max)
names(sub_maxtem) = c('month','year','tmax')
sub_maxtem

library(tidyr)
attach(ny_noaa)
ny_noaa_long = gather(ny_noaa,min_max,tempure,tmax:tmin)

ny_noaa_long

attach(ny_noaa)
sub_snowfall = subset(ny_noaa,snow>0&snow<100,c(year,snow))
sub_snowfall






