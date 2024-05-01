library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)

shp = st_read("E:\\research\\GSHA\\StationsV3\\GlobalGauges_1979_V3.shp")
'%!in%' <- function(x,y)!('%in%'(x,y))
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
path = "E:\\research\\GlobalGaugeData\\combined\\"
outpath="E:\\research\\GSHA\\discharge_filesV3\\"
dt = data.table(Date = seq.Date(as.Date("1700-01-01"), 
                                as.Date("2022-12-31"),1))

statsFun = function(y){
  yr = unique(y$year)
  
  stats=c(0.01,0.1,0.25,0.5,0.75,0.9,0.99)
  names=c('1- percentile', '10- percentile',
          '25- percentile', 'median', '75- percentile',
          '90- percentile', '99- percentile')
  quants = quantile(y$Q, stats)
  stats = as.data.table(t(quants))
  colnames(stats) = names
  stats$mean = mean(y$Q)
  stats$'maximum (AMF)' = max(y$Q)
  stats$'AMF occurrence date' = stringr::str_c(as.character(y$Date[y$Q==stats$`maximum (AMF)`]),collapse=',')
  stats$'frequency of high-flow days'=nrow(y[y$Q>=stats$`90- percentile`])
  y$Date = as.Date(y$Date)

  comb = left_join(dt, y,by='Date')
  comb$high = FALSE
  comb$high = fifelse(comb$Q>=stats$`90- percentile`, TRUE,comb$high)
  comb$high[is.na(comb$high)] = FALSE
  comb$low = FALSE
  comb$low = fifelse(comb$Q<=stats$`10- percentile`, TRUE, comb$low)
  comb$low[is.na(comb$low)] = FALSE

  ##Consecutive high flow days.
  vals=rle(comb$high)$values
  lng = rle(comb$high)$lengths
  high=data.table(vals,lng)[vals==TRUE]
  stats$'average duration of high-flow events' = mean(high$lng)

  stats$'frequency of low-flow days'=nrow(y[y$Q<=stats$`10- percentile`])
  ##Consecutive low flow days.
  vals=rle(comb$low)$values
  lng = rle(comb$low)$lengths
  low=data.table(vals,lng)[vals==TRUE]
  stats$'average duration of low-flow events' = mean(low$lng)
  
  stats$'number of days with Q=0 (days)'=nrow(y[y$Q==0])
  stats$'valid observation days (days)'=nrow(y)
  stats$year = yr
  return(stats)
}



processingFun = function(f){
  df = fread(paste0(path,f, '.csv'))
  df = df[!is.na(Q)&!is.na(Date)&Q>=0]
  df$year=lubridate::year(df$Date)
  df= df#[year>=1979]
  proc = df[,statsFun(.SD),by=year]
  fwrite(proc, paste0(outpath,f,'.csv'))
}
shp = shp[shp$N_1979>0,]

library(parallel)
clust = makeCluster(6)
clusterExport(clust, c('path', 'outpath','dt', 'shp','statsFun'))
clusterEvalQ(clust, c(library(data.table), library(dplyr)))
openFiles = parLapply(clust, shp$Sttn_Nm,processingFun)
stopCluster(clust)

################################################################################
##What is with the zeros?
################################################################################
tab = list()
for(i in 1:nrow(shp)){
  print(i)
  df = fread(paste0(outpath, shp$Sttn_Nm[i], '.csv'))
  df$zeros = as.numeric(df$`number of days with Q=0 (days)`)
  if(any(df$zeros>120)){
    tab[[i]] = shp$Sttn_Nm[i]
  }else{
    tab[[i]] = NA
  }
}
zero = na.omit(unlist(tab))

zeroShp = shp[shp$Sttn_Nm%in%zero,]
library(tmap)
tmap_mode("view")
tm_basemap("Esri.WorldImagery")+
  tm_shape(zeroShp)+
  tm_bubbles(size=0.000005, col="red")


example='Y.25_RID'
df = fread(paste0(path,example, '.csv'))
ryan=fread(paste0(outpath,example,'.csv'))
df$Date=as.Date(df$Date)
df$month = as.factor(lubridate::month(df$Date))
df$year=as.factor(lubridate::year(df$Date))
df$day=lubridate::yday(df$Date)
ggplot(df)+geom_line(aes(x=Date,y=Q))
ggplot(df)+geom_line(aes(x=day,y=Q,color=year))+
  facet_wrap(~month,scales='free')
ggplot(df[,mean(Q),by=list(month)])+geom_line(aes(x=as.numeric(month),y=V1))


agg=fread(paste0(outpath,example,'.csv'))
agg
df[df[, .I[Q == max(Q)], by=year]$V1]
df[,perc:=quantile(Q,0.9),by=year]

##Number of high flow days per year. 
hf=df[df[, .I[Q >= quantile(Q, 0.9)], by=year]$V1][,.N,by=year]
all(hf$N==agg$`frequency of high-flow days`)
df[df[, .I[Q >= quantile(Q, 0.9)], by=year]$V1]

comb = left_join(dt, df,by='Date')
comb$high = FALSE
comb$high = fifelse(comb$Q>=comb$perc, TRUE,comb$high)
comb$high[is.na(comb$high)] = FALSE
comb=comb[year=='2007']
vals=rle(comb$high)$values
lng = rle(comb$high)$lengths
high=data.table(vals,lng)[vals==TRUE]


##Number of low flow days per year. 
lf=df[df[, .I[Q <= quantile(Q, 0.1)], by=year]$V1][,.N,by=year]
all(lf$N==agg$`frequency of low-flow days`)



################################################################################
##Double check the last 100 measurements. 
################################################################################
ryan=shp$Sttn_Nm[35875:35975]
tab=list()
for(i in 1:length(ryan)){
  df = fread(paste0(path,ryan[i], '.csv'))
    df = df[!is.na(Q)&!is.na(Date)&Q>=0]
    df$year=lubridate::year(df$Date)
    df= df#[year>=1979]
    proc = df[,statsFun(.SD),by=year]
    output=fread(paste0(outpath,ryan[i],'.csv'))
    output$`AMF occurrence date`=as.character(output$`AMF occurrence date`)
    tab[[i]]=all.equal(proc,output)
}
tab=unlist(tab)

