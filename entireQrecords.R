shp=st_read('E:\\research\\GSHA\\StationsV3\\GlobalGauges_1979_V3.shp')
path='E:\\research\\GlobalGaugeData\\combined\\'

for(i in 1:nrow(shp)){
  df=fread(paste0(path,shp$Sttn_Nm[i],'.csv'))
  fwrite(df,paste0('E:\\research\\GSHA\\entire_Q_records\\',shp$Sttn_Nm[i],'.csv'))
}



ryan=list()
for(i in 1:100){
  old=fread(paste0(path,shp$Sttn_Nm[i],'.csv'))
  new=fread(paste0('E:\\research\\GSHA\\entire_Q_records\\',shp$Sttn_Nm[i],'.csv'))
  ryan[[i]]=all(new==old)
}
ryan=unlist(ryan)
unique(ryan)
