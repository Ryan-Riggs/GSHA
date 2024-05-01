library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)

##Actual records. 
path = "E:\\research\\GlobalGaugeData\\combined\\"
shp = st_read("E:\\research\\GlobalGaugeData\\Stations\\allUpdated_endYear_v2.shp")
rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeTheseV7_updatedGRDC.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
shp = shp[shp$Sttn_Nm%!in%rm$Sttn_Nm,]
shp = shp[!is.na(shp$year),]
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
weird =c('143832A_BOM','143828A_BOM','145030A_BOM','143800A_BOM','215240_BOM','W4260002.1_BOM')
shp = shp[shp$Sttn_Nm%!in%weird,]


# #shp = st_read("E:\\research\\GlobalGaugeData\\Stations\\allUpdated_endYear.shp")
# #rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeTheseV5.csv")
# shp = st_read("E:\\research\\GlobalGaugeData\\Stations\\allUpdated_endYear.shp")
# rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeTheseV6.csv")
# shp = shp[!is.na(shp$year),]
# '%!in%' <- function(x,y)!('%in%'(x,y))
# shp = shp[shp$Sttn_Nm%!in%rm$Sttn_Nm,]
# weird =c('143832A_BOM','143828A_BOM','145030A_BOM','143800A_BOM','215240_BOM','W4260002.1_BOM')
shp = shp[shp$Sttn_Nm%!in%weird,]
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
shp = shp[,c('Sttn_Nm', 'agency', 'Statn_Nm')]
path = "E:\\research\\GlobalGaugeData\\combined\\"
dt = data.table(Date = seq.Date(as.Date("1700-01-01"), 
                                as.Date("2022-12-31"),1))

tab = list()
for(i in 1:nrow(shp)){
print(i)
df = fread(paste0(path, shp$Sttn_Nm[i], '.csv'))
mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
all = merge(dt, df, all.x=TRUE)
disc = all[Date<=mx&Date>=mn]
filt = disc[disc$Date>=as.Date('1979-01-01'),]
incld_zero = filt[Q>=0&!is.na(Q)]
nincld_zero = filt[Q>0&!is.na(Q)]
discPerc = nrow(disc[disc$Q<0|is.na(disc$Q)])/nrow(disc)
out = data.table(Sttn_Nm=shp$Sttn_Nm[i],percMsng=discPerc*100,start=mn,end=mx,
                 N_1979_0 = nrow(incld_zero), N_1979 = nrow(nincld_zero))
tab[[i]] = out
}
output = rbindlist(tab)

outShp = merge(shp, output, by='Sttn_Nm')
st_write(outShp, "E:\\research\\GSHA\\StationsV4\\GlobalGauges_1979_V4.shp")
