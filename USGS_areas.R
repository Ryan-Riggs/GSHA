library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
#################################################################################
##First version.
#################################################################################
path = "E:\\research\\GlobalGaugeData\\combined\\"
shp = st_read("E:\\research\\GlobalGaugeData\\Stations\\allUpdated_endYear.shp")
rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeTheseV6.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
shp = shp[shp$Sttn_Nm%!in%rm$Sttn_Nm,]
shp = shp[!is.na(shp$year),]
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
weird =c('143832A_BOM','143828A_BOM','145030A_BOM','143800A_BOM','215240_BOM','W4260002.1_BOM')
shp = shp[shp$Sttn_Nm%!in%weird,]
usgs=shp[shp$agency=='USGS',]

data=dataRetrieval::readNWISsite(usgs$Statin_Nm)
data$contrib_drain_area_va_km2=data$contrib_drain_area_va*2.58999
data$drain_area_va_km2=data$drain_area_va*2.58999
data$Sttn_Nm=paste0(data$site_no,'_USGS')
sub=data[,c('Sttn_Nm','contrib_drain_area_va_km2','drain_area_va_km2')]
sub=sub[!is.na(sub$drain_area_va_km2),]
fwrite(sub,'E:\\research\\GSHA\\areas\\USGS_drain_areas.csv')

usgs$area=sub$drain_area_va_km2[match(usgs$Sttn_Nm,sub$Sttn_Nm)]
usgs=usgs[!is.na(usgs$area),]

ggplot(usgs)+
  geom_sf(aes(color=area))+
  scale_color_viridis_c(trans='log10')


library(tmap)
tmap_mode("view")
tm_shape(usgs) +
  tm_dots(col='area',palette='viridis',
          breaks=c(0,1,100,1000,10000,100000,Inf))
