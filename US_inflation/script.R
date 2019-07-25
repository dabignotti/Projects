#Load some packages
library(data.table, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(zoo, warn.conflicts = FALSE, quietly=TRUE)
library(ggrepel, warn.conflicts = FALSE, quietly=TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly=TRUE)
library(extrafont, warn.conflicts = FALSE, quietly=TRUE)
library(scales, warn.conflicts = FALSE, quietly=TRUE)
library(animation, warn.conflicts = FALSE, quietly=TRUE)
library(grid, warn.conflicts = FALSE, quietly=TRUE)
library(tidyr, warn.conflicts = FALSE, quietly=TRUE)
library(viridis, warn.conflicts = FALSE, quietly=TRUE)
library(ggrepel, warn.conflicts = FALSE, quietly=TRUE)

#read files from BLS.gov
cpi1<-fread('http://download.bls.gov/pub/time.series/cu/cu.data.2.Summaries')
cpi.item<-fread("http://download.bls.gov/pub/time.series/cu/cu.item",
                header=FALSE,col.names=c("item.code","item.name","display.level",
                                         "selectable","sort.sequence","blank"))
cpi.series<-fread("http://download.bls.gov/pub/time.series/cu/cu.series")
cpi2<-merge(cpi.item,cpi.series,by.x="item.code",by.y="item_code")


#merge on series_id variable:
setkeyv(cpi1,"series_id")          
setkeyv(cpi2,"series_id")


cpi3<-cpi2[cpi1]
unique(cpi3$item.code)  #Get list of item codes
cpi3[,month:=as.numeric(substr(cpi3$period,2,3))]
cpi3$date<- as.Date(ISOdate(cpi3$year,cpi3$month,1) ) #set up date variable

cpi4<-cpi3[area_code=="0000" & seasonal=="S" & item.code!= "SAA1" & item.code !="SAA2"]

# Create a variable with the index normalized to 100 in January 2000:
bdata<-cpi4[year==2000 & month==1,]
bdata<-dplyr::rename(bdata, value00=value)
bdata<-bdata[, c('value00','series_id'), with = FALSE]
cpi5<-merge(cpi4,bdata,by="series_id")  #merge back to original data
cpi5[,cpi00:=100*value/value00] 


#get unadjusted index:
cpi4n<-cpi3[area_code=="0000" & seasonal=="U" & item.code!= "SAA1" & item.code !="SAA2" &
              !(period %in% c("S01", "S02", "S03"))]
bdata<-cpi4n[year==2000 & month==1,]
bdata<-dplyr::rename(bdata, value00=value)
bdata<-bdata[, c('value00','series_id'), with = FALSE]
cpi5n<-merge(cpi4n,bdata,by="series_id")

cpi5n[,cpi00:=100*value/value00]
cpi6n<-cpi5n[year>1999]
cpi6n<-cpi6n[,cpilag12:=shift(value,13),by=series_id]
cpi6n<-cpi6n[,datelag12:=shift(date,13),by=series_id]
cpi6n<-cpi6n[,cpi12:=c(rep(NA,13),((1+diff(value,13)/value))^1)-1,by=series_id]  
cpi6n<-cpi6n[,cpi1:=c(rep(NA,12),((1+diff(value,1)/value))^1)-1,by=series_id]  

cpi6<-cpi5[year>1999]
xlim<-c(min(cpi6$date),max(cpi6$date))
dd<-unique(cpi6$date)  #list of dates since January 2000

i<-length(dd)
ggplot(data=cpi6,aes(x=date,y=cpi00,color=item.name))+geom_line()+
  theme_fivethirtyeight()+   
  theme(legend.justification=c(0,0), legend.position="none")+
  scale_y_log10(limits=c(90,200),breaks=c(90,100,120,140,160,180,200))+
  #scale_x_date(limits =xlim)+
  scale_x_date(labels= date_format("%b-%Y"),
               limits = as.Date(c('2000-01-01','2018-12-31')))+
  geom_text_repel(
    data = cpi6[date==dd[i]],
    aes(label = item.name),
    size = 3.5,
    nudge_x = 1) +
  labs(x="", y="Consumer Price Index (log scale, Jan 2000=100, SA)",
       title="Consumer Prices",
       subtitle="by major category",
       caption="Source: U.S. Bureau of Labor Statistics")+
  theme(plot.title=element_text(family="Avenir", size=18))+
  theme(plot.caption=element_text(family="Avenir", hjust=0,vjust=1,margin=margin(t=10)))+
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))

i<-length(dd)  #set index to last date
ggplot(data=cpi6n[date<=dd[i] & !(item.name %in% c("Transportation","Services","Other goods and services"))],aes(x=date,y=cpi12,color=item.name))+
  geom_area(aes(fill=item.name),alpha=0.5)+
  theme_fivethirtyeight()+   theme(legend.justification=c(0,0), legend.position="none")+
  scale_y_continuous(label=percent)+
  geom_hline(yintercept=0,linetype=2,color="black")+
  #scale_y_log10(limits=c(90,200),breaks=c(90,100,120,140,160,180,200))+
  scale_x_date(limits =xlim)+
  #geom_text_repel(    data = subset(cpi6[date<=dd[90]], date == max(date)),    aes(label = item.name, y=180),    size = 5,    nudge_x = 45,    segment.color = NA) +
  labs(x="", y="Consumer Price Index (y/y % change NSA)",
       title="Consumer Price Inflation (y/y %)",
       subtitle="Major Categories",
       caption="Source: U.S. Bureau of Labor Statistics")+
  theme(plot.title=element_text(family="Avenir", size=15))+
  theme(plot.caption=element_text(family="Avenir", hjust=0,vjust=1,margin=margin(t=10)))+
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm"))+facet_wrap(~item.name,ncol=2)

i<-length(dd)  #set index to last date
ggplot(data=cpi6n[date==dd[i] & !(item.name %in% c("Transportation","Services","Other goods and services"))],aes(x=item.name,y=cpi12,color=cpi12))+
  scale_color_viridis(option="D",name="Annual Inflation\nRate (%) ",discrete=F,direction=-1,end=0.85,
                      label=percent)+
  geom_segment(aes(xend=item.name,yend=0),size=1.2)+coord_flip()+
  geom_text(aes(label=paste(" ",percent(round(cpi12,3))," "),
                hjust=ifelse(cpi12>0,0,1)))+  #flip justification if point postiive or negative
  geom_point(size=3)+
  theme_fivethirtyeight()+   
  theme(legend.position="top",legend.text=element_text(family="Avenir",size=7))+
  theme(legend.key.width=unit(3,"cm"))+
  scale_y_continuous(label=percent,limits=c(-0.02,.05),breaks=seq(-0.2,.08,.01))  +
  labs(x="", y="Consumer Price Index (y/y % change NSA)",
       title="Consumer Price Inflation (y/y %)",
       subtitle="by major category",
       caption="Source: U.S. Bureau of Labor Statistics")+
  theme(plot.title=element_text(family="Avenir",size=18))+
  theme(plot.caption=element_text(family="Avenir",hjust=0))

