library(data.table)
library(quantmod)
library(tidyverse)
library(tweenr)
library(animation)
library(ggplot2)
library(ggthemes)
library(artyfarty)
library(extrafont)


# Download data
ur.data<-fread("https://download.bls.gov/pub/time.series/la/la.data.1.CurrentS")

# Download series ids

ur.series<-fread("https://download.bls.gov/pub/time.series/la/la.series")

# subset data
ur.list<-ur.series[area_type_code =="A" &   #get states
                     measure_code == "3"  &   #get unemployment rate
                     seasonal == "S",         #get seasonally adjusted data
                   c("series_id","area_code","series_title"),
                   with=F]

## Get state names and area crosswalk
ur.area<-fread("https://download.bls.gov/pub/time.series/la/la.area",
               col.names=
                 c("area_type_code","area_code","area_text","display_level",
                   "selectable","sort_sequence","blank"))                   

# merge data
ur.dt<-merge(ur.data,ur.list,by="series_id",all.y=T)

#create data variable
ur.dt[,month:=as.numeric(substr(ur.dt$period,2,3))]
ur.dt$date<- as.Date(ISOdate(ur.dt$year,ur.dt$month,1) ) #set up date variable
ur.dt<-merge(ur.dt,ur.area[,c("area_text","area_code"),with=F],by="area_code")


# Load national unemployment rate using quantmod and FRED database

# helpful reference https://jeffreybreen.wordpress.com/tag/quantmod/
unrate = getSymbols('UNRATE',src='FRED', auto.assign=F) 
unrate.df = data.frame(date=time(unrate), coredata(unrate) )

# Drop some columns
ur.dt2<-ur.dt[,c("date","area_text","value"),with=F]

## rename variables
ur.dt2<-dplyr::rename(ur.dt2, state=area_text)
ur.dt2<-dplyr::rename(ur.dt2, ur=value)

# merge national unemploymnent 
ur.dt2<-merge(ur.dt2,unrate.df,by="date")
ur.dt2<-dplyr::rename(ur.dt2, ur.us=UNRATE)  #rename UNRATE to ur.us

# create variables for use in ribbon chart
ur.dt2[,up:=ifelse(ur>ur.us,ur,ur.us)]
ur.dt2[,down:=ifelse(ur<ur.us,ur,ur.us)]

# drop D.C. and Puerto Rico (so we can have 50 plots in small multiple)
ur.plot<-ur.dt2[! state %in% c("Puerto Rico","District of Columbia")]


# Get list of states:
st.list<-unique(ur.plot$state)

#Add U.S. as it's own state (for use in animation)
ur.plot.us<-copy(ur.plot)[state=="Alabama"]
ur.plot.us[,state:="United States"]
ur.plot.us[,ur:=ur.us]
ur.plot.us[,up:=ur.us]
ur.plot.us[,down:=ur.us]
ur.plot2<-rbind(ur.plot,ur.plot.us)


# Create plotting function
myplotf<-function(df){
  g<-
    ggplot(data=df,aes(x=date,y=ur))+
    geom_line(color="black")+
    geom_line(linetype=2,aes(y=ur.us))+
    geom_ribbon(aes(ymin=ur,ymax=down),fill="#d73027",alpha=0.5)+
    geom_ribbon(aes(ymin=ur,ymax=up),fill="#4575b4",alpha=0.5)+
    facet_wrap(~state,ncol=10,scales="free_x")+
    scale_y_continuous(limits=c(0,20))+
    theme_minimal()+
    theme(legend.position="top",
          plot.caption=element_text(family="Avenir", hjust=0, size=10),
          plot.subtitle=element_text(family="Avenir", size=10),
          plot.title=element_text(family="Avenir",size=16))+
    labs(x="",y="",
         title="The State of U.S. Jobs",
         subtitle="Solid line is state unemployment rate, dotted line is U.S. average unemployment rate\nRed [blue] indicates the state level is higher [lower] than the national average",
         caption="Data Source: U.S. Bureau of Labor Statistics\nViz based on https://rud.is/b/2017/01/18/workout-wednesday-redux-2017-week-3/,\nitself based on http://thedailyviz.com/2016/12/14/four-decades-of-state-unemployment-rates-in-small-multiples-part-2/")+
    geom_rug(aes(color=ifelse(ur>ur.us,"Worse","Same or Better")),sides="b")+
    scale_color_manual(values=c("#4575b4","#d73027"),name="Better or worse than U.S.")
  return(g)
}


# Data subsetting function

myf<-function(s){
  df<- ur.plot2[state==s]
  df %>% map_if(is.character, as.factor) %>% as_data_frame -> df
  return(df)
}


myplotf(myf("California"))

# Create Small Multiple

ggplot(data=ur.plot,aes(x=date,y=ur))+
  geom_line(color="black")+
  geom_line(linetype=2,aes(y=ur.us))+
  geom_ribbon(aes(ymin=ur,ymax=down),fill="#d73027",alpha=0.5)+
  geom_ribbon(aes(ymin=ur,ymax=up),fill="#4575b4",alpha=0.5)+
  facet_wrap(~state,ncol=10,scales="free_x")+
  theme_minimal()+
  theme(legend.position="top",
        plot.caption=element_text(family="Avenir", size=10, hjust=0),
        plot.subtitle=element_text(family="Avenir", size=10),
        plot.title=element_text(family="Avenir", size=16))+
  labs(x="",y="",
       title="The State of U.S. Jobs",
       subtitle="Solid line is state unemployment rate, dotted line is U.S. average unemployment rate\nRed [blue] indicates the state level is higher [lower] than the national average",
       caption="Data Source: U.S. Bureau of Labor Statistics\nViz based on https://rud.is/b/2017/01/18/workout-wednesday-redux-2017-week-3/, itself based on http://thedailyviz.com/2016/12/14/four-decades-of-state-unemployment-rates-in-small-multiples-part-2/")+
  geom_rug(aes(color=ifelse(ur>ur.us,"Worse","Better")),sides="b")+
  scale_color_manual(values=c("#4575b4","#d73027"),name="Better or worse than U.S.")


# Create Animation

mylist<-lapply(c("United States",st.list,"United States"),myf)
tween.df<-tween_states(mylist,tweenlength=1,statelength=2, ease=rep('cubic-in-out',53), nframes=250)
tween.df<-data.table(tween.df)


oopt = ani.options(interval = 0.2)
saveGIF({for (i in 1:max(tween.df$.frame)) {
  g<-myplotf(tween.df[.frame==i,])
  print(g)
  print(i)
  ani.pause()
}
},movie.name="Workout UR jan 18 2017 v2.gif",ani.width = 600, ani.height =400)
