#Figure 1: Major Indian states affected
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.table("Cases.txt",sep="\n",header=T)
lbls<-df$State
x<-read.table("casescases.txt",sep="\n",header=T)
pct <- round(x$Cases/sum(x$Cases)*100)
lbls <- paste(lbls, pct,sep="\n") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x$Cases,main="Major Indian States Affected",radius=1,cex=0.9,labels = lbls,col=rainbow(length(lbls)))


#Figure 2: State Wise Analysis
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv(file.choose()) #Choose file Daily Data.csv
library("ggplot2")
attach(df)
df1<-data.frame(df$Name.of.State...UT,df$Total.Confirmed.cases,df$Death)
ggplot(df1,aes(fill=Death, y=Total.Confirmed.cases, x=Name.of.State...UT)) + geom_bar(position="stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x="State/UT",y="Total Confirmed Cases",title="State Wise Analysis")


#Figure 3: Map distribution according to severity
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maps)
library(rgdal)
library(scales)
library(maptools)
library(gridExtra)
library(rgeos)
setwd("C:\\Users\\sanjay dhiman\\Downloads")  
states_shape = readShapeSpatial("IND_adm1.shp")
df<-read.csv(file.choose())  #choose file Data State Wise.csv
score=list()
score=df$CONFIRMED
length(score)
class(states_shape)
names(states_shape)
print(states_shape$ID_1)
print(states_shape$NAME_1)
plot(states_shape)
class(states_shape)
score
State_data = data.frame(id=states_shape$ID_1, NAME_1=states_shape$NAME_1, score)
fortify_shape = fortify(states_shape, region = "ID_1")
class(fortify_shape)
Merged_data = merge(fortify_shape, State_data, by="id", all.x=TRUE)
Map_plot = Merged_data[order(Merged_data$order), ]
ggplot() +geom_polygon(data = Map_plot,aes(long,lat, group = group, fill =score),color = "black", size = 0.5) +coord_map()+labs(x="Longitude",y="Latitude",title="Corona in India")+scale_fill_distiller(name="Number of Cases")


#Figure 4: Age Analysis of deceased
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv(file.choose())  #choose file Deceased.csv
df<-data.frame(df)
attach(df)
library("ggplot2")
ggplot(df,aes(Age))+geom_histogram()+labs(x="Age",y="Number of deaths",title="Age analysis of deaths")


#Figure 5: Gender Analysis in Maharashtra
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv("Maharashtra.csv")
df<-data.frame(df)
df1<-na.omit(df)
attach(df)
library("ggplot2")
m=0
f=0
for(i in 1:length(Age))
{
  if(Gender[[i]]=='Male')
    m=m+1
  else if(Gender[[i]]=='Female')
    f=f+1
}
G<-c(m,f)
lblss<-c('Male','Female')
pct <- round(G/sum(G)*100)
lblss <- paste(lblss, pct,sep="\n") # add percents to labels
lblss <- paste(lblss,"%",sep="") # ad % to labels
pie(G,main="Gender Analysis of the deceased in Maharashtra",radius=1,cex=0.9,labels = lblss,stats="count")


#Figure 6: District Distribution Maharashtra
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv("cities maharashtra.csv")
df<-na.omit(df)
lbls<-df$DISTRICT
x<-df$CONFIRMED
pct <- round(x/sum(x)*100)
lbls <- paste(lbls, pct,sep="-") # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x,main="District Distribution",radius=1,cex=0.9,labels = lbls,col=rainbow(length(lbls)))



#Figure 7: Hype in Cases
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv(file.choose())   #choose file Daily Data.csv
attach(df)
library("ggplot2")
y<-sort(Total.Confirmed)
z<-sort(Daily.Confirmed)
x<-sort(Date)
ggplot(df,aes(x,y))+geom_line()+geom_point(aes(color=Total.Confirmed))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Time",y="Total Cases")


#Figure 8: Mortality Rate
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv(file.choose())      #choose file Daily Data.csv
attach(df)
library("ggplot2")
ys<-sort(Total.Confirmed)
zs<-sort(Total.Deceased)
ws<-sort(Total.Recovered)
xs<-sort(Date)

ggplot(df, aes(x=xs)) + 
  geom_line(aes(y = zs), color = "darkred") + 
  geom_line(aes(y = ys), color="steelblue", linetype="twodash") 
ys<-ts(ys)
zs<-ts(zs)
ws<-ts(ws)
ts.plot(ys,zs,ws,col=c("blue","red","green"),plot.type="single",gpars=list(xaxt="n"),lwd=4,ylab="Number of cases")
legend("topleft", legend = c("Total Confirmed","Total Deceased","Total Recovered"),lty = c(1,1,1),lwd=4,col=c("blue","red","green"))


#Figure 9: Test Per million
setwd("C:\\Users\\sanjay dhiman\\Desktop")
df<-read.csv("31 March Test Per million.csv")
attach(df)
library("ggplot2")
barplot(df$Tests.per.million,horiz = T,names.arg = c(df$Country))
ggplot(df, aes(x = Country, y = Tests.per.million, main="Testing per million in various countries")) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Tests per million") +
  scale_x_discrete(name="Country") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))


