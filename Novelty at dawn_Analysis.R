
############################################
######## Buzzard Neophobia Analysis ########
############################################

#Load all required packages
library("ggplot2") #for beautiful graphs

############################################
setwd("~/Documents/Manuscripts/Ongoing/Buzzard's Rafal/Revision Avian Biology Research/Analysis 2")

#Load data files
buzz<-read.table("DataS1_Buzzard_data.txt",header=T,na.strings="na",stringsAsFactors=T)
time <- read.table("DataS2_buzz_times.txt",header=T,na.strings="na")

buzz$Session<-time(buzz$Day)
Hab <- subset(buzz,Condition=="hab")
buzz$Object <- factor(buzz$Object,levels=c("none","small","medium","large"))
buzz$Date <- factor(buzz$Date,levels=c("08.02.","09.02.","10.02.","11.02.","12.02.","14.02.","15.02.","16.02.","17.02.","18.02.","19.02.","20.02.","21.02.","24.02.","27.02.","28.02.","01.03.","04.03.","06.03.","07.03.","08.03.","11.03.","12.03.","13.03.","14.03.","16.03.","17.03."))


#summary of dataset
summary (buzz)
summary(Hab)
summary(time)


###Arrival time
cor.test(as.ts(buzz$Landing_time),as.ts(buzz$Civil_twilight))

# Time of landing FIG.2A in text
time$Measure <- factor(time$Measure, levels = c("Landing_time","Corvids","Sunrise_time","Civil_twilight","Nautical_twilight","Astron_twilight"))
ggplot(time,aes((Day),(times/3600)))+theme_bw()+geom_area(aes(y=(addt/(60*60)),fill=Measure),alpha=0.7)+ scale_fill_grey(start=0.9, end=0,guide=F)+geom_point(aes(shape=Measure,colour=Measure))+scale_colour_manual(values=c("black","black","black","black","black","black"),guide=F)+geom_line(aes(linetype=Measure))+scale_y_continuous(name="Daytime [hh:mm]",limits=c(0,9),breaks=c(3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5),labels = c("03:30","04:00","04:30","05:00","05:30","06:00","06:30","07:00","07:30","08:00","08:30"))+ coord_cartesian(ylim = c(3.5, 8.5))+scale_x_continuous(name="",limits=c(1,42),breaks=c(1,5,10,15,20,25,30,35,40),labels = c("04.02.2019","08.02.2019","13.02.2019","18.02.2019","23.02.2019","28.02.2019","05.03.2019","10.03.2019","15.03.2019"))+theme(legend.position = c(0.91, .85),legend.background = element_rect(size=0.25, linetype="solid",colour ="black"),legend.title = element_blank(),legend.spacing.y = unit(.01, 'cm'))+scale_shape_discrete(labels = c("Buzzard landing","Corvids landing","Sunrise","Civil dawn","Nautical dawn","Astronomical dawn")) +scale_linetype_manual(values=c("dotted","blank","solid","solid","solid","solid"),guide=F)+ annotate("text", x = 1, y = 6.9, label = "Civil twilight",hjust="left")+ annotate("text", x = 1, y = 6.2, label = "Nautical twilight",hjust="left")+ annotate("text", x = 1, y = 5.5, label = "Astronomical twilight",hjust="left")+annotate("text", x = 1, y = 4.25, label = "Night",hjust="left")

ggsave("~/Documents/Manuscripts/Ongoing/Buzzard's Rafal/Revision Avian Biology Research/Analysis 2/Fig.2A.png",width=25,height=16, unit="cm",dpi=300 )

### Latency to feed
# No correlation between latency and session
cor.test(buzz$Latency,buzz$Session)

# trend of windspeed influencing latency in baseline sessions
cor.test(Hab$Latency,Hab$Wind)

# no correlation of temp and latency in baseline sessions
cor.test(Hab$Latency,Hab$Temp)

# no correlation of Humidity and latency in baseline sessions
cor.test(Hab$Latency,Hab$Humidity)

# no correlation of Preassure and latency in baseline sessions
cor.test(Hab$Latency,Hab$Mbar)

# no correlation of LUX and latency in baseline sessions
cor.test(Hab$Latency,Hab$LUX)

#Descriptive Baseline Sessions
summary (Hab)
SD<-sd(Hab$Latency, na.rm=T)
N<-sum(complete.cases(Hab$Latency))
SE<-SD/sqrt(N)

#bootstrapped CI of baseline latencies
CIHab<-numeric (10000)
for (i in 1:10000) CIHab[i]<-mean(sample(Hab$Latency,
								replace=T), na.rm=T)
	quantile(CIHab,c(0.025,0.975))				


# Latency over all Session (Days) showing effect of Objects alternative FIG.2B?
ggplot (buzz, aes(Date,Latency, size=Object, shape=Object))+geom_point()+theme_bw()+annotate("rect",ymin=mean(Hab$Latency)-1.5*IQR(Hab$Latency),ymax=mean(Hab$Latency)+1.5*IQR(Hab$Latency),xmin=0,xmax=28,fill="light gray",alpha=.4)+geom_hline(yintercept=mean(Hab$Latency))+scale_shape_manual(values=c(3,20,18,15))+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+geom_hline(yintercept=mean(buzz$Latency[buzz$Object=="large"]),linetype="dashed")+geom_hline(yintercept=mean(buzz$Latency[buzz$Object=="medium"]),linetype="dotdash")+geom_hline(yintercept=mean(buzz$Latency[buzz$Object=="small"]),linetype="dotted")+ylab("Latency [sec.]")+xlab("Session date")
ggsave("~/Documents/Manuscripts/Ongoing/Buzzard's Rafal/Revision Avian Biology Research/Analysis 2/Fig.2B.png",width=25,height=16, unit="cm",dpi=300 )


# Fearresponses over all Session (Days) showing effect of Objects alternative FIG.2C?
ggplot (buzz, aes(Date,Fearresponse, size=Object, shape=Object))+geom_point()+theme_bw()+scale_shape_manual(values=c(3,20,18,15))+ylab("Fear responses [freq.]")+xlab("Session date")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("~/Documents/Manuscripts/Ongoing/Buzzard's Rafal/Revision Avian Biology Research/Analysis 2/Fig.2C.png",width=25,height=16, unit="cm",dpi=300 )

# Exploration over all Session (Days) showing effect of Objects alternative FIG.2D?
ggplot (buzz, aes(Date,Exploration, size=Object, shape=Object))+geom_point()+theme_bw()+scale_shape_manual(values=c(3,20,18,15))+ylab("Exploration events [freq.]")+xlab("Session date")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("~/Documents/Manuscripts/Ongoing/Buzzard's Rafal/Revision Avian Biology Research/Analysis 2/Fig.2D.png",width=25,height=16, unit="cm",dpi=300 )
