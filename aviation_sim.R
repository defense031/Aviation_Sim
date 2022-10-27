library(lubridate)
library(FAdist)
library(truncnorm)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Initialize
#Run this every time
heli<-as.data.frame(matrix(data=NA,nrow=24,ncol=20))
colnames(heli)<-c("serial","day","tot_hours","phase_hours","status","FMC","fly",
                  "flying_hours","period","maint","phase_maint","queue","hours_since_maint","fail_time","fail","or",
                  "maint125","maint250","maint500","iter")
totals<-heli
heli$serial<-seq(from=1,to=length(heli$serial),by=1)

#inspection marks:
inspect<-as.data.frame(matrix(data=NA,nrow=3,ncol=6))
colnames(inspect)<-c("type","hours","days","min","max","sd")
inspect$type<-c("Major","Major","Minor")
inspect$hours<-c(500,250,125)
inspect$days<-c(7,5,1) ### Need to figure out what these averages are 
inspect$min<-c(3,2,0)
inspect$max<-c(14,8,4)
inspect$sd<-c(2,1,.5)

#sortie length parameters
sortie_mean<-4.5
sortie_sd<-.3

#Bathtub "breaks"
bathtub<-c(10,75)

#Initialization (Don't have to run this before loop)
#Determine size of squadron and starting date
          day<-0
          heli$period<-1
          size<-24
          #Get date
          heli$day<-day
          #Determine initial vector for each aircraft's starting time
          heli$tot_hours<-0
          #heli$hours<-rtruncnorm(n=24,a=0,b=500,mean=100,sd=200)
          #Determine each aircraft's starting status
          heli$status<-"FMC"
          #Determine FMC Y/N
          heli[which(heli$status=="FMC"),which(colnames(heli)=="FMC")]<-1
          #Initialize all day's flights to false
          heli$fly<-FALSE
          #Initialize maintenance requirement for each aircraft
          heli$maint<-0
          #Days since last maintenance
          heli$hours_since_maint<-0
          #Set 3 maintenance phase inspections to 0 (meaning they haven't been completed)
          heli$maint125<-0
          heli$maint250<-0
          heli$maint500<-0
          #Reporting period size
          report<-30
          #Initialize reporting periods (how many to run in sim)
          periods<-4
          #calculate OR for current reporting period
          #First have to initialize historical records
          archive<-heli
          #manually set or to 1
          or<-1
          #Start at p=1
          p<-0

#Number of iterations
iter<-1
#Reporting period size
report<-30
#Initialize reporting periods (how many to run in sim)
periods<-10
#Number of helicopters
size<-24

#Reset everything
heli<-as.data.frame(matrix(data=NA,nrow=24,ncol=20))
colnames(heli)<-c("serial","day","tot_hours","phase_hours","status","FMC","fly",
                  "flying_hours","period","maint","phase_maint","queue","hours_since_maint","fail_time","fail","or",
                  "maint125","maint250","maint500","iter")
heli$serial<-seq(from=1,to=length(heli$serial),by=1)
totals<-heli
#Loop it up!
for(i in 1:iter){
  heli<-as.data.frame(matrix(data=NA,nrow=24,ncol=20))
  colnames(heli)<-c("serial","day","tot_hours","phase_hours","status","FMC","fly",
                    "flying_hours","period","maint","phase_maint","queue","hours_since_maint","fail_time","fail","or",
                    "maint125","maint250","maint500","iter")
  heli$serial<-seq(from=1,to=length(heli$serial),by=1)
  #Initialize all to a clean slate
  day<-0
  heli$period<-1
  heli$iter<-i
  #Get date
  heli$day<-day
  #Determine initial vector for each aircraft's starting time
  heli$tot_hours<-seq(from=0,to=(size-1)*floor(500/size),by=floor(500/size))
  #Set phase hours equal to tot_hours at first
  heli$phase_hours<-heli$tot_hours
  #Determine each aircraft's starting status
  heli$status<-"FMC"
  #Determine FMC Y/N
  heli[which(heli$status=="FMC"),which(colnames(heli)=="FMC")]<-1
  #Initialize all day's flights to false
  heli$fly<-FALSE
  #Initialize maintenance requirement for each aircraft
  heli$maint<-0
  heli$phase_maint<-0
  heli$queue<-0
  #Days since last maintenance
  heli$hours_since_maint<-0
  #Set 3 maintenance phase inspections to 0 (meaning they haven't been completed)
  heli$maint125<-0
  heli$maint250<-0
  heli$maint500<-0
  
  #change inspection statuses for helicopters that are past thresholds already
  heli[which(heli$phase_hours>=125),which(colnames(heli)=="maint125")]<-1
  heli[which(heli$phase_hours>=250),which(colnames(heli)=="maint250")]<-1

  #First have to initialize historical records
  archive<-heli
  #manually set or to 1
  or<-1
  #Start at p=1
  p<-0
  
#Major Inner Loop start
while(p <= periods){
  for(s in 1:size){
  #determine if each aircraft flies or not
  if(or>=.75){
    #if you hit maintenance threshold, take aircraft offline
    if(heli$maint125[s]==0&heli$phase_hours[s]>=125){
          heli$status[s]<-"NMC"
          heli$FMC[s]<-0
          heli$fly[s]<-0
          heli$maint125[s]<-1
          heli$phase_maint[s]<-s
          if(max(heli$queue==0)){heli$queue[s]<-1 #check queue, if it's 0 make it first.  Else add it to the stack
                                }else{heli$queue[s]<-max(heli$queue)+1}
          heli$maint[s]<-ceiling(rtruncnorm(n=1,
                                  a=inspect[which(inspect$hours==125),which(colnames(inspect)=="min")],
                                  b=inspect[which(inspect$hours==125),which(colnames(inspect)=="max")],
                                  mean=inspect[which(inspect$hours==125),which(colnames(inspect)=="days")],
                                  sd=inspect[which(inspect$hours==125),which(colnames(inspect)=="sd")]))}
    if(heli$maint250[s]==0&heli$phase_hours[s]>=250){
      heli$status[s]<-"NMC"
      heli$FMC[s]<-0
      heli$fly[s]<-0
      heli$maint250[s]<-1
      heli$phase_maint[s]<-s
      if(max(heli$queue==0)){heli$queue[s]<-1#check queue, if it's 0 make it first.  Else add it to the stack
                            }else{heli$queue[s]<-max(heli$queue)+1}
      heli$maint[s]<-heli$maint[s]<-ceiling(rtruncnorm(n=1,
                                   a=inspect[which(inspect$hours==250),which(colnames(inspect)=="min")],
                                   b=inspect[which(inspect$hours==250),which(colnames(inspect)=="max")],
                                   mean=inspect[which(inspect$hours==250),which(colnames(inspect)=="days")],
                                   sd=inspect[which(inspect$hours==250),which(colnames(inspect)=="sd")]))
      }
    if(heli$maint500[s]==0&heli$phase_hours[s]>=500){
      heli$status[s]<-"NMC"
      heli$FMC[s]<-0
      heli$fly[s]<-0
      heli$maint500[s]<-1
      heli$phase_maint[s]<-s
      if(max(heli$queue==0)){heli$queue[s]<-1#check queue, if it's 0 make it first.  Else add it to the stack
                            }else{heli$queue[s]<-max(heli$queue)+1}
      heli$maint[s]<-heli$maint[s]<-ceiling(rtruncnorm(n=1,
                                   a=inspect[which(inspect$hours==500),which(colnames(inspect)=="min")],
                                   b=inspect[which(inspect$hours==500),which(colnames(inspect)=="max")],
                                   mean=inspect[which(inspect$hours==500),which(colnames(inspect)=="days")],
                                   sd=inspect[which(inspect$hours==500),which(colnames(inspect)=="sd")]))
      #reset phase hours to 0
      heli$phase_hours[s]<-0
      heli$hours_since_maint[s]<-0
      #reset 125/250/and 500 hour phases to 0
      heli$maint125[s]<-0
      heli$maint250[s]<-0
      heli$maint500[s]<-0
      }#Ends 500 hour phase
    
    #Also determine if they broke down, and how severe the malfunction is:
    #Get random pulls for each helicopter to see when they break (unscheduled maintenance)
    if(heli$maint[s]==0){#check if helicopter is functional first
      #Bin into groups based on hours since maintenance (bathtub curve)
      if(heli$hours_since_maint[s]<=bathtub[1]){
        #Infant
        heli$fail_time[s]<-rweibull(n=1,shape=1,scale=50)
      }else if(heli$hours_since_maint[s]>=bathtub[2]){
        #Elder
        heli$fail_time[s]<-rweibull(n=1,shape=15,scale=125)
      }else{
        #Steady state
        heli$fail_time[s]<-rweibull(n=1,shape=10,scale=75)
      }
        if(heli$hours_since_maint[s]>heli$fail_time[s]){#If random pull is true, then helicopter needs unscheduled maintenance
          heli$fail[s]<-1
          #If it failed, determine how long the repair will take (0 means 1 day of not flying)
          heli$maint[s]<-ceiling(runif(n=1,min=0,max=2))
          #If helicopter has failure, change status
          heli$status[s]<-"NMC"
          heli$FMC[s]<-0
          heli$fly[s]<-0
          heli$hours_since_maint[s]<-0}
        else{
          heli$fail[s]<-0
          heli$status[s]<-"FMC"
          heli$FMC[s]<-1
          heli$fly[s]<-1
          }
    }#closes heli maint == 0 loop
    #If they flew, determine how many hours
    heli$flying_hours[s]<-heli[s,which(colnames(heli)=="fly")]*
                                    rtruncnorm(n=1,a=0,b=10,mean=sortie_mean,sd=sortie_sd)
    #Increment maintenance counter
    heli$hours_since_maint[s]<-heli$hours_since_maint[s]+heli$flying_hours[s]
    #Increase total flying hours and phase hours
    heli$tot_hours[s]<-heli$tot_hours[s]+heli$flying_hours[s]
    heli$phase_hours[s]<-heli$phase_hours[s]+heli$flying_hours[s]
  
  }else{heli$fly<-0 #if OR condition isn't satisfied, ground all aircraft for that day
        heli$flying_hours<-0
  }#end or >.75 condition
      #Perform end of day updates
      #Decrement maintenance days by one day if unscheduled maintenance
      if(heli$maint[s]>0 & heli$phase_maint[s]==0){
        heli$maint[s]<-(heli$maint[s]-1)
      }
        #if phase maintenance is occurring and the ticker reaches 0, turn off phase maintenance tracker
      if(heli$maint[s]>0 & heli$phase_maint[s]>0){ #&heli$queue[s]==1){
          heli$maint[s]<-heli$maint[s]-1#decrease phase maintenance days if first in the queue
      }
      if(heli$maint[s]==0 & heli$phase_maint[s]==0){
      #If maintenance is no longer required, change heli status 
          heli$status[s]<-"FMC"
          heli$FMC[s]<-1
      }
      if(heli$maint[s]==0 & heli$phase_maint[s]>0){
          heli$status[s]<-"FMC"
          heli$FMC[s]<-1
          heli$phase_maint[s]<-0
          heli$hours_since_maint[s]<-0
          #decrement queue for all of those in line
          heli$queue[which(heli$queue>0)]<-heli$queue[which(heli$queue>0)]-1
      }
  } #end of S loop

  #Update date, hours
  if(heli$day[1]%%report==0){#change reporting period if we are divisible by period
    p=p+1
    heli$period<-p
    heli$day<-1
  }else{heli$day<-heli$day+1}
  #If it's not the end of a reporting period, add 1 to day
  archive<-rbind(archive,heli)
  #OR = (sum of FMC days() / (total days) over the whole fleet
  or<-sum(archive[which(archive$period==p),which(colnames(archive)=="FMC")])/
            ((max(archive[which(archive$period==p),which(colnames(archive)=="day")])+1)*size)
  heli$or<-or
  
} #End of period loop
  #Gets rid of burn in period (day 0 initialization)
  archive<-archive[-c(1:24),]
  #Gets rid of last 25 (it runs first day of next period and then always stops at that point)
  archive<-archive[-c(length(archive$serial):(length(archive$serial))+24),]
  totals<-rbind(totals,archive)
} #End of total loop

  
totals<-totals[-c(1:24),]

heli$queue


#Summarise OR, hours by iteration
orSummary<-totals%>%filter(serial==1,day==30)
keep <- c("day", "or", "period","iter")
orSummary <- orSummary[keep]
orSummary$iter<-as.character(orSummary$iter)
orSummary<-orSummary[!is.na(orSummary$or),]

g1<-ggplot(data=orSummary)
g1+geom_line(aes(x=period,y=or,color=iter))+scale_y_continuous(limits=c(.3,1))


if(max(heli$queue==0)){heli$queue[s]<-1#check queue, if it's 0 make it first.  Else add it to the stack
}else{heli$queue[s]<-max(heli$queue)+1}

heli$day[s]

#This kinda works, something to build off of
iter_day<-seq(from=2,to=30,by=1)
match(heli$day[s],iter_day-1)


#Figure out how to get days = days -1 and period = period except if it's 1 give me 30 and period - 1.
if(day%%report==0){}




archive$day


