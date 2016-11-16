
library(survival)
library(KMsurv)
library(OIsurv)

#######6.2
data(alloauto)

haz.smooth<-function(data,n.points=100, b=5)
{
  a<-survfit(Surv(data[,1],data[,2])~1, type="flem")
  H.na<--log(a$surv)
  m<-length(H.na)
  h.na<-(H.na-c(0,H.na[-m]))
  points<-(0:n.points)/n.points*max(data[,1]*(data[,2]==1))
  h.est<-NULL
  for(i in 1:n.points)
  {
    times<-a$time[abs(points[i]-a$time)<=b]
    haz<-h.na[abs(points[i]-a$time)<=b]
    h.est<-c(h.est,sum(1.465*dnorm((times-points[i])/b)*haz)/b)
  }
  data.frame(time=points[-n.points], h.est=h.est)
}
haz.62<-haz.smooth(alloauto[alloauto$type==1,c(1,3)],n.points=100,b=5)

plot(haz.62$time,haz.62$h.est,type="l",xlab = "month",ylab="hazard")

data("larynx")

hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE) 
attach(hmohiv)
expect <- survexp(futime ~ ratetable(age=(accept.dt - birth.dt),sex=1,year=accept.dt,race="white"), jasa, cohort=FALSE,ratetable=survexp.usr)
survdiff(Surv(jasa$futime, jasa$fustat) ~ offset(expect))

data("heart")


test<-c(1,2,3)
which(test==5)
test[0]
test[1]
?inf
inf(test<2.5)



data("larynx")
larynx[1:5,]
larynxfit<-survfit(Surv(time,delta)~stage, data=larynx)
plot(larynxfit, xlab="time to death (months)", ylab="S(t)")
text(4, 0.05, "stage 4")
text(9, 0.17, "stage 3")
text(9, 0.36, "stage 2")
text(9, 0.45, "stage 1")
table(larynx[,2])
larynx[larynx[,2]==0.3,]
larynx[larynx[,2]==0.8,]
Surv(larynx$time, larynx$delta)
S2<-1*(larynx[, "stage"]==2)
S2<-(larynx[, "stage"]==2)
S2
