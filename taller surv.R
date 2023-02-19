library(dplyr)

library(survival)

library(knitr)

library(dplyr)

library(gridExtra)

library(openintro)

library(plotrix)

library(formattable)

library(KMsurv)

library(MASS)

library(ggplot2)

library(plotly)

library(survminer)

library(ggalluvial)

library(circlize)

library(readxl)

library(pillar)

library(kableExtra)

library(survival)

library(VSURF)

library(KMsurv)

library(randomForestSRC)

library(lubridate)

mening <- read_xlsx("/Users/carlosmartinperez/Desktop/MENINGITIS/Meningitis pre-intervención.xlsx",sheet=2)

fechas<- read_xlsx("/Users/carlosmartinperez/Desktop/MENINGITIS/Meningitis pre-intervención.xlsx",sheet=4)


lcr<-as.duration(as.period(interval(mening$Fecha_h_meningitis , mening$Fecha_h_LCR)))
lcr<-ifelse(lcr<0,0,lcr)
lcr<-round(lcr/3600,1)

mening$lcr<-lcr


emp<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_empirico)))
emp<-ifelse(emp<0,0,emp)
emp<-round(emp/3600,1)
mening$emp<-emp

dirigido1<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_dirigido1)))
dirigido1<-ifelse(dirigido1<0,0,dirigido1)
dirigido1<-round(dirigido1/3600,1)
mening$dirigido1<-dirigido1

dirigido2<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_dirigido2)))
dirigido2<-ifelse(dirigido2<0,0,dirigido2)
dirigido2<-round(dirigido2/3600,1)
mening$dirigido2<-dirigido2

dirigido3<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_dirigido3)))
dirigido3<-ifelse(dirigido3<0,0,dirigido3)
dirigido3<-round(dirigido3/3600,1)
mening$dirigido3<-dirigido3

rapidat<-as.duration(as.period(interval(fechas$Fecha_h_rapida , fechas$Fecha_h_dirigido1)))
rapidat<-ifelse(rapidat<0,0,rapidat)
rapidat<-round(rapidat/3600,1)
mening$rapidat<-rapidat

rapida<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_rapida)))
rapida<-ifelse(rapida<0,0,rapida)
rapida<-round(rapida/3600,1)
mening$rapida<-rapida

provi<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_provi)))
provi<-ifelse(provi<0,0,provi)
provi<-round(provi/3600,1)
mening$provi<-provi

recepcion<-as.duration(as.period(interval(fechas$Fecha_h_meningitis , fechas$Fecha_h_recepcion)))
recepcion<-ifelse(recepcion<0,0,recepcion)
recepcion<-round(recepcion/3600,1)
mening$recepcion<-recepcion

final<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_h_resultado_final)))
final<-ifelse(final<0,0,final)
final<-round(final/3600,1)
mening$final<-final

cambiodat<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$Fecha_cambio)))
cambiodat<-ifelse(cambiodat<0,0,cambiodat)
cambiodat<-round(cambiodat/3600,1)
cambiodat
mening$cambiodat<-cambiodat

diagetiol<-as.duration(as.period(interval(fechas$Fecha_h_LCR , fechas$fecha_diag_etiol)))
diagetiol<-ifelse(diagetiol<0,0,diagetiol)
diagetiol<-round(diagetiol/3600,1)
mening$diagetiol<-diagetiol

mening$tiempo<-as.duration(as.period(interval(mening$Fecha_h_meningitis , mening$Fecha_h_alta)))
mening$tiempo<-ifelse(mening$tiempo<0,0,mening$tiempo)
mening$tiempo<-round(mening$tiempo/3600,1)


datos<-matrix(c(lcr,emp,dirigido1,dirigido2,dirigido3,rapidat,rapida,recepcion,provi,final,cambiodat,diagetiol),ncol= 12)
colnames(datos)<-c("lcr","emp", "dirigido1","dirigido2","dirigido3", "rapidat","rapida","recepcion","provi","final",
                   "cambiodat","diagetiol")

datos<-as.data.frame(datos)
datos$PCR2<-mening$PCR2

datos$Año<-mening$Año

ulti<-mening$Año

ulti2<-mening$Año

ulti<-ifelse(ulti>2019 & datos$PCR2=="Si","Si","No")

ulti2<-ifelse(ulti2>2019,"Si","No")

datos$ulti<-ulti

datos$ulti2<-ulti2

datos$Virica<-mening$Virica

datos$Año<-as.factor(datos$Año)

datos$PCR2<-as.factor(datos$PCR2)

datos$ulti<-as.factor(datos$ulti)

datos$ulti2<-as.factor(datos$ulti2)

datos$Virica<-as.factor(datos$Virica)


data<-cbind(mening,datos)


Time<-as.numeric(mening$tiempo/24)
Muertos<-as.numeric(mening$Muerte_atribuible)
surv<-Surv(Time,Muertos)
coxph(surv~cambiodat,data=mening)

#t.test(mening$diagetiol,mening$PCR2)
pcrsi    <- datos %>% filter(PCR2 == "Si") %>% pull(diagetiol)
pcrno    <- datos %>% filter(PCR2 == "No") %>% pull(diagetiol)
t.test(
  x           = pcrsi,
  y           = pcrno,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)
boxplot(mening$lcr)
boxplot(data$rapida)
plot(data$Año, data$lcr)
