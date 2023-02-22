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
library(ggsankey)
library(circlize)
library(readxl)
library(pillar)
library(kableExtra)
library(survival)
library(VSURF)
library(KMsurv)
library(randomForestSRC)
library(lubridate)
library(nlme)
library(fastDummies)
library(caret)

mening <- read_xlsx("/Users/carlosmartinperez/Desktop/MENINGITIS/plantilla_meningitis.xlsx")
#fechas<- read_xlsx("/Users/carlosmartinperez/Desktop/MENINGITIS/Meningitis pre-intervención.xlsx",sheet=4)

lcr<-as.duration(as.period(interval(mening$Fecha_h_meningitis , mening$Fecha_h_LCR)))
lcr<-ifelse(lcr<0,0,lcr)
lcr<-round(lcr/3600,1)
mening$lcr<-lcr
mening$lcr24<-mening$lcr/24

emp<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_empirico)))
emp<-ifelse(emp<0,0,emp)
emp<-round(emp/3600,1)
mening$emp<-emp
mening$emp24<-mening$emp/24

dirigido1<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_dirigido1)))
dirigido1<-ifelse(dirigido1<0,0,dirigido1)
dirigido1<-round(dirigido1/3600,1)
mening$dirigido1<-dirigido1
mening$dirigido124<-mening$dirigido1/24

dirigido2<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_dirigido2)))
dirigido2<-ifelse(dirigido2<0,0,dirigido2)
dirigido2<-round(dirigido2/3600,1)
mening$dirigido2<-dirigido2
mening$dirigido224<-mening$dirigido2/24

dirigido3<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_dirigido3)))
dirigido3<-ifelse(dirigido3<0,0,dirigido3)
dirigido3<-round(dirigido3/3600,1)
mening$dirigido3<-dirigido3
mening$dirigido324<-mening$dirigido3/24

rapidat<-as.duration(as.period(interval(mening$Fecha_h_rapida , mening$Fecha_h_dirigido1)))
rapidat<-ifelse(rapidat<0,0,rapidat)
rapidat<-round(rapidat/3600,1)
mening$rapidat<-rapidat
mening$rapidat<-mening$rapidat/24

rapida<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_rapida)))
rapida<-ifelse(rapida<0,0,rapida)
rapida<-round(rapida/3600,1)
mening$rapida<-rapida
mening$rapida24<-mening$rapida/24

provi<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_provi)))
provi<-ifelse(provi<0,0,provi)
provi<-round(provi/3600,1)
mening$provi<-provi
mening$provi24<-mening$provi/24

recepcion<-as.duration(as.period(interval(mening$Fecha_h_meningitis , mening$Fecha_h_recepcion)))
recepcion<-ifelse(recepcion<0,0,recepcion)
recepcion<-round(recepcion/3600,1)
mening$recepcion<-recepcion
mening$recepcion24<-mening$recepcion/24

final<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_h_resultado_final)))
final<-ifelse(final<0,0,final)
final<-round(final/3600,1)
mening$final<-final
mening$final24<-mening$final/24

cambiodat<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$Fecha_cambio)))
cambiodat<-ifelse(cambiodat<0,0,cambiodat)
cambiodat<-round(cambiodat/3600,1)
mening$cambiodat<-cambiodat
mening$cambiodat24<-mening$cambiodat/24

diagetiol<-as.duration(as.period(interval(mening$Fecha_h_LCR , mening$fecha_diag_etiol)))
diagetiol<-ifelse(diagetiol<0,0,diagetiol)
diagetiol<-round(diagetiol/3600,1)
mening$diagetiol<-diagetiol
mening$diagetiol24<-mening$diagetiol/24

mening$tiempo<-as.duration(as.period(interval(mening$Fecha_h_meningitis , mening$Fecha_h_alta)))
mening$tiempo<-ifelse(mening$tiempo<0,0,mening$tiempo)
mening$tiempo<-round(mening$tiempo/3600,1)

mening$alta24<-mening$tiempo/24
ulti2<-mening$Año
ulti2<-ifelse(ulti2>2019,1,0)
mening$ulti2<-ulti2
glimpse(mening)

summary(mening)

Time<-mening$tiempo/24
recepcion24<-mening$recepcion/24

glimpse(mening)

mening2<-dplyr::select(mening,c(id,Edad,Sexo,Charlson_Index,Glasgow,Diagnostico_sindromico2,TABP, T_em_combinado,
                                T_em_mixto,ED_0, Actividad0,PCR2,ED_1,DOORMAT1,Provisional,T_dirigido2_combinado,
                                T_dirigido2_mixto, ED_2,DOORMAT2,Microorganismo , cul_pos,Virica,T_dirigido3_combinado,
                                T_dirigido3_mixto,ED_3, DOORMAT3,UCI, Muerte_atribuible,Cambio,lcr24,emp24,rapida24,recepcion24,
                                final24, cambiodat24,diagetiol24, alta24, Año, ulti2))
glimpse(mening2)
############ CRECION DE DUMMYS PARA MICROORGANISMO #################################3
mening2<-dummy_cols(mening2,  select_columns = c( "Microorganismo"),remove_selected_columns = TRUE)
glimpse(mening2)  
######################3

############# ELIMINCION DE VARIABLES CON VARIANZA PROXIMA A CERO ##################
zv1<-mening2 %>%  nearZeroVar(saveMetrics = TRUE)

tablezv <- data.frame(zv1)
tablezv<-dplyr::select(tablezv,c(nzv))
tablezv$n<-seq(1:dim(tablezv)[1])
tablezv<-dplyr::filter(tablezv,nzv=="TRUE")
tablezv
mening2<-dplyr::select(mening2,-c(7,17,23,31,39,40,44,45,47,48))
glimpse(mening2) 
##########################################

######### SUBSAMPLES VIVOS Y MENINGRF ###################3
vivos<-dplyr::filter(mening2,Muerte_atribuible==0)
glimpse(vivos)

meningRF<-dplyr::select(mening2,-c(id))
glimpse(meningRF)

##################################################
#Estudiamos correlación entre variables 
##################################################

library(PerformanceAnalytics)
library(tidyverse)

dfc<-dplyr::select(meningRF,lcr24,emp24,recepcion24,final24,cambiodat24,diagetiol24)
dfc %>% 
select_if(is.numeric)
chart.Correlation(dfc, histogram = F, method = "pearson")

dfc2<-dplyr::select(meningRF,ED_0,ED_1,ED_2,ED_3,DOORMAT1,DOORMAT2,DOORMAT3)
dfc2 %>% 
  select_if(is.numeric)
chart.Correlation(dfc2, histogram = F, method = "pearson")

####### Eliminamos correlacionadas
meningRF<-dplyr::select(meningRF,-c(lcr24,final24,cambiodat24))
glimpse(meningRF)
########### BD sin tratamientos combinados y mixtos
mRF_breve<-dplyr::select(meningRF,-c(T_dirigido3_combinado,T_dirigido2_combinado,T_em_mixto,T_em_combinado))
glimpse(mRF_breve)




###############################################################33
ggplot(mening,aes(x=id , y = Time)) +
  geom_linerange(aes(ymin = 0, ymax = Time), lwd=0.2) +
  geom_point(aes( color=factor(Muerte_atribuible)), size=2) +
  geom_point(aes( x=id, y = lcr24), size=1,shape=13 ) +
  geom_point(aes( x=id, y = emp24), size=1,colour="peru",shape=13) +
  geom_point(aes( x=id, y = diagetiol24), size=1,colour="green",shape=13) +
  #ylim(0,4)+
  labs(y = "tiempo (Días)", x = "Paciente nº") +
  coord_flip() +
  scale_color_discrete(name="Tiempo hasta evento", breaks=c("0", "1"),
                       labels=c("Censura", "Fallecido meningitis")) + theme(legend.position="left") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10),
        legend.text=element_text(size=7), legend.title=element_text(size=8))





############################3
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
glimpse(datos)
#######################################################
