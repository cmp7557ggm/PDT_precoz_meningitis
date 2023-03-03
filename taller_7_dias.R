library(dplyr)
library(survival)
library(knitr)
library(knitr)
library(dplyr)
library(gridExtra)
library(openintro)
library(plotrix)
library(formattable)
library(MASS)
library(ggplot2)
library(plotly)
library(readxl)
library(pillar)
library(kableExtra)

mening <- read_excel("plantilla_meningitis.xlsx")
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

mening2<-dplyr::select(mening,c(id,Edad,Sexo,Charlson_Index,charlson_Index3, Glasgow,Diagnostico_sindromico2,TABP, T_em_combinado,
                                T_em_mixto,ED_0, Actividad0,PCR2,ED_1,DOORMAT1,Provisional,T_dirigido2_combinado,
                                T_dirigido2_mixto, ED_2,DOORMAT2,Microorganismo , cul_pos,Virica,T_dirigido3_combinado,
                                T_dirigido3_mixto,ED_3, DOORMAT3,UCI, Muerte_atribuible,Cambio,lcr24,emp24,rapida24,recepcion24,
                                final24, cambiodat24,diagetiol24, alta24, Año, ulti2))
glimpse(mening2)
tiempo7<-ifelse(mening2$alta24>7,7,mening2$alta24)
mening2$tiempo7<-tiempo7
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
mening2<-dplyr::select(mening2,-c(8,18,24,32,41,42,46,47,49,50))
glimpse(mening2) 
##########################################
meningRF<-dplyr::select(mening2,-c(id,alta24))
glimpse(meningRF)

meningRF<-dplyr::select(meningRF,-c(T_dirigido3_combinado,T_dirigido2_combinado,T_em_mixto,T_em_combinado,
                                    Actividad0 ))

meningRF = meningRF [ , c(29,19,1:18,20:28,30:34)]
glimpse(meningRF)

df<-meningRF
ccc<-combinar(df,c(3:31),1)

#mRF_breve$ccc<-ccc
d<-Mconf(df,ccc)
d<-as.data.frame(d)
d$Coeficiente<-as.numeric(d$Coeficiente)
d$p_valor<-as.numeric(d$p_valor)
seleccion1<-dplyr::filter(d,p_valor<0.1)
seleccion1%>%
  kbl()%>%
  kable_paper("striped", full_width = F)

c1<-coxph(Surv(tiempo7,Muerte_atribuible)~Cambio+Glasgow+DOORMAT1+diagetiol24,data=meningRF)
s1<-summary(c1)
s1

############## VIMP RANDOM FOREST ##############################
tune(Surv(tiempo7,Muerte_atribuible )~.,meningRF,doBest = TRUE)
mod1<-rfsrc(Surv(tiempo7,Muerte_atribuible )~.,data=meningRF,ntree = 1000, block.size = 1, nodesize = 1, mtry=14 ,importance = T)
mod1
#tune.rfsrc(Surv(alta24,Muerte_atribuible )~.,data=mRF_breve)
imperm<-data.frame(mod1$importance)
imperm<-arrange(imperm,desc(mod1.importance))
imperm%>%
  kbl()%>%
  kable_styling(full_width = F)
oo <- subsample(mod1, verbose = FALSE)
# take a delete-d-jackknife procedure for example
vimpCI <- extract.subsample(oo)$var.jk.sel.Z
vimpCI<-as.data.frame(vimpCI)
vimpCI_pos<-dplyr::filter(vimpCI,signif==TRUE)
vimpCI_pos<-dplyr::arrange(vimpCI_pos,desc(mean))
vimpCI_pos
# Confidence Intervals for VIMP
plot.subsample(oo)

vimpCI <- extract.subsample(oo)$var.jk.sel.Z
vimpCI<-as.data.frame(vimpCI)
vimpCI_pos<-dplyr::filter(vimpCI,signif==TRUE)
vimpCI_pos<-dplyr::arrange(vimpCI_pos,desc(mean))
vimpCI_pos%>%
  kbl()%>%
  kable_styling(full_width = F)

meningRF2<-dplyr::select(meningRF,tiempo7,Muerte_atribuible,Glasgow,Cambio,cambiodat24,DOORMAT1,Edad,final24,
                         recepcion24,Año,Virica, Charlson_Index,diagetiol24)

tune(Surv(tiempo7,Muerte_atribuible )~.,meningRF2,doBest = TRUE)
mod2<-rfsrc(Surv(tiempo7,Muerte_atribuible )~.,data=meningRF2,ntree = 1000, block.size = 1, nodesize = 1, mtry=6 ,importance = T)
mod2

plot.variable(mod2, partial=T,plots.per.page = 1, xvar.names = c("Glasgow","Cambio","cambiodat24","DOORMAT1","Edad","final24",
                                                                 "recepcion24","Año","Virica", "Charlson_Index","diagetiol24"),
              notch=FALSE,
              surv.type = "surv",
              time=1)
summary(meningRF2$cambiodat24)


