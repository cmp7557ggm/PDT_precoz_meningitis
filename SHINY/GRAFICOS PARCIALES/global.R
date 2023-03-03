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
library(ggRandomForests)
library(fastDummies)
library(caret)
library(parallel)

mening <- read_xlsx("/Users/carlosmartinperez/Desktop/MENINGITIS/plantilla_meningitis.xlsx")

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

Time<-mening$tiempo/24
recepcion24<-mening$recepcion/24

mening2<-dplyr::select(mening,c(id,Edad,Sexo,Charlson_Index,charlson_Index3, Glasgow,Diagnostico_sindromico2,TABP, T_em_combinado,
                                T_em_mixto,ED_0, Actividad0,PCR2,ED_1,DOORMAT1,Provisional,T_dirigido2_combinado,
                                T_dirigido2_mixto, ED_2,DOORMAT2,Microorganismo , cul_pos,Virica,T_dirigido3_combinado,
                                T_dirigido3_mixto,ED_3, DOORMAT3,UCI, Muerte_atribuible,Cambio,lcr24,emp24,rapida24,recepcion24,
                                final24, cambiodat24,diagetiol24, alta24, Año, ulti2))
############ CRECION DE DUMMYS PARA MICROORGANISMO #################################
mening2<-dummy_cols(mening2,  select_columns = c( "Microorganismo"),remove_selected_columns = TRUE)
############# ELIMINACION DE VARIABLES CON VARIANZA PROXIMA A CERO ##################
mening2<-dplyr::select(mening2,-c(8,18,24,32,40,41,45,46,48,49))
######### SUBSAMPLES VIVOS Y MENINGRF ###################3
vivos<-dplyr::filter(mening2,Muerte_atribuible==0)

meningRF<-dplyr::select(mening2,-c(id))
########### BD sin tratamientos combinados y mixtos
mRF_breve<-dplyr::select(meningRF,-c(T_dirigido3_combinado,T_dirigido2_combinado,T_em_mixto,T_em_combinado))
########### BD con tiempo y evento en las dos primeras columnas
mRF_breve = mRF_breve [ , c(28,20,1:19,21:27,29:35)]
mRF_breve$alta24<-as.numeric(mRF_breve$alta24)
mRF_breve<-dplyr::select(mRF_breve,-c(ED_1,ED_2,ED_3,DOORMAT2,DOORMAT3,
                                      recepcion24, diagetiol24, lcr24))

###########################################################################################
#Algoritmos para obtener todos los modelos posibles con su correspondiente p valor
#
df<-mRF_breve
glimpse(df)

#_____funcion primera
data_select<-function(df,Yvar,Xvar=NULL){
  df<-df[,c(Yvar,Xvar)]
  Xvar2<-2:(length(Xvar)+1)
  return(df)
}

#_____funcion segunda (combinaciones tomandas de n en n)
combinar<-function(df,Xvar=NULL,numXVar=1){
  
  comb_Var<-t(combn(Xvar,numXVar));combinaciones<-comb_Var
  
  colnames(combinaciones)<-NULL
  
  options(warn=-1)
  
  get_names<-function(df,x){
    outcome<-matrix(data=NA,nrow=nrow(x),ncol=ncol(x))
    for(i in 1:nrow(x)){
      outcome[i,1:ncol(x)]<-colnames(df)[x[i,1:ncol(x)]]
    }
    return(outcome)
  }
  combn_names<-get_names(df,combinaciones)
  return(combn_names)
}


Mconf<-function(df, x)
{
  filas<-nrow(ccc)
  materr2<-matrix(nrow=filas,ncol=3)
  colnames(materr2)<-c( "Variables", "Coeficiente","p_valor")
  rownames(materr2)<-paste("Modelo",c(1:filas))
  
  for (i in 1:nrow(ccc)){
    xnam <- as.matrix(ccc[i,])
    #fmla <- as.formula(paste(surv,"~", paste(xnam)))
    fmla <- as.formula(paste("Surv(",colnames(df[1]),",",colnames(df[2]),") ~", paste(xnam)))
    mod <- coxph(fmla, data = df)
    
    fmla<-as.character(fmla)
    coeficiente<-summary(mod)$coefficients[1]
    pv<-summary(mod)$coefficients[5]
    
    materr2[i,1]<-nrow(xnam)
    materr2[i,1]<-fmla[3]
    materr2[i,2]<-round(coeficiente,2)
    materr2[i,3]<-round(pv,4)
    
  }
  return(materr2)
}

############## VIMP RANDOM FOREST ##############################
mod1<-rfsrc(Surv(alta24,Muerte_atribuible )~.,data=mRF_breve,ntree = 1000, block.size = 1, nodesize = 1, mtry=22 ,importance = T)
mod1
#tune.rfsrc(Surv(alta24,Muerte_atribuible )~.,data=mRF_breve)
imperm<-data.frame(mod1$importance)
imperm<-arrange(imperm,desc(mod1.importance))
############## PARA GRAFICO VIMP CON INTERVALOS DE CONFIANAZA ##############################
oo <- subsample(mod1, verbose = FALSE)
# take a delete-d-jackknife procedure for example
vimpCI <- extract.subsample(oo)$var.jk.sel.Z
vimpCI<-as.data.frame(vimpCI)
vimpCI_pos<-dplyr::filter(vimpCI,signif==TRUE)
vimpCI_pos<-dplyr::arrange(vimpCI_pos,desc(mean))

