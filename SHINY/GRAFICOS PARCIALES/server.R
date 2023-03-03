shinyServer(function(input, output) {
  
  
  
  
  output$titulin2=renderText({
    paste("Importancia por permutación de las variables " )
  })
  output$titulin2b=renderText({
    paste("Importancia por permutación con intervalos de confianza " )
  })
  output$titulin3=renderText({
    paste("Variables con VIMP > 0")
  })
  output$titulin3b=renderText({
    paste("Variables con VIMP significativo")
  })
  output$titulin4=renderText({
    paste("Clasificacion según profundidad mínima" )
  })
  output$titulin5=renderText({
    paste("Variables top según profundidad mínima" )
  })
  output$titulin6=renderText({
    paste("Comparando VIMP y Profundidad mínima" )
  })
  
  
  output$gvimp=renderPlot({ 
    plot(gg_vimp(mod1))
  })
  output$gvimpb=renderPlot({ 
    oo <- subsample(mod1, verbose = FALSE)
    plot.subsample(oo)
  })
  
  VIMP<-reactive({
    imperm<-as.data.frame(mod1$importance)
    colnames(imperm)<-c("vimp")
    imperm$variable<-rownames(imperm)
    imperm = imperm [ , c(2,1)]
    VIMP<-arrange(imperm,desc(vimp))
    
  })
  
  output$tvimp=renderTable({ 
    imperm<-dplyr::filter(VIMP(),vimp>0)
    imperm
     
  })
  output$tvimpb=renderTable({ 
    oo <- subsample(mod1, verbose = FALSE)
    # take a delete-d-jackknife procedure for example
    vimpCI <- extract.subsample(oo)$var.jk.sel.Z
    vimpCI<-as.data.frame(vimpCI)
    vimpCI$Variable<-rownames(vimpCI)
    vimpCI_pos<-dplyr::filter(vimpCI,signif==TRUE)
    vimpCI_pos<-dplyr::arrange(vimpCI_pos,desc(mean))
    vimpCI_pos
  })
  
  pmin<-reactive({
    pmin<-var.select(mod1, conservative = "medium")
  })
  output$gpm=renderPlot({ 
    gg_pmin<-gg_minimal_depth(pmin())
    plot(gg_pmin)
  })
  output$tpm=renderPrint({ 
    gg_pmin<-gg_minimal_depth(pmin())
    print(gg_pmin)
  })
  output$comp=renderPlot({ 
    gg_pmin<-gg_minimal_depth(pmin())
    plot (gg_minimal_vimp (gg_pmin))+ 
      theme (legend.position = c (0.8, 0.2)) 
  })

  
  mf1<-reactive({
    
    fmla <- as.formula(paste("Surv( tiempo,Muerte_atribuible) ~", paste(input$var,collapse = "+")))
    set.seed(123)
    mf1 <- rfsrc(fmla, data =dataframe(),ntree = 500)
    mf1
  })
  output$modelomio<-renderPrint({
    mf1()
  })
  output$textomodelofin1<-renderPrint({
    paste("base de datos de meningitis")
  }) 
  output$textomodelofin2<-renderPrint({ 
    nvar<-data.frame(mf1()$xvar.names)
    nvar<-dim(nvar)[1]
    paste("Modelo con ",nvar, "variables")
  }) 
  output$textomodelofin3<-renderPrint({ 
    err<-get.cindex(mf1()$yvar[,1], mf1()$yvar[,2], mf1()$predicted.oob)
    err<-round(err,3)
    err<-err*100
    paste("Error del ",err,"%")
  })
  output$var<-renderUI({
    mod2<-rfsrc(Surv(tiempo,Muerte_atribuible )~.,data=dataframe(),ntree =500, block.size = 1,nodesize = 4,mtry = 10,
                importance = T,na.action = c( "na.impute"))
    checkboxGroupInput("var","Seleccionar las variables pra construir el modelo",
                       choices = mod2$xvar.names)
  })
  output$var1<-renderUI({
    nvar<-data.frame(mod1$xvar.names)
    nvar<-dim(nvar)[1]
    paste(nvar,"Variables CON VIMP > 0")
  })
  output$var2<-renderUI({
    #lv<-as.data.frame(mf1()$xvar.names)
    #colnames(lv)<-c("Variables:")
    #lv
    radioButtons("var2","Seleccionar variable para gráfico parcial",
                 choices = paste(c(rownames(vimpCI_pos),"final24")))
  })
  output$tipo<-renderUI({
    radioButtons("tipo","Seleccionar resultado ",
                 choices = c("Superviencia en un tiempo","Mortalidad"))
  })
  output$parcial<-renderPlot({
    if(input$tipo=="Superviencia en un tiempo"){
      plot.variable(mod1, partial=T,plots.per.page = 1, xvar.names = c(input$var2),
                    notch=F,
                    surv.type = "surv",
                    time=input$tiempos,
                    ylim=c(0.70,0.99))}
    else{
      if(input$tipo=="Mortalidad"){
        plot.variable(mod1, partial=T,plots.per.page = 1, xvar.names = c(input$var2),
                      notch=F,
                      surv.type = "mort")
      }
    }
    
  })  
  
  output$marginal<-renderPlot({
    if(input$tipo=="Superviencia en un tiempo"){
      plot.variable(mod1, partial=F,plots.per.page = 1, xvar.names = c(input$var2),
                    notch=FALSE,
                    surv.type = "surv",
                    time=input$tiempos)}
    else{
      if(input$tipo=="Mortalidad"){
        plot.variable(mod1, partial=F,plots.per.page = 1, xvar.names = c(input$var2),
                      notch=F,
                      surv.type = "mort")
      }
    }
    
  })
  output$textgraf<-renderText({
    paste("Gráfico de efectos parciales. Variable ",input$var2, "día",input$tiempos)
  })
  output$textgraf2<-renderText({
    paste("Gráfico de efectos marginales. Variable ",input$var2)
  })
  output$dia<-renderText({
    paste("Día ",input$tiempos)
  })
})