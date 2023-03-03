shinyUI(
  navbarPage("Importancia y gráficos parciales en meningitis ",
             theme = shinythemes::shinytheme("flatly"),
             
             tabPanel("Importancia",
                      tabsetPanel(
                        
                        tabPanel("Importancia por permutación",
                                 column(7,offset = 1,
                                        br(),
                                        h4(textOutput("titulin2")),
                                        plotOutput("gvimp")),
                                 column(4,
                                        br(),
                                        h4(textOutput("titulin3")),
                                        tableOutput("tvimp")
                                 )
                        ),
                        tabPanel("VIMP con IC",
                                 column(7,offset = 1,
                                        br(),
                                        h4(textOutput("titulin2b")),
                                        plotOutput("gvimpb",width = "100%",
                                                   height = "600px")),
                                 column(4,
                                        br(),
                                        h4(textOutput("titulin3b")),
                                        tableOutput("tvimpb")
                                 )
                        ),
                        tabPanel("Importancia por profundidad mínima",
                                 column(6,offset = 1,
                                        br(),
                                        h4(textOutput("titulin4")),
                                        plotOutput("gpm")),
                                 column(5,
                                        br(),
                                        h4(textOutput("titulin5")),
                                        verbatimTextOutput("tpm")
                                 )
                        ),
                        tabPanel("Comparativa importancias por los dos métodos",
                                 column(7,offset = 2,
                                        br(),
                                        h4(textOutput("titulin6")),
                                        plotOutput("comp"))
                        )
                        
                      )
             ),
             tabPanel("Gráficos",
                      
             tabPanel("Gráficos marginales y de efectos parciales",
                      sidebarPanel(
                        #uiOutput("var1"),
                        #br(),br(),
                        uiOutput("var2"),
                        br(),br(),
                        uiOutput("tipo"),
                        conditionalPanel(condition = "input.tipo=='Superviencia en un tiempo'",
                                         sliderInput("tiempos","Seleccionar tiempo para estimar supervivencia",
                                                     min=0,max=30,value = 4,
                                                     animate =animationOptions(interval = 1000, loop = F)))
                        
                        
                      ),
                      mainPanel(
                        column(10,
                               h4(textOutput("textgraf")),
                               plotOutput("parcial"),
                               
                               h4(textOutput("textgraf2")),
                               plotOutput("marginal")
                        )
                      )
             )
  )
  ))