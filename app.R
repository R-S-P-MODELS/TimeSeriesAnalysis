#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#require(forecast)
require(plotly)
require(pracma)




# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Application title
  titlePanel("Time Series Properties"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "Arquivo",label = "Insira sua serie temporal como vetor",accept = c("*.csv","*.txt"),placeholder = "Aguardando a matriz" ),
      conditionalPanel(
        condition = "input.Referencias == 'VizualizaçõesVetoriais'",
        selectInput("Opcoes",label = "Métrica a se observar",choices = c('Grafico','Coeficiente de Hurst','FFT Amplitude','FFT Phase Information','MAF',"LogModel","Trend","Random","Seasonality","Differential","Differential Hurst"))),
      
      
      conditionalPanel(
        condition = "input.Referencias == 'VizualizaçõesMatriciais'",
        selectInput("OpcoesM",label = "Métrica a se observar",choices = c('Distances','Similarity'))),
      
      conditionalPanel(
        condition = "input.Opcoes == 'MAF'",
        numericInput("AVG",label = "Tamanho da janela",min =1,max=100,value=2 )),
      
      conditionalPanel(
        condition="input.Opcoes=='Trend' | input.Opcoes=='Random' | input.Opcoes=='Seasonality' ",
        numericInput("Frequencia",label="Frequencia da Serie temporal",min=2,max=100,value=2)
      )
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "Referencias",
                  
                  #  tabPanel("InformacoesNumericas",tableOutput("InformacoesNumericasTabulares")),
                  tabPanel("VizualizaçõesVetoriais", plotlyOutput("Vetoriais"))
                  #  tabPanel("Forecast",plotlyOutput("Forecasting"))
                  # tabPanel("VizualizaçõesMatriciais",plotlyOutput("Matriciais")),
                  #  tabPanel("Comunidades",plotOutput("Comunidade"))
      ),
      
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      h3("Aplicativo Desenvolvido por Rafael Silva Pereira"),
      h3("Email de contato r.s.p.models@gmail.com")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=10000000*1024^2)
  
  
  leitura<-reactive({
    if(!is.null(input$Arquivo)){
      #require(data.table)
      Serie=scan(input$Arquivo$datapath)
      return(Serie)
    }
  })
  
  output$Forecasting<-renderPlotly({
    if(!is.null(input$Arquivo)){
      Serie=leitura()
      
    }
    
  })
  
  output$Vetoriais<-renderPlotly({
    if(!is.null(input$Arquivo)){
      #require(data.table)
      Serie=leitura()
      
      if(input$Opcoes=="Grafico")
        Objetivo=data.frame(1:length(Serie),Serie)
      if(input$Opcoes=="Coeficiente de Hurst"){
        coef=hurstexp(Serie, d = 8, display = FALSE)
        Objetivo=data.frame(1:length(coef),coef)
        
      }
      if(input$Opcoes=="FFT Amplitude"){
        transf=fft(Serie)
        Amplitude= Re(transf)*Re(transf) + Im(transf) * (-Im(transf))
        Objetivo=data.frame(1:length(Amplitude),Amplitude)
      }
      if(input$Opcoes=="FFT Phase Information"){
        transf=fft(Serie)
        Phase= atan(Im(transf)/Re(transf))
        Objetivo=data.frame(1:length(Phase),Phase)
      }
      if(input$Opcoes=="MAF"){
        Average=movavg(Serie, input$AVG, type="s")
        Objetivo=data.frame(1:length(Average),Average)
        
      }
      if(input$Opcoes=="LogModel"){
        LogModel=log(Serie)
        Objetivo=data.frame(1:length(LogModel),LogModel)
      }
      if(input$Opcoes=="Trend"){
        #  Series=ts(Serie,frequency = as.numeric(input$Frequencia) )
        # print(Series)
        Decomposicao=stats::decompose(ts(Serie,frequency=input$Frequencia))
        # print(Decomposicao)
        Objetivo=data.frame(1:(length(Decomposicao$trend)-2),Decomposicao$trend[2:(length(Decomposicao$trend)-1)]  )
        
        
      }
      if(input$Opcoes=="Random"){
        #Series=ts(Serie,frequency = as.numeric(input$Frequencia))
        Decomposicao=stats::decompose(ts(Serie,frequency=input$Frequencia))
        Objetivo=data.frame(1:(length(Decomposicao$random)-2),Decomposicao$random[2:(length(Decomposicao$random)-1)]  )
        
        
      }
      if(input$Opcoes=="Differential"){
        Derivative=diff(Serie)
        Objetivo=data.frame(1:length(Derivative),Derivative)
        
      }
      
      if(input$Opcoes=="Differential Hurst"){
        Derivative=diff(Serie)
        coef=hurstexp(Derivative, d = 8, display = FALSE)
        Objetivo=data.frame(1:length(coef),coef)
        
        
      }
      if(input$Opcoes=="Seasonality"){
        #  Series=ts(Serie,frequency = as.numeric(input$Frequencia) )
        # print(Series)
        Decomposicao=stats::decompose(ts(Serie,frequency=input$Frequencia))
        Sasonalidade=Serie-Decomposicao$trend
        Sasonalidade=matrix(Sasonalidade,nrow=input$Frequencia)
        Sasonalidade=colMeans(Sasonalidade)
        # print(Decomposicao)
        Objetivo=data.frame(1:length(Sasonalidade),Sasonalidade  )
        
        
      }
      
      
      # este será uma vizualização gráfica dos elementos que retornam como vetores
      names(Objetivo)=c('objeto','valor')
      ggplotly(ggplot(data=Objetivo,aes(x=objeto,y=valor)) +geom_line()   )
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)