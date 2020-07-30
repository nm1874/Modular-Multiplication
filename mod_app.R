#Collaborators Jenna, Roger, Kris


#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(flextable)
library(DT)
library(rsconnect)
library(lattice)

source('modcalc.R')
nproposed <- 7
n <- 7
chartcolor <- 'pink'
linewidth <-1


#The user interface
header <- dashboardHeader(title = "Modular Multiplication")

sidebar <- dashboardSidebar(
  disable = FALSE,
  h3('Choose n'),
  numericInput(inputId = 'nnumeric', label='', value = n),
  uiOutput(outputId = 'result'),
 
  
  textInput('', label=h3(''), value='')
  
)




body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      h3("Multiplication Table"),
      tableOutput(outputId = 'multiplicationTable')
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3('Order of elements'),
      plotOutput(outputId = 'distPlot'),
      hr(),
      uiOutput('checking'),
      hr(),
      uiOutput('textout')
    )
  )
)


ui <- dashboardPage(header, sidebar, body)



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  
  
  updtbl <- function(n) {
    
    v <- Mod.coprime(n)
    
    mul <- as.data.frame(cbind(v, outer(v, v, function(x, y) (x * y) %% n)))
    colnames(mul) <- c('Multiply', v)
    multi_table <- flextable(mul)
    output$multiplicationTable <- renderUI({htmltools_value(multi_table)})
    
    x<-c()
    for (i in v) x<-c(x, Mod.order(i, n))
    t <- table(x)
    output$distPlot <- renderPlot({
      barchart(t, horizontal=FALSE, xlab='order of the element',
               ylab = 'occurrences')
    })
  }
  
  updtbl(7)
  
  
  
  
  observeEvent(input$nnumeric, {
    nproposed <<- input$nnumeric
    if (nproposed>0 & is.integer(nproposed)) {
      n <<- nproposed
      output$result <-renderUI(paste(''))
      updtbl(n)
    }
    else {
      output$result <-renderUI(paste('Only positive integers'))
    }
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
