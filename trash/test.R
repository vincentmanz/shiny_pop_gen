## load libraries
library(shiny)
library(stringr)
library(scatterD3)

#source("/Users/echang/scratch/tmp/MSD_D3scatter/csvFile_Module.R")
csvFileInput <- function(id, label="CSV file") {
  ## Create namespace
  ns<-NS(id)
  tagList(
    uiOutput(ns("controls"))
  )
}

csvFileControl <- function(id){
  ns <- NS(id)
  tagList(
    column(width=3, uiOutput(ns("ColName"))),
    column(width=3, uiOutput(ns("ColEntry")))
  )
}

csvFileUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("csvTable"))
  )
}

## server module
csvFile <- function(input, output, session, stringsAsFactors) {
  ns <- session$ns
  ## to reuse namespace, session must be first!!!
  
  ## User selected file
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  dataframe <- reactive({
    read.csv(
      userFile()$datapath,
      header = input$header,
      sep=input$sep,
      quote = input$quote,
      stringsAsFactors = stringsAsFactors
    )
  })
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  xvar <- reactive({input[[ "xvar" ]] })
  yvar <- reactive({input[[ "yvar" ]] })
  
  output$controls <- renderUI({
    ## use taglist to keep everything together
    tagList(
      fileInput(ns('file'), 'Choose CSV file', 
                accept=c('txt/csv','text/comma-separated-values,text/plain','.csv')),
      checkboxInput(ns('header'), 'Has heading', TRUE),
      radioButtons(ns('sep'),'Separator', c(Comma=',',Semicolon=';',Tab='\t'), ','),
      selectInput(ns('quote'),'Quote', c(None ='','Double Quote'='"','Single Quote'="'"),'"')
    )
  })
  
  ## use renderUI to display table
  output$csvTable <- renderUI({
    output$table <- renderDataTable(dataframe())
    dataTableOutput(ns("table"))
  })
  
  ## Column Name
  output$ColName <- renderUI({
    df <- dataframe()
    if (is.null(df)) return(NULL)
    items=names(df)
    print(items)
    names(items)=items
    tagList(
      selectInput(ns("xvar"), "Column Names", items),
      selectInput(ns("yvar"), "Column Names", items)
    )
  })
  
  ## Column Entry
  output$ColEntry <- renderUI({
    df <- dataframe()
    if (is.null(input$col)) return(NULL)
    tagList(
      selectInput(ns("entry"), "Entry Names", df[,input$xvar])
    )
  })
  
  rlist <- list(dataframe=dataframe,xvar=xvar,yvar=yvar)
  # Return the reactive that yields the data frame
  return(rlist)
  
}## End of module


## scatterD3 module -------------------------------------------------------------

D3scatterUI <- function(id){
  ns<-NS(id)
  tagList(
    scatterD3Output(ns("scatterplot1"))
  )
}

D3scatter <- function(input,output,session,rlist){
  ns <- session$ns
  
  output$scatterplot1 <- renderScatterD3({
    #scatterD3(data = data, x=mpg, y=carb,
    mtdf <- rlist$dataframe()
    x <- mtdf[[rlist$xvar()]]
    y <- mtdf[[rlist$yvar()]]
    scatterD3(x=x,y=y,
              labels_size= 9, point_opacity = 1,
              #col_var=cyl, symbol_var= data$Assay,
              #lab= paste(mpg, carb, sep="|") , lasso=TRUE,
              #xlab= "IFN-γ", ylab= "IL-10",
              #click_callback = "function(id, index) {
              #  alert('scatterplot ID: ' + id + ' - Point index: ' + index) 
              #  }", 
              transitions= T)
  })
}


## Shiny ######################################################################
ui <- fluidPage(
  titlePanel("Upload"),
  
  tabsetPanel(type="tabs",
              tabPanel("tab1",
                       sidebarLayout(
                         sidebarPanel(csvFileInput("basic")),
                         mainPanel(csvFileUI("basic"))
                       )
              ),
              tabPanel("tab2",
                       tagList(
                         fluidRow(csvFileControl("basic")),
                         fluidRow(D3scatterUI("first"))
                       )
              )
  )
)

server <- function(input, output, session) {
  ## Option 1. CSV uploaded file
  rlist <- callModule(csvFile, "basic", stringsAsFactors = FALSE) 
  
  ## Option 2. mtcar data loaded at start
  #datafile <- reactive({mtcars}) ## data loaded at runApp()
  #callModule(csvFile, "basic") 
  
  callModule(D3scatter, "first", rlist)
  
}

shinyApp(ui, server)