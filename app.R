options(encoding = "UTF-8")

library(shiny)
library(data.table)
library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(tibble)
library(readr)
library(shinyjs)
library(DT)
library(bs4Dash)
library(waiter)
library(httr)
library(ggplot2)
library(glue)
library(showtext)

showtext_auto(enable = TRUE)


lapply(
  list.files("module"),
  FUN = function(x)
    source(paste0("module/", x))
)


library(shiny)

shinyjs::useShinyjs()


ui <-
  dashboardPage(
    # preloader = list(html = tagList(spin_1(), h4("哈哈等久吧傻子")), color = "#3c8dbc"),
    dashboardHeader(title = "Tidyverse",skin = "light"),
    dashboardSidebar(
      minified = FALSE,
      skin = "light",
      sidebarMenu(
        menuItem("tibble教學與適用", tabName = "上傳你的data"),
        menuItem("tidyr教學與試用", tabName = "tidyr教學"),
        menuItem("ggplot教學與試用", tabName = "ggplot教學")
      ),
      fileInput("data", "請提供你的數據", accept = ".csv"),
      actionButton("upload", "上傳"),
      selectInput("data_choice","選擇資料", choices = NULL),
      actionButton("data_action", "確認")
    ),
    controlbar = dashboardControlbar(
      actionButton("submit", "Submit"),
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "上傳你的data",
                fluidRow(
                box( width = 12,
                     DT::dataTableOutput("output",height = "50%"))),
                fluidRow(
                  column(
                    width = 12,
                    h4("glimpse"),
                    verbatimTextOutput("glimpse")
                  )
                )
                
        ),
        tabItem(tabName = "tidyr教學",
                tidyr_ui("tidyr")),
        tabItem(tabName = "ggplot教學",
                ggplot_ui("ggplot"))
      )
    )
  )


server <- function(input, output, session) {
  
  #dataList
  dataList <- reactiveVal(list("iris" = iris, "mtcars" = mtcars))
  
  
  observe(
    updateSelectInput(
      session = session,
      inputId = "data_choice",
      choices = names(dataList())
    )
  )
  
  observeEvent(input$data, {
    shinyjs::alert("Thank you!")
  })
  
  data <- reactive({
    req(input$data)
    fread(input$data$datapath)
  })
  
  output$output <- DT::renderDataTable(
    dataList()[[input$data_choice]],
    editable = TRUE
  )
  
  observeEvent(input$data_action, {
    
    ggplot_server("ggplot",dataList()[[input$data_choice]])
    
    tidyr_server("tidyr",dataList()[[input$data_choice]])
    
  })
  
  upload_confirm <- function(){
    
    modalDialog(
      "確認新增資料？",
      title = "確認新增",
      textInput("data_name", "輸入資料名稱"),
      DT::dataTableOutput("view_data",height = "50%"),
      footer = tagList(
        actionButton("upload_cancel", "Cancel"),
        actionButton("upload_ok", "OK")
      )
    )
    
  }
  
  observeEvent(input$upload, {
    showModal(upload_confirm())
    
    output$view_data <- DT::renderDataTable(
      {data()}
    )
  })
  
  observeEvent(input$upload_ok, {
    req(input$data_name)
    
    current_data <- dataList()
    current_data[[input$data_name]] <- data()
    dataList(current_data)
    
    if(input$data_name %in% names(dataList())){
      shinyjs::alert("新增成功")
    }else{
      shinyjs::alert("新增失敗")
    }
    
    removeModal()
    
  })
  
  observeEvent(input$upload_cancel, {
    removeModal()
  })
  
  
  output$glimpse <- renderPrint({
    req(input$data_choice)
    glimpse(dataList()[[input$data_choice]])
  })
  
  
  
}

shinyApp(ui = ui, server = server)













