library(shiny)
library(firebase)
library(promises)
library(future)
library(data.table)
library(tidyverse)
library(shinyjs)
library(DT)
library(bs4Dash)
library(waiter)
library(httr)

# devtools::install_github("Kohze/fireData")

plan(multisession)

lapply(list.files("R"), FUN = function(x) source(paste0("R/", x)))

library(shiny)

shinyjs::useShinyjs()


ui <-
  dashboardPage(
    preloader = list(html = tagList(spin_1(), h4("哈哈等久吧傻子")), color = "#3c8dbc"),
    dashboardHeader(title = "基礎tidyr網站",skin = "light"),
    dashboardSidebar(
      minified = FALSE,
      sidebarMenu(
        menuItem("上傳你的data", tabName = "上傳你的data"),
        menuItem("Database", tabName = "Database"),
        menuItem("select應用", tabName = "select應用"),
        menuItem("filter應用", tabName = "filter應用"),
        menuItem("mutate應用", tabName = "mutate應用"),
        menuItem("group by 加 sumarise的應用",tabName = "groupby加sumarise的應用"),
        menuItem("pivot的應用",
                 menuSubItem("pivot_wider的應用",tabName = "pivot_wider的應用"),
                 menuSubItem("pivot_longer的應用",tabName = "pivot_longer"))
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "上傳你的data",
                # includeHTML("tidyr.html"),
                fluidRow(bs4Card(
                  title = "上傳csv檔案", width = 4,
                  tags$p("Ready to take the Shiny tutorial? If so"),
                  fileInput("data", "請提供你的數據", accept = ".csv")
                ),
                box( width = 8,
                     DT::dataTableOutput("output",height = "50%")))
        ),
        tabItem(tabName = "Database",
                # includeHTML("tidyr.html"),
                fluidRow(bs4Card(
                  title = "上傳csv檔案", width = 6,
                  tags$p("Ready to take the Shiny tutorial? If so"),
                  poll_ui("main")
                ),
                box(width = 6,
                    read_results_ui("results"))),
                fluidRow(
                  box(
                    width = 12, 
                    analitic_ui("analitic"))
                )),
        tabItem(tabName = "select應用",
                # box(width=12,
                #     # includeMarkdown("selecty.Rmd")
                #     ),
                fluidRow(
                  box(title= "這是用select()函數製作的功能",width=4,collapsible = FALSE,background = "gray-dark",
                      selectInput("選擇欄位", "選擇欄位", choices = character(0), multiple = TRUE)
                  ),
                  box( width = 8,collapsible = FALSE,background = "gray-dark",
                       DT::dataTableOutput("select表格"))),
                userBox(id="firstidbox",image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg")
        ),
        tabItem(tabName = "filter應用",
                # box(width=12,title = NULL,collapsed = FALSE,headerBorder = FALSE,gradient = FALSE,closable = FALSE,
                #     # includeMarkdown("filter.Rmd")
                #     ),
                fluidRow(
                  box(title= "這是用filter函數製作的功能",width=4,
                      selectInput("篩選欄位", "篩選欄位", choices = c("無", character(0)), multiple = TRUE),
                      textInput("輸入資料", "輸入資料")
                  ),
                  box( width = 8,
                       DT::dataTableOutput("篩選表格")))
        ),
        tabItem(tabName = "mutate應用",
                # box(width=12,
                #     # includeMarkdown("mutate.Rmd")
                #     ),
                fluidRow(
                  box(title= "這是用mutate函數製作的功能",width=4,
                      textInput("新增欄位", "新增欄位"),
                      textInput("輸入資料2", "輸入資料"),
                      actionButton("apply_mutate", "應用")
                  ),
                  box( width = 8,
                       DT::dataTableOutput("mutate表格")))
        ),
        tabItem(tabName = "groupby加sumarise的應用",
                # box(width=12,
                #     # includeMarkdown("group_by+summarise.Rmd")
                #     ),
                fluidRow(
                  box(title= "這是用groupby加summarise函數製作的功能",width=4,
                      selectInput("選擇欄位g", "選擇欄位", choices = character(0), multiple = TRUE),
                      actionButton("apply_group", "應用")
                  ),
                  box( width = 8,
                       DT::dataTableOutput("group表格")))
        ),
        tabItem(tabName = "pivot_wider的應用",
                fluidRow(
                  box(title= "這是用pivot_wider函數製作的功能",width=4,
                      selectInput("value欄位", "value欄位", choices = character(0)),
                      selectInput("name欄位", "name欄位", choices = character(0)),
                      actionButton("apply_pivot_w","應用pivot_wider")
                  ),
                  box(width = 8,
                      DT::dataTableOutput("pivot_w表格")))
        ),
        tabItem(tabName = "pivot_longer",
                fluidRow(
                  box(title= "這是用pivot_longer函數製作的功能",width=4,
                      selectInput("value_l欄位", "value欄位", choices = character(0),multiple = TRUE),
                      textInput("value欄名稱","value欄名稱"),
                      textInput("name欄名稱","name欄名稱"),
                      actionButton("apply_pivot_l","應用pivot_longer")
                  ),
                  box(width = 8,
                      DT::dataTableOutput("pivot_l表格")))
        )
      )
    )
  )


server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Important message",
      "a",
      footer = modalButton("可悲")
    ))
  })
  
  observeEvent(input$data, {
    shinyjs::alert("Thank you!")
  })
  
  Sys.sleep(1) 
  
  
  data <- reactive({
    req(input$data)
    fread(input$data$datapath)
  })
  
  
  output$output <- DT::renderDataTable(
    { data() },
    editable = TRUE
  )
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "選擇欄位", choices = 欄位, selected = 欄位[1])
  })
  
  output$select表格 = renderDT({
    data()%>% select(input$選擇欄位)
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "篩選欄位", choices = c("無", 欄位), selected = "無")
  })
  
  
  output$篩選表格 = renderDT({
    if (input$篩選欄位 != "無" && input$輸入資料 != "") {
      data()%>% 
        filter(.data[[input$篩選欄位]] == input$輸入資料)
    } else {
      data()
    }
  })
  
  observeEvent(input$button, {
    toggle("hello")
  })
  
  
  output$mutate表格 = renderDT({
    if (input$apply_mutate > 0 && !is.null(input$輸入資料2)) {
      data_mutate <- data() %>%
        mutate(!!input$新增欄位:= !!rlang::parse_expr(input$輸入資料2))
    } else {data_mutate <- data() }
    data_mutate
  })
  
  observe({
    欄位g <- names(data())
    updateSelectInput(session, "選擇欄位g", choices = c("無", 欄位g), selected = "無")
  })
  
  output$group表格 = renderDT({
    if (input$apply_group %% 2 == 1) {
      selected_columns <- c(input$選擇欄位g)
      data_group <- data() %>%
        group_by(!!!syms(selected_columns))%>%
        summarise(count=n())
    } else {
      data_group <- data()
    }
    data_group
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "value欄位", choices = 欄位, selected = 欄位[1])
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "name欄位", choices = 欄位, selected = 欄位[2])
  })
  
  
  
  output$pivot_w表格 = renderDT({
    if (input$apply_pivot_w %% 2 == 1) {
      data_pivot <- data() %>%
        pivot_wider(names_from = input$name欄位,values_from = input$value欄位)
    }
    else {
      data_pivot <- data()
    }
    data_pivot
  })
  
  observe({
    欄位 <- names(data())
    updateSelectInput(session, "value_l欄位", choices = 欄位, selected = 欄位[1])
  })
  
  output$pivot_l表格 = renderDT({
    if (input$apply_pivot_l %% 2 == 1) {w
      data_pivot_l <- data() %>%
        pivot_longer(c(input$value_l欄位),names_to = input$name欄名稱,values_to = input$value欄名稱)
    }
    else {
      data_pivot_l <- data()
    }
    data_pivot_l
  })
  
  poll_server("main")
  
  read_results_server("results")
  
  analitic_server("analitic")
  
}

shinyApp(ui = ui, server = server)