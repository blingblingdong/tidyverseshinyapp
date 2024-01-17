tidyr_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    h3("tidyr教學與試用"),
    p("這裡我們會介紹第二個tidyvese的核心套件tidyr"),
    p("tidyr的主要功能是幫助我們整理資料，讓資料的格式更加整齊"),
    fluidRow(
      box(title= "這是用select()函數製作的功能",width=4,collapsible = FALSE,
          selectInput(ns("選擇欄位"), "選擇欄位", choices = character(0), multiple = TRUE)
      ),
      box(width = 8,collapsible = FALSE,
           DT::dataTableOutput(ns("select表格")))),
    fluidRow(
      box(title= "這是用filter函數製作的功能",width=4,
          selectInput(ns("篩選欄位"), "篩選欄位", choices = c("無", character(0)), multiple = FALSE),
          textInput(ns("輸入資料"), "輸入資料")
      ),
      box( width = 8,
           DT::dataTableOutput(ns("篩選表格")))),
    fluidRow(
      box(title= "這是用mutate函數製作的功能",width=4,
          textInput(ns("新增欄位"), "新增欄位"),
          textInput(ns("輸入資料2"), "輸入資料"),
          actionButton(ns("apply_mutate"), "應用")
      ),
      box( width = 8,
           DT::dataTableOutput(ns("mutate表格")))),
    fluidRow(
      box(title= "這是用groupby加summarise函數製作的功能",width=4,
          selectInput(ns("選擇欄位g"), "選擇欄位", choices = character(0), multiple = TRUE),
          actionButton(ns("apply_group"), "應用")
      ),
      box( width = 8,
           DT::dataTableOutput(ns("group表格")))),
    fluidRow(
      box(title= "這是用pivot_wider函數製作的功能",width=4,
          selectInput(ns("value欄位"), "value欄位", choices = character(0)),
          selectInput(ns("name欄位"), "name欄位", choices = character(0)),
          actionButton(ns("apply_pivot_w"),"應用pivot_wider")
      ),
      box(width = 8,
          DT::dataTableOutput(ns("pivot_w表格")))),
    fluidRow(
      box(title= "這是用pivot_longer函數製作的功能",width=4,
          selectInput(ns("value_l欄位"), "value欄位", choices = character(0),multiple = TRUE),
          textInput(ns("value欄名稱"),"value欄名稱"),
          textInput(ns("name欄名稱"),"name欄名稱"),
          actionButton(ns("apply_pivot_l"),"應用pivot_longer")
      ),
      box(width = 8,
          DT::dataTableOutput(ns("pivot_l表格"))))
    
  )
  
  
}

tidyr_server <- function(id, data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      observe({
        欄位 <- names(data)
        updateSelectInput(session, "選擇欄位", choices = 欄位, selected = 欄位[1])
      })
      
      output$select表格 = renderDT({
        data%>% select(input$選擇欄位)
      })
      
      observe({
        欄位 <- names(data)
        updateSelectInput(session, "篩選欄位", choices = c("無", 欄位), selected = "無")
      })
      
      
      output$篩選表格 = renderDT({
        if (input$篩選欄位 != "無" && input$輸入資料 != "") {
          data%>% 
            filter(.data[[input$篩選欄位]] == input$輸入資料)
        } else {
          data
        }
      })
      
      observeEvent(input$button, {
        toggle("hello")
      })
      
      
      output$mutate表格 = renderDT({
        if (input$apply_mutate > 0 && !is.null(input$輸入資料2)) {
          data_mutate <- data %>%
            mutate(!!input$新增欄位:= !!rlang::parse_expr(input$輸入資料2))
        } else {data_mutate <- data }
        data_mutate
      })
      
      observe({
        欄位g <- names(data)
        updateSelectInput(session, "選擇欄位g", choices = c("無", 欄位g), selected = "無")
      })
      
      output$group表格 = renderDT({
        if (input$apply_group %% 2 == 1) {
          selected_columns <- c(input$選擇欄位g)
          data_group <- data %>%
            group_by(!!!syms(selected_columns))%>%
            summarise(count=n())
        } else {
          data_group <- data
        }
        data_group
      })
      
      observe({
        欄位 <- names(data)
        updateSelectInput(session, "value欄位", choices = 欄位, selected = 欄位[1])
      })
      
      observe({
        欄位 <- names(data)
        updateSelectInput(session, "name欄位", choices = 欄位, selected = 欄位[2])
      })
      
      
      
      output$pivot_w表格 = renderDT({
        if (input$apply_pivot_w %% 2 == 1) {
          data_pivot <- data %>%
            pivot_wider(names_from = input$name欄位,values_from = input$value欄位)
        }
        else {
          data_pivot <- data
        }
        data_pivot
      })
      
      observe({
        欄位 <- names(data)
        updateSelectInput(session, "value_l欄位", choices = 欄位, selected = 欄位[1])
      })
      
      output$pivot_l表格 = renderDT({
        if (input$apply_pivot_l %% 2 == 1) {
          data_pivot_l <- data %>%
            pivot_longer(c(input$value_l欄位),names_to = input$name欄名稱,values_to = input$value欄名稱)
        }
        else {
          data_pivot_l <- data
        }
        data_pivot_l
      })
      
      
    }
  )
}