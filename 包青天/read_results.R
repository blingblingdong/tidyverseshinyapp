read_results_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dataTableOutput(ns("results"))
  )
}

read_results_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      timer <- reactiveTimer(15000)
      
      data <- reactiveValues()
      
      observe({
        url <- Sys.getenv("db_url")
        data$results <- future({
          download_df(
            projectURL = url,
            fileName = "記帳囉")
        })
        timer()
        
      })
      
      output$results <- renderDataTable({
        data$results
      })
      
    }
  )
}

analitic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    textOutput(ns("analitic")),
    actionButton(ns("analitic_btn"), "進行分析")
  )
}

analitic_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      timer <- reactiveTimer(15000)
      
      data <- reactiveValues(analitic = NULL)
      
      observe({
        url <- Sys.getenv("db_url")
        data$analitic <- eventReactive(input$analitic_btn, {
          download_df(
            projectURL = url,
            fileName = "記帳囉"
          ) %>%
            as_tibble()  # 將 data.frame 轉換為 tibble
        })
        timer()
      })
      
      output$analitic <- 
        renderText({
        # 確保 data$analitic 有值
        req(data$analitic())
        
        # 計算總消費金額
        total_spent <- sum(data$analitic()$價格)
        
        paste0("你目前的總消費金額是：", total_spent)
      })
     
      
    }
  )
}

      
      