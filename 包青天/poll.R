poll_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        textInput(ns("name"), "輸入品項名稱:"),
        numericInput(ns("price"), "價格:", value = 0)
      ),
      column(
        width = 6,
        selectizeInput(
          ns("cate"),
          "消費種類:",
          choices = c("三餐", "小零食", "生活雜貨","學業用","娛樂開支","通勤")),
        textAreaInput(
          ns("comments"),
          width = "100%",
          label = "Comments"
        ),
        actionButton(ns("submit"), "確認")
      )
    )
  )
  
}

poll_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      observeEvent(input$submit, {
        
        name <- input$name
        price <- input$price
        cate <- input$cate
        comments <- input$comments
        time <- Sys.time()
        url <- Sys.getenv("db_url")
        
        future({
          upload_row(
            x = list(
              "ID" = as.numeric(time),
              "品名" = name,
              "價格" = price,
              "種類" = cate,
              "備註" = comments
            ),
            projectURL = url,
            fileName = "記帳囉"
          )
        })
        
      })
      
    }
  )
}