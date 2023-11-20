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
