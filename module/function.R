plotForApp <- function(data, x, y, ...){
  
  ggplot(data, aes(factor(.data[[x]], levels = unique(.data[[x]])), .data[[y]], ...))
  
}


updtaeAxis <- function(data, axis, session){
  
  axis_choices <- names(data)
  updateSelectInput(session, axis, choices = axis_choices, selected = axis_choices[1])

}

ifNULL <- function(data,x){
  if(x == "NULL"){
    x2 <- NULL
  }else{
    x2 <- data[[x]]
  }
  
  return(x2)
}

# box

# fluidRow(
#   box(
#     width=4,
#     title = "箱形圖", solidHeader = TRUE, collapsible = TRUE,
#     selectInput(ns("box_x"), "選擇x軸", choices = names(data)),
#     selectInput(ns("box_y"), "選擇y軸", choices = names(data)),
#     textInput(ns("box_title"), "輸入標題"),
#     selectInput(ns("box_theme1"), "選擇主題", c("白" = "白", "黑" = "黑", "藍" = "藍")),
#     numericInput(ns("box_文字大小"), "文字大小", value = 17, min = 1, max = 50),
#     selectInput(ns("geom_box"), "選擇圖表類型", c("盒狀圖", "抖點圖", "小提琴圖"))
#   ),
#   box(
#     width=8,
#     title = "圖表", solidHeader = TRUE, collapsible = TRUE,
#     plotOutput(ns("box_plot"))
#   )
# ),

# output$box_plot <- renderPlot({
#   
#   plotForApp(data, input$box_x, input$box_y) +
#     geom_box() +
#     labs(title = input$box_title, x = " ", y = " ") +
#     theme(text = element_text(family = "黑體-繁 中黑", size=input$box_文字大小))
# }, res = 96)
# 
# geom_box <- reactive({
#   switch(input$geom_box,
#          盒狀圖 = geom_boxplot(),
#          抖點圖 = geom_jitter(),
#          小提琴圖 = geom_violin()
#   )
# })


# x <- tibble(
#   x = c("a", "b", "c", "a", "b", "c"),
#   y = c(1, 2, 3, 4, 5, 6)
# ) %>%
#   plotForApp('x', 'y', color = NULL) +
#   geom_point()

# theme1 <- reactive({
#   switch(input$theme1,
#          白 = theme_classic(),
#          黑 = dark_theme_classic(),
#          藍 = theme_economist()
#   )
# })