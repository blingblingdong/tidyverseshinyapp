ggplot_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width=4,
        title = "控制圖表", solidHeader = TRUE, collapsible = TRUE,
        selectInput(ns("y"), "選擇Ｙ軸", choices = names(data)),
        selectInput(ns("x"), "選擇Ｘ軸", choices = names(data)),
        selectInput(ns("geom"), "選擇圖表類型", c("點子圖", "直線圖")),
        textInput(ns("title"), "輸入標題"),
        selectInput(ns("theme1"), "選擇主題", c("白" = "白", "黑" = "黑", "藍" = "藍")),
        numericInput(ns("文字大小"), "文字大小", value = 17, min = 1, max = 50)
      ),
      box(
        width=8,
        title = "圖表", solidHeader = TRUE, collapsible = TRUE,
        plotOutput(ns("plot"))
      )
    ),
  )
  
  
  
}

ggplot_server <- function(id, data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      a = "#004AAD"
      b = "#C0C1C5"
      
      observe({
        y_choices <- names(data)
        updateSelectInput(session, "y", choices = y_choices, selected = y_choices[1])
      })
      
      observe({
        x_choices <- names(data)
        updateSelectInput(session, "x", choices = x_choices, selected = x_choices[1])
      })
      
      plot_geom <- reactive({
        switch(input$geom,
               點子圖 = geom_point(color = a,size=2),
               直線圖 = geom_line(group = 1, linewidth = 1, color = a)
        )
      })
      
      theme1<- reactive({
        switch(input$theme1,
               白 = theme_classic(),
               黑 =dark_theme_classic(),
               藍=theme_economist()
        )
      })
      
      output$plot <- renderPlot({
        x_data <- data[[input$x]]
        x_levels <- unique(x_data)
        x_factor <- factor(x_data, levels = x_levels)
        
        ggplot(data, aes(x = x_factor, y = .data[[input$y]])) +
          plot_geom() +
          labs(title = input$title, x = " ", y = " ") +
          theme1()+
          theme(text = element_text(family = "黑體-繁 中黑", size=input$文字大小))
      }, res = 96)
      
      
      
    }
  )
}