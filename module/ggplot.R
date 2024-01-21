a = "#004AAD"
b = "#C0C1C5"


ggplot_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      h3("ggplot教學"),
      p(""),
      p("ggplot是一個繪圖套件，可以用來繪製各種圖表，包括散點圖、長條圖、盒狀圖、小提琴圖等等。"),
      p("ggplot的優點是可以將資料視覺化，讓我們更容易理解資料。"),
      p("ggplot的缺點是需要學習一些語法，但是學習曲線不算陡峭。"),
      p("更多的文字敘述請移步至此的文章，在這邊你可以試用功能以及示範代碼")
    ),
    fluidRow(
      box(
        width=4,
        title = "點子圖", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("基礎",
                   selectInput(ns("x"), "選擇Ｘ軸", choices = names(data)),
                   selectInput(ns("y"), "選擇Ｙ軸", choices = names(data)),
                   selectInput(ns("color"), "選擇顏色變量", choices = names(data)),
                   selectInput(ns("size"), "選擇大小變量", choices = names(data)),
                   ),
          tabPanel("美化與加註",
                   textInput(ns("title"), "輸入標題"),
                   textInput(ns("subtitle"), "輸入副標題"),
                   numericInput(ns("文字大小"), "文字大小", value = 14, min = 1, max = 50)
                   )
        ),
        actionButton(ns("plot_button"), "繪圖")
      ),
      box(
        width=8,
        title = "圖表", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("圖表",
                   plotOutput(ns("plot"))),
          tabPanel("代碼",
                   verbatimTextOutput(ns("code")))
        )
      )
    ),
    fluidRow(
      box(
        width=4,
        title = "長條圖", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("基礎",
                   selectInput(ns("bar_x"), "選擇x軸", choices = names(data)),
                   selectInput(ns("bar_y"), "選擇y軸", choices = names(data)),
                   selectInput(ns("geom_bar"), "選擇模式", c("count", "identity")),
                   selectInput(ns("br_fill"), "選擇填充變量", choices = names(data)),
          ),
          tabPanel("美化與加註",
                   textInput(ns("br_title"), "輸入標題"),
                   textInput(ns("br_subtitle"), "輸入副標題"),
                   numericInput(ns("br_文字大小"), "文字大小", value = 14, min = 1, max = 50)
          )
        ),
        actionButton(ns("br_plot_button"), "繪圖")
      ),
      box(
        width=8,
        title = "圖表", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("圖表",
                   plotOutput(ns("bar_plot"))),
          tabPanel("代碼",
                   verbatimTextOutput(ns("br_code")))
        )
      )
    ),
    fluidRow(
      box(
        width=4,
        title = "直方圖與連續圖", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("基礎",
                   selectInput(ns("fr_x"), "選擇Ｘ軸", choices = NULL),
                   numericInput(ns("fr_binwidth"), "binwidth", value = 0.5, min = 0.01, max = 1, step = 0.01),
                   selectInput(ns("fr_color"), "選擇顏色變量", choices = names(data)),
                   selectInput(ns("fr_type"), "選擇圖表類型", c("直方圖", "連續圖")),
                   sliderInput(ns("xlim"),"x軸觀測範圍",value = c(0,1),min = 0,max = 1,step = 0.01),
          ),
          tabPanel("美化與加註",
                   textInput(ns("fr_title"), "輸入標題"),
                   textInput(ns("fr_subtitle"), "輸入副標題"),
                   numericInput(ns("fr_文字大小"), "文字大小", value = 17, min = 1, max = 50)
          )
        ),
        actionButton(ns("fr_plot_button"), "繪圖")
      ),
      box(
        width=8,
        title = "圖表", solidHeader = TRUE, collapsible = TRUE,
        tabsetPanel(
          tabPanel("圖表",
                   plotOutput(ns("fr_plot"))),
          tabPanel("代碼",
                   verbatimTextOutput(ns("fr_code")))
        )
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "儲存圖表展示",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(ns("plot_list"), "選擇圖表", choices = NULL),
        numericInput(ns("height"), "高", min = 2, max = 10, value = 3, step = 0.5),
        numericInput(ns("width"), "寬", min = 2, max = 10, value = 4, step = 0.5),
        actionButton(ns("delete_button"), "刪除圖表"),
        actionButton(ns("save_button"), "儲存圖表")
      ),
      column(
        width = 8,
        plotOutput(ns("plot_list_plot"))
      )
    )
  )
}

ggplot_server <- function(id, data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      # axis
      axis <- c("box_x", "box_y", "x", "y", "bar_x","bar_y")
      
      observe({
        axis%>%
          map(~updtaeAxis(data, .x, session))
      })
      
      # list
      plotList <- reactiveVal(list())
    
      # geom point
    
      
      observe({
        color_choices <- names(data)
        updateSelectInput(session, "color", choices = c("NULL" = "NULL", color_choices), selected = "NULL")
      })
      
      observe({
        size_choices <- names(data)
        updateSelectInput(session, "size", choices = c("NULL" = "NULL", size_choices), selected = "NULL")
      })
      
      observe({
        plot_choices <- names(plotList())
        updateSelectInput(session, "plot_list", choices = plot_choices, selected = plot_choices[1])
      })
      
      output$code <- renderPrint({
        plotcode <- glue::glue(
          "ggplot(data, aes(x = {input$x}, y = {input$y}, color = {input$color}, size = {input$size})) +
           geom_point() +
           labs(title = {input$title}, x = \"\", y = \"\", subtitle = {input$subtitle}) +
           theme_classic() +
           theme(text = element_text(family = \"sans\", size = {input$文字大小})) +
           scale_colour_brewer(palette = \"Set1\") +
           scale_size(range = c(1, 10))
          "
        )
        plotcode
      })
      
      
      poll_confirm <- function(){
        
        modalDialog(
          "確認新增圖表？",
          title = "確認新增",
          textInput(ns("plot_name"), "輸入圖表名稱"),
          footer = tagList(
            actionButton(ns("cancel"), "Cancel"),
            actionButton(ns("ok1"), "OK")
          )
        )
        
      }
      
      observeEvent(input$plot_button, {
        
        showModal(poll_confirm())
        
      })
      
     observeEvent(input$ok1, {
       
       if(input$color == "NULL"){
         geom_color <- NULL
       }else{
         geom_color <- as.character(data[[input$color]])
       }
       
       
       geom_size <- ifNULL(data, input$size)
       
       new_plot <- ggplot(data, aes(x = .data[[input$x]], y = .data[[input$y]], color = geom_color, size = geom_size)) +
              geom_point() +
              labs(title = input$title, x = " ", y = " ", subtitle = input$subtitle) +
              theme(text = element_text(family = "sans", size = input$文字大小)) +
              guides(color = guide_legend(title = input$color), size = guide_legend(title = input$size)) +
              theme_classic()+
              scale_colour_brewer(palette = "Set1")
         

              # 更新 plotList
              current_plots <- plotList()
              current_plots[[input$plot_name]] <- new_plot
              plotList(current_plots)
            
              # 確認添加成功
              if(input$plot_name %in% names(plotList())) {
                shinyjs::alert("圖表新增成功")
              } else {
                shinyjs::alert("圖表新增失敗")
              }
            
              removeModal()
     })
     
     observeEvent(input$cancel, {
       removeModal()
     })
    
      output$plot <- renderPlot({
        
        if(input$color == "NULL"){
          geom_color <- NULL
        }else{
          geom_color <- as.character(data[[input$color]])
        }
        
        geom_size <- ifNULL(data, input$size)
        
        ggplot(data, aes(x = .data[[input$x]], y = .data[[input$y]], color = geom_color, size = geom_size)) +
          geom_point()+
          labs(title = input$title, x = " ", y = " ", subtitle = input$subtitle) +
          guides(color = guide_legend(color = input$color, size = input$size))+
          theme_classic()+
          theme(text = element_text(family = "sans", size=input$文字大小))+
          scale_colour_brewer(palette = "Set1")
        
      }, res = 96)
      
      
      # bar
      
      observe({
        br_fill_choices <- names(data)
        updateSelectInput(session, "br_fill", choices = c("NULL" = "NULL", br_fill_choices), selected = "NULL")
      })
      
      br_confirm <- function(){
        
        modalDialog(
          "確認新增圖表？",
          title = "確認新增",
          textInput(ns("br_plot_name"), "輸入圖表名稱"),
          footer = tagList(
            actionButton(ns("br_cancel"), "Cancel"),
            actionButton(ns("br_ok"), "OK")
          )
        )
        
      }
      
      observeEvent(input$br_plot_button, {
        
        showModal(br_confirm())
        
      })
      
      observeEvent(input$br_ok, {
       
        
        br_new_plot <-  plot_bar()+
                          labs(title = input$br_title, x = " ", y = " ", subtitle = input$br_subtitle) +
                          theme_classic()+
                          guides(fill = guide_legend(fill = input$br_fill))+
                          scale_fill_brewer(palette = "Set1")+
                          theme(text = element_text(family = "sans", size=input$br_文字大小))
          
          
        # 更新 plotList
        br_current_plots <- plotList()
        br_current_plots[[input$br_plot_name]] <- br_new_plot
        plotList(br_current_plots)
        
        # 確認添加成功
        if(input$br_plot_name %in% names(plotList())) {
          shinyjs::alert("圖表新增成功")
        } else {
          shinyjs::alert("圖表新增失敗")
        }
        
        removeModal()
      })
      
      observeEvent(input$br_cancel, {
        removeModal()
      })
      
      output$br_code <- renderPrint({
        plotcode <- glue::glue(
          "ggplot(data, aes(x = {input$bar_x}, fill = {input$br_fill})) +
           geom_bar(stat = \"count\", position = \"dodge\") +
           labs(title = {input$br_title}, x = \"\", y = \"\", subtitle = {input$br_subtitle}) +
           theme_classic() +
           theme(text = element_text(family = \"sans\", size = {input$br_文字大小})) +
           scale_fill_brewer(palette = \"Set1\")
          "
        )
        plotcode
      })
      
     
      output$bar_plot <- renderPlot({
        
        plot_bar()+
          labs(title = input$br_title, x = " ", y = " ", subtitle = input$br_subtitle) +
          theme_classic()+
          guides(fill = guide_legend(fill = input$br_fill))+
          scale_fill_brewer(palette = "Set1")+
          theme(text = element_text(family = "sans", size=input$br_文字大小))
        
      }, res = 96)
      
      
      plot_bar <- reactive({
        
        if(input$br_fill == "NULL"){
          br_fill_br <- NULL
        }else{
          br_fill_br <- as.factor(data[[input$br_fill]])
        }
        
        switch(input$geom_bar,
               count = ggplot(data, aes(x = as.character(.data[[input$bar_x]]), fill = br_fill_br)) +
                 geom_bar(stat = "count", position = "dodge"),
               identity = ggplot(data, aes(x = as.character(.data[[input$bar_x]]), y = .data[[input$bar_y]],  fill = br_fill_br)) +
                 geom_bar(stat = "identity", position = "dodge")
        )
      })
      
      # geom fr
      observe({
        fr_max <- max(data[[input$fr_x]])
        fr_min <- min(data[[input$fr_x]])
        updateSliderInput(session, "xlim", min = fr_min, max = fr_max, value = c(fr_min, fr_max))
      })
      
      observe({
        fr_color_choices <- names(data)
        updateSelectInput(session, "fr_color", choices = c("NULL" = "NULL", fr_color_choices), selected = "NULL")
      })
      
      observe({
        numeric_data <- data %>% select_if(is.numeric)
        fr_x_choices <- names(numeric_data)
        updateSelectInput(session, "fr_x", choices = fr_x_choices)
      })
      
      plot_fr <- reactive({
        
        if(input$fr_color == "NULL"){
          fr_geom_color <- NULL
        }else{
          fr_geom_color <- as.character(data[[input$fr_color]])
        }
        
        switch(input$fr_type,
               直方圖 = ggplot(data, aes(.data[[input$fr_x]], fill = fr_geom_color))+
                         geom_histogram(binwidth = input$fr_binwidth, position = "dodge"),
               連續圖 = ggplot(data, aes(.data[[input$fr_x]], color = fr_geom_color))+
                          geom_freqpoly(binwidth = input$fr_binwidth)
        )
      })
      
      
      
      output$fr_code <- renderPrint({
        plotcode <- glue::glue(
          "ggplot(data, aes(x = {input$fr_x}, fill = {input$fr_color})) +
           geom_histogram(binwidth = {input$fr_binwidth}, position = \"dodge\") +
           labs(title = {input$fr_title}, x = \"\", y = \"\", subtitle = {input$fr_subtitle}) +
           coord_cartesian(xlim = c({input$xlim[1]}, {input$xlim[2]})) +
           theme_classic() +
           theme(text = element_text(family = \"sans\", size = {input$fr_文字大小})) +
           scale_fill_brewer(palette = \"Set1\")
          "
        )
        plotcode
      })
      
      
      fr_confirm <- function(){
        
        modalDialog(
          "確認新增圖表？",
          title = "確認新增",
          textInput(ns("fr_plot_name"), "輸入圖表名稱"),
          footer = tagList(
            actionButton(ns("fr_cancel"), "Cancel"),
            actionButton(ns("fr_ok"), "OK")
          )
        )
        
      }
      
      observeEvent(input$fr_plot_button, {
        
        showModal(fr_confirm())
        
      })
      
      observeEvent(input$fr_ok, {
        
        new_plot <- plot_fr() +
                    labs(title = input$fr_title, x = " ", y = " ", subtitle = input$fr_subtitle) +
                    guides(color = guide_legend(title = input$fr_color), fill = guide_legend(title = input$fr_color)) +
                    coord_cartesian(xlim = input$xlim)+
                    theme_classic()+
                    scale_colour_brewer(palette = "Set1")+
                    scale_fill_brewer(palette = "Set1")+
                    theme(text = element_text(family = "sans", size = input$fr_文字大小))
        
          
                    
        # 更新 plotList
        current_plots <- plotList()
        current_plots[[input$fr_plot_name]] <- new_plot
        plotList(current_plots)
        
        # 確認添加成功
        if(input$fr_plot_name %in% names(plotList())) {
          shinyjs::alert("圖表新增成功")
        } else {
          shinyjs::alert("圖表新增失敗")
        }
        
        removeModal()
      })
      
      observeEvent(input$fr_cancel, {
        removeModal()
      })
      
      
      output$fr_plot <- renderPlot({
        
        
        plot_fr() +
          labs(title = input$fr_title, x = " ", y = " ", subtitle = input$fr_subtitle) +
          guides(color = guide_legend(title = input$fr_color), fill = guide_legend(title = input$fr_color)) +
          coord_cartesian(xlim = input$xlim)+
          theme_classic()+
          scale_colour_brewer(palette = "Set1")+
          scale_fill_brewer(palette = "Set1")+
          theme(text = element_text(family = "sans", size = input$fr_文字大小))
        
      }, res = 96)
      
      
      ## view plot
      
      plot_width <- reactive(input$width*100)
      plot_height <- reactive(input$height*100)
      
      observe({
        plot_choices <- names(plotList())
        updateSelectInput(session, "plot_list", choices = plot_choices, selected = plot_choices[1])
      })
      
      
      output$plot_list_plot <- renderPlot({
        if(length(plotList()) == 0) {
          ggplot() + 
            annotate(
              geom = "text", x = 1, y = 2, 
              label = "還沒有圖表喔", hjust = 0.5, vjust = 0, size = 10
            )+
            theme_void()
        } else {
          plotList()[[input$plot_list]]
        }
      }, res = 96,height = plot_height, width = plot_width)
      
      ## delete plot
      delete_modal <- function(){
        
        modalDialog(
          "確認刪除圖表？",
          title = "確認刪除",
          footer = tagList(
            actionButton(ns("cancel2"), "Cancel"),
            actionButton(ns("ok2"), "OK")
          )
        )
        
      }
      
      observeEvent(input$delete_button, {
        showModal(delete_modal())
      })
      
      observeEvent(input$ok2, {
        current_plots <- plotList()
        current_plots[[input$plot_list]] <- NULL
        plotList(current_plots)
        removeModal()
      })
      
      observeEvent(input$cancel2, {
        removeModal()
      })
      
      
      ## save plot
      
      save_modal <- function(){
        
        modalDialog(
          "確認儲存圖表？",
          title = "確認儲存",
          textInput(ns("save_name"), "輸入圖表名稱"),
          footer = tagList(
            actionButton(ns("cancel3"), "Cancel"),
            actionButton(ns("ok3"), "OK")
          )
        )
        
      }
      
      observeEvent(input$save_button, {
        showModal(save_modal())
      })
      
      observeEvent(input$ok3, {
        ggsave(plot = plotList()[[input$plot_list]], filename = paste0(input$save_name, ".png"),
               width = input$width, height = input$height, units = 'in')
        
        shinyjs::alert("圖表儲存成功")
        
        removeModal()
      })
      
      observeEvent(input$cancel3, {
        removeModal()
      })
      
      
      
      
    }
  )
}