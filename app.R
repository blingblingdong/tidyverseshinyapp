options(encoding = "UTF-8")

library(shiny)
library(promises)
library(future)
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
library(ggdark)
library(ggthemes)

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
      sidebarMenu(
        menuItem("上傳你的data", tabName = "上傳你的data"),
        menuItem("tidyr教學與試用", tabName = "tidyr教學"),
        menuItem("ggplot教學與試用", tabName = "ggplot教學")
      )
    ),
    controlbar = dashboardControlbar(
      actionButton("submit", "Submit"),
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "上傳你的data",
                fluidRow(bs4Card(
                  title = "上傳csv檔案", width = 4,
                  tags$p("Ready to take the Shiny tutorial? If so"),
                  fileInput("data", "請提供你的數據", accept = ".csv")
                ),
                box( width = 8,
                     DT::dataTableOutput("output",height = "50%")))
        ),
        tabItem(tabName = "tidyr教學",
                tidyr_ui("tidyr")),
        tabItem(tabName = "ggplot教學",
                ggplot_ui("ggplot"))
      )
    )
  )


server <- function(input, output, session) {
  
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
  
  tidyr_server("tidyr", data())
  
  ggplot_server("ggplot", data())
  
  
  
}

shinyApp(ui = ui, server = server)













