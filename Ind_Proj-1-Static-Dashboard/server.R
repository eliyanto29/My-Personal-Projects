server <- function(input, output) {
  
  # Tab 1 profit_VB ------------------------------------------
  output$profit_VB <- renderValueBox({
    
    valueBox(RKB,icon = icon("chart-bar"), color = "purple",subtitle = "Average Profit")
  })
  
  # Tab 1 time_VB ------------------------------------------
  output$time_VB <- renderValueBox({
    valueBox(3.96,icon = icon("clock"), color = "fuchsia",subtitle = "Average Ship Days")
  })
  
  # Tab 1 quantity_VB ------------------------------------------
  output$quantity_VB <- renderValueBox({
    valueBox(RQB,icon = icon("box"), color = "teal",subtitle = "Average Quantity")
  })
  
  # Plotly 1
  output$Plot1 <- renderPlotly({
    fig1
  })
  
  # Plotly 2
  output$Plot2 <- renderPlotly({
    fig2
  })
  
  # Plotly 3
  output$Plot3 <- renderPlotly({
    fig3
  })
  
  # Plotly 4
  output$Plot4 <- renderPlotly({
    fig4
  })
  
  # Plotly 5
  output$Plot5 <- renderPlotly({
    fig5
  })
  
  output$datatable = DT::renderDataTable({
    retail_display=retail[,12:15]
  })
  
  output$plot1 <- renderUI({
    tags$img(src = "https://raw.githubusercontent.com/eliyanto29/My-Personal-Projects/master/Ind_Proj-1-Static-Dashboard/Profile%20Joko.png")
  })
  
}