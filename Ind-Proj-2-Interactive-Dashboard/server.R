
function(input, output) {
  
  # Plotly Output untuk key "PlotSales1" !!PERHATIKAN KEY-Variabelnya!!
  output$PlotSales1 <- renderPlotly({
    #Memilih data sesuai input key *segmen_pilihan*
    data <- data[data$Segment == input$segmen_pilihan1,]
    #Memilih data sesuai input key *category_pilihan*
    data <- data[data$Category == input$category_pilihan1,]
    #Mengubah format column menjadi DateTime
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    #Membuat variabel baru "Year"
    data$Year= format(data$Order.Date, format = "%Y")
    #Memilih data sesuai input key *Year*
    data=data[data$Year==input$tahun_pilihan1,]
    #Recap untuk variabel sales perbulan
    Penjualan_bulanan <- data %>%
      mutate(Order.Date = month(Order.Date)) %>% #Mengambil data berdasar bulan
      group_by(Order.Date) %>% #Mengelompokkan berdasarkan variabel bulan
      summarize(mean_Sales = mean(Sales)) #Summarisasi
    Penjualan_bulanan<- data.frame(Penjualan_bulanan)#Menyimpan dalam datframe
    Penjualan_bulanan<-merge(df_label, Penjualan_bulanan, 
                             by.x="label_angka", 
                             by.y="Order.Date")#Merger dengan label bulan
    Sales_Filter=round(Penjualan_bulanan$mean_Sales,2) #Pembulatan
    Sales_Filter
    #line plot interactive 
    fig1 <- plot_ly()
    fig1 <- fig1 %>% add_lines(x = ~Penjualan_bulanan$label_angka, #sumbu x
                               y = ~Sales_Filter, name = "Sales")# sumbu y
    #Memberi judul gambar dan keterangan
    fig1 <- fig1 %>% layout(
      title=paste("Tahun ", input$tahun), 
      yaxis = list(title="Sales"),
      xaxis = list(title="Bulan ke-"),
      legend = list(orientation = 'h')
    )
    #Mengganti nama titik di sumbu x jadi nama bulan
    fig1 <- fig1 %>%
      layout(
        xaxis = list(
          ticktext = Penjualan_bulanan$label_bulan,
          tickvals = Penjualan_bulanan$label_angka,
          tickmode = "array"
        ))
    
    fig1
    
    
  })
  
  output$PlotSubCat1 <- renderPlotly({
    data <- data[data$Segment == input$segmen_pilihan1,]
    data <- data[data$Category == input$category_pilihan1,]
    
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    
    data$Year= format(data$Order.Date, format = "%Y")
    data=data[data$Year==input$tahun_pilihan1,]
    
    SubCategory<-as.data.frame(table(data$Sub.Category))
    
    fig2 <- plot_ly(
      x = SubCategory$Var1, 
      y = SubCategory$Freq,
      color = SubCategory$Var1,
      type = "bar"
    )
    fig2 <- fig2 %>% layout(showlegend = FALSE)
    fig2
    
    
  })
  
  output$PlotQuantity2 <- renderPlotly({
    data <- data[data$Segment == input$segmen_pilihan2,]
    data <- data[data$Category == input$category_pilihan2,]
    
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    
    data$Year= format(data$Order.Date, format = "%Y")
    data=data[data$Year==input$tahun_pilihan2,]
    
    Quantity_bulanan <- data %>%
      mutate(Order.Date = month(Order.Date)) %>%
      group_by(Order.Date) %>%
      summarize(mean_Quantity = mean(Quantity))
    Quantity_bulanan<- data.frame(Quantity_bulanan)#Menyimpan dalam datframe
    Quantity_bulanan<-merge(df_label, Quantity_bulanan, 
                            by.x="label_angka", 
                            by.y="Order.Date")#Merger dengan label bulan
    Quantity_Filter=round(Quantity_bulanan$mean_Quantity,2)
    Quantity_Filter
    
    fig3 <- plot_ly()
    fig3 <- fig3 %>% add_lines(x = ~Quantity_bulanan$label_angka, y = ~Quantity_Filter, name = "Sales")
    fig3 <- fig3 %>% layout(
      title=paste("Tahun ", input$tahun),
      yaxis = list(title="Sales"),
      xaxis = list(title="Bulan ke-"),
      legend = list(orientation = 'h')
    )
    fig3 <- fig3 %>%
      layout(
        xaxis = list(
          ticktext = Quantity_bulanan$label_bulan, 
          tickvals = Quantity_bulanan$label_angka,
          tickmode = "array"
        ))
    
    fig3
    
    
  })
  
  output$PlotSubCat2 <- renderPlotly({
    data <- data[data$Segment == input$segmen_pilihan2,]
    data <- data[data$Category == input$category_pilihan2,]
    
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    
    data$Year= format(data$Order.Date, format = "%Y")
    data=data[data$Year==input$tahun_pilihan2,]
    
    
    SubCategory <- data %>%
      group_by(Sub.Category) %>%
      summarize(mean_Sales = mean(Sales))
    SubCategory
    
    fig4 <- plot_ly(
      x = SubCategory$Sub.Category,
      y = SubCategory$mean_Sales,
      color = SubCategory$Sub.Category,
      type = "bar"
    )
    fig4 <- fig4 %>% layout(showlegend = FALSE)
    fig4
    
    
  })
  
  output$PlotProfit3 <- renderPlotly({
    data <- data[data$Segment == input$segmen_pilihan3,]
    data <- data[data$Category == input$category_pilihan3,]
    
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    
    data$Year= format(data$Order.Date, format = "%Y")
    data=data[data$Year==input$tahun_pilihan3,]
    
    Profit_bulanan <- data %>%
      mutate(Order.Date = month(Order.Date)) %>%
      group_by(Order.Date) %>%
      summarize(mean_Profit = mean(Profit))
    Profit_bulanan<- data.frame(Profit_bulanan)#Menyimpan dalam datframe
    Profit_bulanan<-merge(df_label, Profit_bulanan, 
                          by.x="label_angka", 
                          by.y="Order.Date")#Merger dengan label bulan
    Profit_Filter=round(Profit_bulanan$mean_Profit,2)
    Profit_Filter
    
    fig5 <- plot_ly()
    fig5 <- fig5 %>% add_lines(x = ~Profit_bulanan$label_angka, y = ~Profit_Filter, name = "Sales")
    fig5 <- fig5 %>% layout(
      title=paste("Tahun ", input$tahun),
      yaxis = list(title="Sales"),
      xaxis = list(title="Bulan ke-"),
      legend = list(orientation = 'h')
    )
    fig5 <- fig5 %>%
      layout(
        xaxis = list(
          ticktext = Profit_bulanan$label_bulan, 
          tickvals = Profit_bulanan$label_angka,
          tickmode = "array"
        ))
    
    fig5
    
    
  })
  
  output$PlotSubCat3 <- renderPlotly({
    
    data <- data[data$Segment == input$segmen_pilihan3,]
    data <- data[data$Category == input$category_pilihan3,]
    
    data$Order.Date <- as.POSIXct(data$Order.Date, format = "%Y-%m-%d",tz="UTC")
    data$Ship.Date <- as.POSIXct(data$Ship.Date, format = "%Y-%m-%d",tz="UTC")
    
    data$Year= format(data$Order.Date, format = "%Y")
    data=data[data$Year==input$tahun_pilihan3,]
    
    SubCategory <- data %>%
      group_by(Sub.Category) %>%
      summarize(mean_Sales = mean(Sales))
    SubCategory
    
    fig6 <- plot_ly(
      x = SubCategory$Sub.Category,
      y = SubCategory$mean_Sales,
      color = SubCategory$Sub.Category,
      type = "bar"
    )
    fig6 <- fig6 %>% layout(showlegend = FALSE)
    fig6
  })
  
  output$plot1 <- renderUI({
    tags$img(src = "https://raw.githubusercontent.com/eliyanto29/My-Personal-Projects/master/Ind_Proj-1-Static-Dashboard/Profile%20Joko.png")
  })
  
}