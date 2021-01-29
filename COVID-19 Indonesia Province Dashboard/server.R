library(httr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(DT)

library(dplyr)
library(ggplot2)
library(rjson)
library(leaflet)
library(RCurl)
library(plotly)
library(DT)


function(input, output) { 
  
  output$plot1 <- renderUI({
    tags$img(src = "https://raw.githubusercontent.com/eliyanto29/My-Personal-Projects/master/Sep_2_COVID_19_Indonesia_Shiny_DashBoard/dashboard.png")
  })
  
  output$tanggal_up <- renderInfoBox({
    infoBox(
      "Tanggal", tanggal_u, icon = icon("calendar",lib='glyphicon'),
      color = "purple"
    )
  })
  
  output$bulan_up <- renderInfoBox({
    infoBox(
      "Bulan", bulan_u, icon = icon("calendar",lib='glyphicon'),
      color = "fuchsia"
    )
  })
  
  output$tahun_up <- renderInfoBox({
    infoBox(
      "Tahun", tahun_u, icon = icon("calendar",lib='glyphicon'),
      color = "yellow"
    )
  })
  
  output$value1 <- renderValueBox({
    valueBox(
      formatC(konfirmasi_data, format="d", big.mark=',')
      ,paste('Konfirmasi')
      ,icon = icon("plus-sign",lib='glyphicon')
      ,color = "blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(sembuh_data, format="d", big.mark=',')
      ,paste('Sembuh')
      ,icon = icon("heart-empty",lib='glyphicon')
      ,color = "green")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(meninggal_data, format="d", big.mark=',')
      ,paste('Meninggal')
      ,icon = icon("object-align-vertical",lib='glyphicon')
      ,color = "red")
  })
  
  output$mapplot <- renderLeaflet({
    m <- leaflet(data_map_prov_indo) %>% 
      addTiles()  %>% 
      setView( lat=-4, lng=118 , zoom=4) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(~longitude, ~latitude, 
                       fillColor = mypalette(konfirmasi) , fillOpacity = 0.7, color="white", radius=~sqrt(konfirmasi)/11, stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend(pal= mypalette, values=~konfirmasi, opacity=0.9, title = "Kasus Positif", position = "bottomright" )
    
    m 
  })
  
  output$provinsi10b_plot <- renderPlot({
    
    ggplot(provinsi10b) +
      geom_bar(aes(y=konfirmasi,x = reorder(prov, konfirmasi), fill=prov), 
               stat="identity" ) +
      scale_fill_brewer(palette="Spectral") +
      coord_flip()+
      guides(fill=FALSE)+
      labs (y="Jumlah Kasus COVID-19", x = "Provinsi")
    
  })
  
  output$coviddata = DT::renderDataTable({
    indo_covid_prov
  })
  
  output$konf_plot <- renderPlotly({
    fig <- plot_ly(new_dataframe, x = ~tanggal, y = ~kasus_baru, 
                   line = list(color = 'rgb(22, 96, 167)'),
                   marker = list(color = 'rgb(22, 96, 167)'), 
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = "Perkembangan Kasus Terkonfirmasi Positif COVID-19 Per-Hari Nasional",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$sembuh_plot <- renderPlotly({
    fig <- plot_ly(new_dataframe, x = ~tanggal, y = ~sembuh,
                   line = list(color = 'rgb(96, 167, 22)'),
                   marker = list(color = 'rgb(96, 167, 22)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = "Perkembangan Kasus Sembuh Per-Hari Nasional",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$meninggal_plot <- renderPlotly({
    fig <- plot_ly(new_dataframe, x = ~tanggal, y = ~sembuh, 
                   line = list(color = 'rgb(167, 1, 1)'), 
                   marker = list(color = 'rgb(167, 1, 1)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = "Perkembangan Kasus Meninggal Per-Hari Nasional",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$gabungan1_plot <- renderPlotly({
    fig <- plot_ly(new_dataframe, x = ~tanggal, y = ~kasus_baru,
                   name="Kasus Baru",
                   line = list(color = 'rgb(22, 96, 167)'), 
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~sembuh, 
                             name = "Sembuh",
                             line = list(color = 'rgb(96, 167, 22)'), 
                             marker = list(color = 'rgb(96, 167, 22)'),
                             type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~meninggal, 
                             name = "Meninggal",
                             line = list(color = 'rgb(167, 1, 1)'), 
                             marker = list(color = 'rgb(167, 1, 1)'),
                             type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = "Perkembangan Kasus Per-Hari Gabungan Nasional",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$gabungan2_plot <- renderPlotly({
    fig <- plot_ly(akumulasi, x = ~tanggal, y = ~akumulasi_kasus_aktif,
                   name="Akumulasi Kasus Aktif",
                   line = list(color = 'rgb(22, 96, 167)'), 
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~akumulasi_sembuh, 
                             name = "Akumulasi Kasus Sembuh",
                             line = list(color = 'rgb(96, 167, 22)'), 
                             marker = list(color = 'rgb(96, 167, 22)'),
                             type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~akumulasi_meninggal, 
                             name = "Akumulasi Kasus Meninggal",
                             line = list(color = 'rgb(167, 1, 1)'), 
                             marker = list(color = 'rgb(167, 1, 1)'),
                             type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = "Kasus Aktif, Sembuh, dan Meninggal Per-Hari",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$gabungan3_plot <- renderPlotly({
    fig = plot_ly(bulanan, x=~bulan, y=~`Kasus Baru`, type="bar", name = "Kasus Baru", marker = list(color = 'rgb(22, 96, 167)'))
    fig <- fig %>% add_trace(y = ~Sembuh, name = 'Sembuh', marker = list(color = 'rgb(96, 167, 22)'))
    fig <- fig %>% add_trace(y = ~Meninggal, name = 'Meninggal', marker = list(color = 'rgb(167, 1, 1)'))
    
    fig <- fig %>% layout(
      title = "Tren Bulanan COVID-19 Nasional",
      xaxis = list(title = 'Bulan'),
      yaxis = list(title = 'Jumlah'),
      barmode = 'group')
    
    fig
  })
  
  output$barnasplot <- renderPlotly({
    fig <- plot_ly(new_dataframe, x = ~tanggal, y = ~kasus_baru,
                   name="Kasus Baru",
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'bar')
    fig <- fig %>% add_trace(y = ~sembuh, 
                             name = "Sembuh",
                             marker = list(color = 'rgb(96, 167, 22)')) 
    fig <- fig %>% add_trace(y = ~meninggal, 
                             name = "Meninggal",
                             marker = list(color = 'rgb(167, 1, 1)'))
    fig <- fig %>% layout(title = "Perkembangan Kasus Per-Hari Gabungan Nasional",
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"),
                          barmode = 'stack')
    
    fig
  })
  
  
  output$value1_a <- renderValueBox({
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    konfirmasi_prov <- sum(dataframe_prov$KASUS)
    
    valueBox(
      formatC(konfirmasi_prov, format="d", big.mark=',')
      ,paste('Konfirmasi')
      ,icon = icon("plus-sign",lib='glyphicon')
      ,color = "blue")
  })
  
  output$value2_a <- renderValueBox({
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    sembuh_prov<- sum(dataframe_prov$SEMBUH)
    
    valueBox(
      formatC(sembuh_prov, format="d", big.mark=',')
      ,paste('Sembuh')
      ,icon = icon("heart-empty",lib='glyphicon')
      ,color = "green")
  })
  
  output$value3_a <- renderValueBox({
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    meninggal_prov<- sum(dataframe_prov$MENINGGAL)
    
    valueBox(
      formatC(meninggal_prov, format="d", big.mark=',')
      ,paste('Meninggal')
      ,icon = icon("object-align-vertical",lib='glyphicon')
      ,color = "red")
  })
  
  output$coviddata_prov = DT::renderDataTable({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        `Konfirmasi Positif` = KASUS,
        Meninggal = MENINGGAL,
        Sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    new_dataframe_prov
  })
  
  output$konf_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    fig <- plot_ly(new_dataframe_prov, x = ~tanggal, y = ~kasus_baru, 
                   line = list(color = 'rgb(22, 96, 167)'),
                   marker = list(color = 'rgb(22, 96, 167)'), 
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = paste("Perkembangan Kasus Terkonfirmasi Positif COVID-19 Per-Hari Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$sembuh_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    fig <- plot_ly(new_dataframe_prov, x = ~tanggal, y = ~sembuh,
                   line = list(color = 'rgb(96, 167, 22)'),
                   marker = list(color = 'rgb(96, 167, 22)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = paste("Perkembangan Kasus Sembuh Per-Hari Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$meninggal_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    fig <- plot_ly(new_dataframe_prov, x = ~tanggal, y = ~sembuh, 
                   line = list(color = 'rgb(167, 1, 1)'), 
                   marker = list(color = 'rgb(167, 1, 1)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = paste("Perkembangan Kasus Meninggal Per-Hari Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    
    fig
  })
  
  output$gabungan1_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    fig <- plot_ly(new_dataframe_prov, x = ~tanggal, y = ~kasus_baru,
                   name="Kasus Baru",
                   line = list(color = 'rgb(22, 96, 167)'), 
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~sembuh, 
                             name = "Sembuh",
                             line = list(color = 'rgb(96, 167, 22)'), 
                             marker = list(color = 'rgb(96, 167, 22)'),
                             type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~meninggal, 
                             name = "Meninggal",
                             line = list(color = 'rgb(167, 1, 1)'), 
                             marker = list(color = 'rgb(167, 1, 1)'),
                             type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = paste("Perkembangan Kasus Per-Hari Gabungan Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    fig
  })
  
  output$gabungan2_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    akumulasi_prov <- 
      new_dataframe_prov%>% 
      transmute(
        tanggal,
        akumulasi_kasus_aktif = cumsum(kasus_baru)- cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    fig <- plot_ly(akumulasi_prov, x = ~tanggal, y = ~akumulasi_kasus_aktif,
                   name="Akumulasi Kasus Aktif",
                   line = list(color = 'rgb(22, 96, 167)'), 
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~akumulasi_sembuh, 
                             name = "Akumulasi Kasus Sembuh",
                             line = list(color = 'rgb(96, 167, 22)'), 
                             marker = list(color = 'rgb(96, 167, 22)'),
                             type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>% add_trace(y = ~akumulasi_meninggal, 
                             name = "Akumulasi Kasus Meninggal",
                             line = list(color = 'rgb(167, 1, 1)'), 
                             marker = list(color = 'rgb(167, 1, 1)'),
                             type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% layout(title = paste("Kasus Aktif, Sembuh, dan Meninggal Per-Hari Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"))
    fig
  })
  
  output$gabungan3_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    akumulasi_prov <- 
      new_dataframe_prov%>% 
      transmute(
        tanggal,
        akumulasi_kasus_aktif = cumsum(kasus_baru)- cumsum(sembuh) - cumsum(meninggal),
        akumulasi_sembuh = cumsum(sembuh),
        akumulasi_meninggal = cumsum(meninggal)
      )
    
    bulanan_prov <- new_dataframe_prov %>% 
      count(
        tahun = year(tanggal),
        bulan_ke = month(tanggal),
        wt = kasus_baru,
        name="Kasus Baru"
      )
    
    bulanan0_prov <- new_dataframe_prov %>% 
      count(
        tahun = year(tanggal),
        bulan_ke = month(tanggal),
        wt = sembuh,
        name="Sembuh"
      )
    
    bulanan1_prov <- new_dataframe_prov %>% 
      count(
        tahun = year(tanggal),
        bulan_ke = month(tanggal),
        wt = meninggal,
        name="Meninggal"
      )
    
    bulanan_prov$Sembuh<-bulanan0_prov$Sembuh
    
    bulanan_prov$Meninggal<-bulanan1_prov$Meninggal
    
    bulanan_prov <-
      bulanan_prov %>% 
      mutate(
        jumlah_KBbulanlalu = dplyr::lag(`Kasus Baru`, 1),
        jumlah_KBbulanlalu = ifelse(is.na(jumlah_KBbulanlalu), 0, jumlah_KBbulanlalu),
        lebih_baik = `Kasus Baru` < jumlah_KBbulanlalu
      )
    
    bulanan_prov$lebih_baik_ind <- ifelse(bulanan_prov$lebih_baik==T, "Ya", "Tidak")
    
    bulanan_prov$bulan <- ifelse(bulanan_prov$bulan_ke==1, "Januari",
                                 ifelse(bulanan_prov$bulan_ke==2,"Februari",
                                        ifelse(bulanan_prov$bulan_ke==3,"Maret",
                                               ifelse(bulanan_prov$bulan_ke==4,"April",
                                                      ifelse(bulanan_prov$bulan_ke==5,"Mei",
                                                             ifelse(bulanan_prov$bulan_ke==6,"Juni",
                                                                    ifelse(bulanan_prov$bulan_ke==7,"Juli",
                                                                           ifelse(bulanan_prov$bulan_ke==8,"Agustus",
                                                                                  ifelse(bulanan_prov$bulan_ke==9,"September",
                                                                                         ifelse(bulanan_prov$bulan_ke==10,"Oktober",
                                                                                                ifelse(bulanan_prov$bulan_ke==11,"November","Desember"
                                                                                                )))))))))))
    bulanan_prov$bulan <- factor(bulanan_prov$bulan, levels = bulanan_prov$bulan)
    
    fig = plot_ly(bulanan_prov, x=~bulan, y=~`Kasus Baru`, type="bar", name = "Kasus Baru", marker = list(color = 'rgb(22, 96, 167)'))
    fig <- fig %>% add_trace(y = ~Sembuh, name = 'Sembuh', marker = list(color = 'rgb(96, 167, 22)'))
    fig <- fig %>% add_trace(y = ~Meninggal, name = 'Meninggal', marker = list(color = 'rgb(167, 1, 1)'))
    
    fig <- fig %>% layout(
      title = paste("Tren Bulanan COVID-19 Provinsi", provinsi),
      xaxis = list(title = 'Bulan'),
      yaxis = list(title = 'Jumlah'),
      barmode = 'group')
    
    fig
    
  })
  
  output$barnasplot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    dataset_use <- data_raw$list_perkembangan
    
    dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
    dataset_use$tanggal = as.Date(dataset_use$tanggal)
    
    dataframe_prov <- data.frame(dataset_use)
    
    new_dataframe_prov <-
      dataframe_prov[,1:4]%>% 
      rename(
        kasus_baru = KASUS,
        meninggal = MENINGGAL,
        sembuh = SEMBUH
      ) %>% 
      mutate(
        tanggal = as.Date(tanggal)
      )
    
    fig <- plot_ly(new_dataframe_prov, x = ~tanggal, y = ~kasus_baru,
                   name="Kasus Baru",
                   marker = list(color = 'rgb(22, 96, 167)'),
                   type = 'bar')
    fig <- fig %>% add_trace(y = ~sembuh, 
                             name = "Sembuh",
                             marker = list(color = 'rgb(96, 167, 22)')) 
    fig <- fig %>% add_trace(y = ~meninggal, 
                             name = "Meninggal",
                             marker = list(color = 'rgb(167, 1, 1)'))
    fig <- fig %>% layout(title = paste("Perkembangan Kasus Per-Hari Gabungan Provinsi", provinsi),
                          xaxis = list(title = "Tanggal"),
                          yaxis = list (title = "Jumlah"),
                          barmode = 'stack')
    
    fig
  })
  
  output$jkNas1_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    kelamin_kasus<-data_raw$data$kasus$jenis_kelamin$list_data
    kelamin_perawatan<-data_raw$data$perawatan$jenis_kelamin$list_data
    kelamin_sembuh<-data_raw$data$sembuh$jenis_kelamin$list_data
    kelamin_meninggal<-data_raw$data$meninggal$jenis_kelamin$list_data
    
    kelamin_semua0<-merge(kelamin_kasus,kelamin_perawatan, by="key")
    kelamin_semua1<-merge(kelamin_sembuh,kelamin_meninggal, by="key")
    
    kelamin_semua<-merge(kelamin_semua0,kelamin_semua1, by="key")
    kelamin_semua$Jumlah <- (kelamin_semua$doc_count.x.x+kelamin_semua$doc_count.y.x+kelamin_semua$doc_count.x.y+kelamin_semua$doc_count.y.y)/4
    
    fig <- plot_ly(kelamin_semua, x=~key, y=~Jumlah, type="bar", color=~key)
    fig <- fig %>% layout(xaxis = list(title = "Jumlah"),
                          yaxis = list (title = ""),
                          barmode = 'stack')
    fig <- fig %>% layout(annotations =  
                            list(x = 1, y = -0.12, text = "Sumber: https://data.covid19.go.id", 
                                 showarrow = F, xref='paper', yref='paper', 
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                 font=list(size=12, color="black"))
    )
    fig
  })
  
  output$jkNas2_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    kelamin_kasus<-data_raw$data$kasus$jenis_kelamin$list_data
    kelamin_perawatan<-data_raw$data$perawatan$jenis_kelamin$list_data
    kelamin_sembuh<-data_raw$data$sembuh$jenis_kelamin$list_data
    kelamin_meninggal<-data_raw$data$meninggal$jenis_kelamin$list_data
    
    kelamin_semua0<-merge(kelamin_kasus,kelamin_perawatan, by="key")
    kelamin_semua1<-merge(kelamin_sembuh,kelamin_meninggal, by="key")
    
    kelamin_semua<-merge(kelamin_semua0,kelamin_semua1, by="key")
    kelamin_semua$Jumlah <- kelamin_semua$doc_count.x.x+kelamin_semua$doc_count.y.x+kelamin_semua$doc_count.x.y+kelamin_semua$doc_count.y.y
    
    fig <- plot_ly(kelamin_kasus, x=~key, y=~doc_count, type="bar", color=~key)
    fig <- fig %>% layout(xaxis = list(title = "Jumlah"),
                          yaxis = list (title = ""),
                          barmode = 'stack')
    fig <- fig %>% layout(annotations =  
                            list(x = 1, y = -0.12, text = "Sumber: https://data.covid19.go.id", 
                                 showarrow = F, xref='paper', yref='paper', 
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                 font=list(size=12, color="black"))
    )
    fig
  })
  
  output$jkNas3_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    kelamin_kasus<-data_raw$data$kasus$jenis_kelamin$list_data
    kelamin_perawatan<-data_raw$data$perawatan$jenis_kelamin$list_data
    kelamin_sembuh<-data_raw$data$sembuh$jenis_kelamin$list_data
    kelamin_meninggal<-data_raw$data$meninggal$jenis_kelamin$list_data
    
    kelamin_semua0<-merge(kelamin_kasus,kelamin_perawatan, by="key")
    kelamin_semua1<-merge(kelamin_sembuh,kelamin_meninggal, by="key")
    
    kelamin_semua<-merge(kelamin_semua0,kelamin_semua1, by="key")
    kelamin_semua$Jumlah <- kelamin_semua$doc_count.x.x+kelamin_semua$doc_count.y.x+kelamin_semua$doc_count.x.y+kelamin_semua$doc_count.y.y
    
    fig <- plot_ly(kelamin_perawatan, x=~key, y=~doc_count, type="bar", color=~key)
    fig <- fig %>% layout(xaxis = list(title = "Jumlah"),
                          yaxis = list (title = ""),
                          barmode = 'stack')
    fig <- fig %>% layout(annotations =  
                            list(x = 1, y = -0.12, text = "Sumber: https://data.covid19.go.id", 
                                 showarrow = F, xref='paper', yref='paper', 
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                 font=list(size=12, color="black"))
    )
    fig
  })
  
  output$jkNas4_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    kelamin_kasus<-data_raw$data$kasus$jenis_kelamin$list_data
    kelamin_perawatan<-data_raw$data$perawatan$jenis_kelamin$list_data
    kelamin_sembuh<-data_raw$data$sembuh$jenis_kelamin$list_data
    kelamin_meninggal<-data_raw$data$meninggal$jenis_kelamin$list_data
    
    kelamin_semua0<-merge(kelamin_kasus,kelamin_perawatan, by="key")
    kelamin_semua1<-merge(kelamin_sembuh,kelamin_meninggal, by="key")
    
    kelamin_semua<-merge(kelamin_semua0,kelamin_semua1, by="key")
    kelamin_semua$Jumlah <- kelamin_semua$doc_count.x.x+kelamin_semua$doc_count.y.x+kelamin_semua$doc_count.x.y+kelamin_semua$doc_count.y.y
    
    fig <- plot_ly(kelamin_sembuh, x=~key, y=~doc_count, type="bar", color=~key)
    fig <- fig %>% layout(xaxis = list(title = "Jumlah"),
                          yaxis = list (title = ""),
                          barmode = 'stack')
    fig <- fig %>% layout(annotations =  
                            list(x = 1, y = -0.12, text = "Sumber: https://data.covid19.go.id", 
                                 showarrow = F, xref='paper', yref='paper', 
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                 font=list(size=12, color="black"))
    )
    fig
  })
  
  output$jkNas5_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    kelamin_kasus<-data_raw$data$kasus$jenis_kelamin$list_data
    kelamin_perawatan<-data_raw$data$perawatan$jenis_kelamin$list_data
    kelamin_sembuh<-data_raw$data$sembuh$jenis_kelamin$list_data
    kelamin_meninggal<-data_raw$data$meninggal$jenis_kelamin$list_data
    
    kelamin_semua0<-merge(kelamin_kasus,kelamin_perawatan, by="key")
    kelamin_semua1<-merge(kelamin_sembuh,kelamin_meninggal, by="key")
    
    kelamin_semua<-merge(kelamin_semua0,kelamin_semua1, by="key")
    kelamin_semua$Jumlah <- kelamin_semua$doc_count.x.x+kelamin_semua$doc_count.y.x+kelamin_semua$doc_count.x.y+kelamin_semua$doc_count.y.y
    
    fig <- plot_ly(kelamin_meninggal, x=~key, y=~doc_count, type="bar", color=~key)
    fig <- fig %>% layout(xaxis = list(title = "Jumlah"),
                          yaxis = list (title = ""),
                          barmode = 'stack')
    fig <- fig %>% layout(annotations =  
                            list(x = 1, y = -0.12, text = "Sumber: https://data.covid19.go.id", 
                                 showarrow = F, xref='paper', yref='paper', 
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                 font=list(size=12, color="black"))
    )
    fig
  })
  
  output$KUNas1_plot_a <- renderPlot({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KU_kasus<-data_raw$data$kasus$kelompok_umur$list_data
    KU_perawatan<-data_raw$data$perawatan$kelompok_umur$list_data
    KU_sembuh<-data_raw$data$sembuh$kelompok_umur$list_data
    KU_meninggal<-data_raw$data$meninggal$kelompok_umur$list_data
    
    KU_semua0<-merge(KU_kasus,KU_perawatan, by="key")
    KU_semua1<-merge(KU_sembuh,KU_meninggal, by="key")
    
    KU_semua<-merge(KU_semua0,KU_semua1, by="key")
    KU_semua$Jumlah <- (KU_semua$doc_count.x.x+KU_semua$doc_count.y.x+KU_semua$doc_count.x.y+KU_semua$doc_count.y.y)/4
    
    KU_semua$Jumlah<-round(KU_semua$Jumlah, digits=2)
    names(KU_semua)[names(KU_semua) == "key"] <- "Kelompok Umur"
    
    KU_semua <- KU_semua %>%
      arrange(desc(`Kelompok Umur`)) %>%
      mutate(lab.ypos = cumsum(Jumlah) - 0.5*Jumlah)
    KU_semua
    
    ggplot(KU_semua, aes(x = "", y = Jumlah, fill = `Kelompok Umur`)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste(Jumlah,"%")), color = "white")+
      theme_void()
    
  })
  
  output$KUNas2_plot_a <- renderPlot({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KU_kasus<-data_raw$data$kasus$kelompok_umur$list_data
    KU_perawatan<-data_raw$data$perawatan$kelompok_umur$list_data
    KU_sembuh<-data_raw$data$sembuh$kelompok_umur$list_data
    KU_meninggal<-data_raw$data$meninggal$kelompok_umur$list_data
    
    KU_kasus$doc_count<-round(KU_kasus$doc_count, digits=2)
    names(KU_kasus)[names(KU_kasus) == "key"] <- "Kelompok Umur"
    
    KU_kasus <- KU_kasus %>%
      arrange(desc(`Kelompok Umur`)) %>%
      mutate(lab.ypos = cumsum(doc_count) - 0.5*doc_count)
    KU_kasus
    
    ggplot(KU_kasus, aes(x = "", y = doc_count, fill = `Kelompok Umur`)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste(doc_count,"%")), color = "white")+
      theme_void()
    
  })
  
  output$KUNas3_plot_a <- renderPlot({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KU_kasus<-data_raw$data$kasus$kelompok_umur$list_data
    KU_perawatan<-data_raw$data$perawatan$kelompok_umur$list_data
    KU_sembuh<-data_raw$data$sembuh$kelompok_umur$list_data
    KU_meninggal<-data_raw$data$meninggal$kelompok_umur$list_data
    
    KU_perawatan$doc_count<-round(KU_perawatan$doc_count, digits=2)
    names(KU_perawatan)[names(KU_perawatan) == "key"] <- "Kelompok Umur"
    
    KU_perawatan <- KU_perawatan %>%
      arrange(desc(`Kelompok Umur`)) %>%
      mutate(lab.ypos = cumsum(doc_count) - 0.5*doc_count)
    KU_perawatan
    
    ggplot(KU_perawatan, aes(x = "", y = doc_count, fill = `Kelompok Umur`)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste(doc_count,"%")), color = "white")+
      theme_void()
    
  })
  
  output$KUNas4_plot_a <- renderPlot({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KU_kasus<-data_raw$data$kasus$kelompok_umur$list_data
    KU_perawatan<-data_raw$data$perawatan$kelompok_umur$list_data
    KU_sembuh<-data_raw$data$sembuh$kelompok_umur$list_data
    KU_meninggal<-data_raw$data$meninggal$kelompok_umur$list_data
    
    KU_sembuh$doc_count<-round(KU_sembuh$doc_count, digits=2)
    names(KU_sembuh)[names(KU_sembuh) == "key"] <- "Kelompok Umur"
    
    KU_sembuh <- KU_sembuh %>%
      arrange(desc(`Kelompok Umur`)) %>%
      mutate(lab.ypos = cumsum(doc_count) - 0.5*doc_count)
    KU_sembuh
    
    ggplot(KU_sembuh, aes(x = "", y = doc_count, fill = `Kelompok Umur`)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste(doc_count,"%")), color = "white")+
      theme_void()
    
  })
  
  output$KUNas5_plot_a <- renderPlot({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KU_kasus<-data_raw$data$kasus$kelompok_umur$list_data
    KU_perawatan<-data_raw$data$perawatan$kelompok_umur$list_data
    KU_sembuh<-data_raw$data$sembuh$kelompok_umur$list_data
    KU_meninggal<-data_raw$data$meninggal$kelompok_umur$list_data
    
    KU_meninggal$doc_count<-round(KU_meninggal$doc_count, digits=2)
    names(KU_meninggal)[names(KU_meninggal) == "key"] <- "Kelompok Umur"
    
    KU_meninggal <- KU_meninggal %>%
      arrange(desc(`Kelompok Umur`)) %>%
      mutate(lab.ypos = cumsum(doc_count) - 0.5*doc_count)
    KU_meninggal
    
    ggplot(KU_meninggal, aes(x = "", y = doc_count, fill = `Kelompok Umur`)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = paste(doc_count,"%")), color = "white")+
      theme_void()
    
  })
  
  
  output$GPNas1_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    gejala<-data_raw$data$kasus$gejala$list_data
    
    gejala$doc_count<-round(gejala$doc_count, digits=2)
    names(gejala)[names(gejala) == "key"] <- "Gejala"
    
    fig<-plot_ly(gejala, x=~Gejala, y=~doc_count, color=~Gejala, type='bar',
                 text = ~doc_count, name=~Gejala, textposition = 'outside')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(xaxis = list(title = "",
                                       zeroline = FALSE, 
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       showgrid = FALSE), 
                          yaxis = list(title = "Jumlah"))
    
    
    fig
    
  })
  
  
  output$GPNas2_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    gejala<-data_raw$data$sembuh$gejala$list_data
    
    gejala$doc_count<-round(gejala$doc_count, digits=2)
    names(gejala)[names(gejala) == "key"] <- "Gejala"
    
    fig<-plot_ly(gejala, x=~Gejala, y=~doc_count, color=~Gejala, type='bar',
                 text = ~doc_count, name=~Gejala, textposition = 'outside')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(xaxis = list(title = "",
                                       zeroline = FALSE, 
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       showgrid = FALSE), 
                          yaxis = list(title = "Jumlah"))
    
    
    fig
    
  })
  
  output$GPNas3_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    gejala<-data_raw$data$meninggal$gejala$list_data
    
    gejala$doc_count<-round(gejala$doc_count, digits=2)
    names(gejala)[names(gejala) == "key"] <- "Gejala"
    
    fig<-plot_ly(gejala, x=~Gejala, y=~doc_count, color=~Gejala, type='bar',
                 text = ~doc_count, name=~Gejala, textposition = 'outside')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(xaxis = list(title = "",
                                       zeroline = FALSE, 
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       showgrid = FALSE), 
                          yaxis = list(title = "Jumlah"))
    
    
    fig
    
  })
  
  output$KPNas1_plot_a <- renderPlotly({
    provinsi <- input$prov
    link <- "https://data.covid19.go.id/public/api/prov_detail_"
    json_t <- ".json"
    url_prov <- paste(link,input$prov,json_t,sep="")
    url_use <- GET(url_prov)
    data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
    
    KP<-data_raw$data$kasus$kondisi_penyerta$list_data
    
    KP$doc_count<-round(KP$doc_count, digits=2)
    names(KP)[names(KP) == "key"] <- "Kondisi Penyerta"
    
    fig<-plot_ly(KP, x=~`Kondisi Penyerta`, y=~doc_count, color=~`Kondisi Penyerta`, type='bar',
                 text = ~doc_count, name=~`Kondisi Penyerta`, textposition = 'outside')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(xaxis = list(title = "",
                                       zeroline = FALSE, 
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       showgrid = FALSE), 
                          yaxis = list(title = "Jumlah"))
    
    
    fig
    
  })
  
  
}