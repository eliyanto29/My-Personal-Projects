library(shiny)                   
library(shinydashboard)          

library(dplyr)                   
library(stringr)                 
library(purrr)                   
library(rlang)                   

library(DT)                      
library(r2d3)                    

library(DBI)                     
library(dbplyr)                  
library(RSQLite) 

library(lubridate)
library(leaflet)
library(ggplot2)
library(plotly)


# Olah Data Global

retail=read.csv('retail_pakai.csv', header = T,sep = ",")
mydb <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(mydb, "retail", retail)

retail<-dbGetQuery(mydb, 'SELECT * FROM retail LIMIT 9994')

retail$Ship.Day <- 
  as.Date(as.character(retail$Ship.Date), format="%Y-%m-%d")-
  as.Date(as.character(retail$Order.Date), format="%Y-%m-%d")

retail$Order.Date <- as.POSIXct(retail$Order.Date, format = "%Y-%m-%d",tz="UTC")
retail$Ship.Date <- as.POSIXct(retail$Ship.Date, format = "%Y-%m-%d",tz="UTC")

retail$Ship.Day <- as.numeric(retail$Ship.Day)

#----------------Rata2_Keuntungan_Bulanan---------------
#Rata-rata Penjualan Keuntungan
Keuntungan_bulanan <- retail %>%
  mutate(Order.Date = month(Order.Date)) %>%
  group_by(Order.Date) %>%
  summarize(mean_Profit = mean(Profit))

#Rata-rata Penjualan Bulanan Overall
RKB<-round(mean(Keuntungan_bulanan$mean_Profit),2)
typeof(RKB)
#------------------Rata2_Waktu_Pengiriman---------------
#Rata-rata waktu pengiriman
RWP<-round(mean(retail$Ship.Day),2)
typeof(RWP)

#------------------Rata2_Quantitas---------------
#Rata-rata Quantitas Harian
Quantity_bulanan <- retail %>%
  mutate(Order.Date = day(Order.Date)) %>%
  group_by(Order.Date) %>%
  summarize(mean_Quantity = mean(Quantity))

RQB<-round(mean(Quantity_bulanan$mean_Quantity),2)
RQB

#Plotly output
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right")

# Line Plotly Rata-rata Penjualan Bulanan
Penjualan_bulanan <- retail %>%
  mutate(Order.Date = month(Order.Date)) %>%
  group_by(Order.Date) %>%
  summarize(mean_Sales = mean(Sales))
Salesku=Penjualan_bulanan$mean_Sales
Salesku

# Line Plotly Rata-rata Quantity Bulanan
Quantity_bulanan <- retail %>%
  mutate(Order.Date = month(Order.Date)) %>%
  group_by(Order.Date) %>%
  summarize(mean_Quantity = mean(Quantity))
Quan=Quantity_bulanan$mean_Quantity
Quan

# Line Plotly Rata-rata Profit Bulanan
Profit_bulanan <- retail %>%
  mutate(Order.Date = month(Order.Date)) %>%
  group_by(Order.Date) %>%
  summarize(mean_Quantity = mean(Quantity))
Prof=Profit_bulanan$mean_Quantity
Prof

fig1 <- plot_ly()
fig1 <- fig1 %>% add_lines(x = ~1:12, y = ~Salesku, name = "Sales")
fig1 <- fig1 %>% add_lines(x = ~1:12, y = ~Quan, name = "Quantity", yaxis = "y2")
fig1 <- fig1 %>% layout(
  title=NULL,
  yaxis2 = ay,
  yaxis = list(title="Sales"),
  xaxis = list(title="Month-"),
  legend = list(orientation = 'h')
)

fig1

Segment<-as.data.frame(table(retail$Segment))
Segment

Category<-as.data.frame(table(retail$Category))
Category

fig2 <- plot_ly()
fig2 <- fig2 %>% add_pie(data = Segment, labels = Segment$Var1, values = Segment$Freq,
                       name = "Segment", domain = list(row = 0, column = 0))
fig2 <- fig2 %>% add_pie(data = Category, labels = Category$Var1, values = Category$Freq,
                       name = "Category", domain = list(row = 0, column = 1))

fig2 <- fig2 %>% layout(showlegend = F,
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig2

SubCategory<-as.data.frame(table(retail$Sub.Category))
SubCategory

fig3 <- plot_ly(
  x = SubCategory$Var1,
  y = SubCategory$Freq,
  color = SubCategory$Var1,
  type = "bar"
)
fig3 <- fig3 %>% layout(showlegend = FALSE)
fig3

Customer<-as.data.frame(table(retail$Customer.ID))
DataKonsumen  <- Customer[order(-Customer$Freq),]
DataKonsumen  <- DataKonsumen [1:10,]
DataKonsumen

fig4 <- plot_ly(
  x = 1:10,
  y = DataKonsumen$Freq,
  color = DataKonsumen$Var1,
  labels = DataKonsumen$Var1,
  type = "bar"
)

fig4 <- fig4 %>%
  layout(
    xaxis = list(
      ticktext = DataKonsumen$Var1, 
      tickvals = 1:12,
      tickmode = "array"
    ))

fig4 <- fig4 %>% layout(showlegend = FALSE)
fig4

Product<-as.data.frame(table(retail$Product.Name))
DataProduk <- Product[order(-Product$Freq),]
DataProduk <- DataProduk[1:10,]
DataProduk <- DataProduk[order(DataProduk$Freq),]
DataProduk
fig5 <- plot_ly(x = DataProduk$Freq, 
                y = 1:10, 
                type = 'bar', 
                orientation = 'h')
fig5 <- fig5 %>%
  layout(
    yaxis = list(
      ticktext = DataProduk$Var1, 
      tickvals = 1:12,
      tickmode = "array"
    ))
fig5 <- fig5 %>% layout(showlegend = F)
fig5

Pilihan_Segmen<-as.data.frame(table(retail$Segment))
pilihan_segmen<-Pilihan_Segmen$Var1
pilihan_segmen<-as.character(pilihan_segmen)
pilihan_segmen<-append(pilihan_segmen, "Semua Segment")
pilihan_segmen

Pilihan_Category<-as.data.frame(table(retail$Category))
Pilihan_Category<-Pilihan_Category$Var1
pilihan_category<-as.character(Pilihan_Category)
pilihan_category<-append(pilihan_category, "Semua Kategori")
pilihan_category
