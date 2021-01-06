library(shiny)                   
library(shinydashboard)          
library(tidyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(leaflet)
library(dplyr)                   
library(stringr)                 
library(purrr)                   
library(rlang)                   
library(leaflet)
library(DT)                      
library(r2d3)                    
library(DBI)                     
library(dbplyr)                  
library(RSQLite) 
library(lubridate)


# Olah Data Global
# Membuka dataset
retail<-read.csv('retail_pakai.csv', header = T,sep = ",")
#Membuat R Database Memory
mydb <- dbConnect(RSQLite::SQLite(), ":memory:")
#Mengunggah dataset ke database system
dbWriteTable(mydb, "retail", retail)
#Mengakses dataset dari database
retail<-dbGetQuery(mydb, 'SELECT * FROM retail LIMIT 9994') #nama dataset dan jumlah baris
#rename
data <- retail
#Mebuat dataframe label
label_bulan = list("Januari", "Februari", "Maret", 
                   "April", "Mei", "Juni",
                   "Juli", "Agustus", "September",
                   "Oktober", "November", "Desember") 
label_angka = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
df_label <- data.frame(unlist(label_bulan), unlist(label_angka))
names(df_label) <- c("label_bulan", "label_angka") 
