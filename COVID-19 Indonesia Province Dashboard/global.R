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
library(ggplot2)
library(plotly)
library(rgdal)
library(leaflet)


# Olah Data Global

new_dataframe<-read.csv('CovidIndonesia.csv', header = T,sep = ",")
data_map_prov_indo<-read.csv('CovidIndonesiaProv.csv', header = T,sep = ",")

mydb <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(mydb, "new_dataframe", new_dataframe)
dbWriteTable(mydb, "data_map_prov_indo", data_map_prov_indo)

new_dataframe<-dbGetQuery(mydb, 'SELECT * FROM new_dataframe LIMIT 306')
data_map_prov_indo<-dbGetQuery(mydb, 'SELECT * FROM data_map_prov_indo LIMIT 34')

konfirmasi_data <- sum(data_map_prov_indo$konfirmasi)
sembuh_data <- sum(data_map_prov_indo$sembuh)
meninggal_data <- sum(data_map_prov_indo$meninggal)

provinsi10b <- data_map_prov_indo[order(-data_map_prov_indo$konfirmasi),]
row.names(provinsi10b) <- NULL
provinsi10b <- provinsi10b[1:10, ]

provinsi10b$prov <- factor(provinsi10b$prov, 
                           levels = provinsi10b$prov)

indo_covid_prov <- data.frame(data_map_prov_indo$prov, data_map_prov_indo$konfirmasi, data_map_prov_indo$sembuh, data_map_prov_indo$meninggal)
names(indo_covid_prov)[names(indo_covid_prov) == "data_map_prov_indo.prov"] <- "Provinsi"
names(indo_covid_prov)[names(indo_covid_prov) == "data_map_prov_indo.konfirmasi"] <- "Konfirmasi Positif"
names(indo_covid_prov)[names(indo_covid_prov) == "data_map_prov_indo.sembuh"] <- "Sembuh"
names(indo_covid_prov)[names(indo_covid_prov) == "data_map_prov_indo.meninggal"] <- "Meninggal"
rownames(indo_covid_prov) <- NULL

akumulasi <- 
  new_dataframe%>% 
  transmute(
    tanggal,
    akumulasi_kasus_aktif = cumsum(kasus_baru)- cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

bulanan <- new_dataframe %>% 
  count(
    tahun = year(tanggal),
    bulan_ke = month(tanggal),
    wt = kasus_baru,
    name="Kasus Baru"
  )

bulanan0 <- new_dataframe %>% 
  count(
    tahun = year(tanggal),
    bulan_ke = month(tanggal),
    wt = sembuh,
    name="Sembuh"
  )

bulanan1 <- new_dataframe %>% 
  count(
    tahun = year(tanggal),
    bulan_ke = month(tanggal),
    wt = meninggal,
    name="Meninggal"
  )

bulanan$Sembuh<-bulanan0$Sembuh

bulanan$Meninggal<-bulanan1$Meninggal

bulanan <-
  bulanan %>% 
  mutate(
    jumlah_KBbulanlalu = dplyr::lag(`Kasus Baru`, 1),
    jumlah_KBbulanlalu = ifelse(is.na(jumlah_KBbulanlalu), 0, jumlah_KBbulanlalu),
    lebih_baik = `Kasus Baru` < jumlah_KBbulanlalu
  )

bulanan$lebih_baik_ind <- ifelse(bulanan$lebih_baik==T, "Ya", "Tidak")

bulanan$bulan <- ifelse(bulanan$bulan_ke==1, "Januari",
                        ifelse(bulanan$bulan_ke==2,"Februari",
                               ifelse(bulanan$bulan_ke==3,"Maret",
                                      ifelse(bulanan$bulan_ke==4,"April",
                                             ifelse(bulanan$bulan_ke==5,"Mei",
                                                    ifelse(bulanan$bulan_ke==6,"Juni",
                                                           ifelse(bulanan$bulan_ke==7,"Juli",
                                                                  ifelse(bulanan$bulan_ke==8,"Agustus",
                                                                         ifelse(bulanan$bulan_ke==9,"September",
                                                                                ifelse(bulanan$bulan_ke==10,"Oktober",
                                                                                       ifelse(bulanan$bulan_ke==11,"November","Desember"
                                                                                       )))))))))))
bulanan$bulan <- factor(bulanan$bulan, levels = bulanan$bulan)

# Prepare the text for the tooltip:
mytext <- paste(
  "Provinsi ", data_map_prov_indo$prov, "<br/>",
  "Konfirmasi: ", data_map_prov_indo$konfirmasi, "<br/>", 
  "Sembuh: ", data_map_prov_indo$sembuh, "<br/>", 
  "Meninggal: ", data_map_prov_indo$meninggal, sep="") %>%
  lapply(htmltools::HTML)

mypalette <- colorBin( palette="Reds", domain=data_map_prov_indo$konfirmasi, na.color="transparent")