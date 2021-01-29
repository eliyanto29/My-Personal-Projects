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

### Data
# Olah Data

url_use <- GET("https://data.covid19.go.id/public/api/update.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)
tanggal_upadate<-data_raw$update$penambahan$created
tanggal_upadate = as.Date(tanggal_upadate)
tanggal_u <- day(tanggal_upadate)
bulan_u <- month(tanggal_upadate)
tahun_u <- year(tanggal_upadate)

length(data_raw)
names(data_raw)
cov_id_update <- data_raw$update 
lapply(cov_id_update, names)
tanggal   <- cov_id_update$harian$key
tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01")
tanggal = as.Date(tanggal)
positif   <- cov_id_update$harian$jumlah_positif
dirawat   <- cov_id_update$harian$jumlah_dirawat
meninggal <- cov_id_update$harian$jumlah_meninggal
sembuh    <- cov_id_update$harian$jumlah_sembuh
positif_kum   <- cov_id_update$harian$jumlah_positif_kum
dirawat_kum   <- cov_id_update$harian$jumlah_dirawat_kum
meninggal_kum <- cov_id_update$harian$jumlah_meninggal_kum
sembuh_kum    <- cov_id_update$harian$jumlah_sembuh_kum


dataframe <- data.frame(tanggal, 
                        positif, 
                        meninggal, 
                        sembuh,
                        positif_kum, 
                        sembuh_kum, 
                        meninggal_kum)

names(dataframe)[names(dataframe) == "value"]   <- "KASUS"
names(dataframe)[names(dataframe) == "value.1"] <- "MENINGGAL"
names(dataframe)[names(dataframe) == "value.2"] <- "SEMBUH"
names(dataframe)[names(dataframe) == "value.3"] <- "AKUMULASI_KASUS"
names(dataframe)[names(dataframe) == "value.4"] <- "AKUMULASI_SEMBUH"
names(dataframe)[names(dataframe) == "value.5"] <- "AKUMULASI_MENINGGAL"

new_dataframe <-
  dataframe[,1:4]%>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.Date(tanggal)
  )

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

new_dataframe

#-------------------------------------------------- DATA PER PROVINSI ------------------------------------------------#
#1 ACEH
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_ACEH.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_1 <- data.frame(dataset_use)

kasus_1 <- sum(dataframe_1$KASUS)
sembuh_1<- sum(dataframe_1$SEMBUH)
meninggal_1<- sum(dataframe_1$MENINGGAL)

#2 SUMATERA UTARA
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SUMATERA_UTARA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_2 <- data.frame(dataset_use)

kasus_2 <- sum(dataframe_2$KASUS)
sembuh_2<- sum(dataframe_2$SEMBUH)
meninggal_2<- sum(dataframe_2$MENINGGAL)

#3 SUMATERA BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SUMATERA_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_3 <- data.frame(dataset_use)

kasus_3 <- sum(dataframe_3$KASUS)
sembuh_3<- sum(dataframe_3$SEMBUH)
meninggal_3<- sum(dataframe_3$MENINGGAL)

#4 RIAU
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_RIAU.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_4 <- data.frame(dataset_use)

kasus_4 <- sum(dataframe_4$KASUS)
sembuh_4<- sum(dataframe_4$SEMBUH)
meninggal_4<- sum(dataframe_4$MENINGGAL)

#5 JAMBI
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_JAMBI.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_5 <- data.frame(dataset_use)

kasus_5 <- sum(dataframe_5$KASUS)
sembuh_5<- sum(dataframe_5$SEMBUH)
meninggal_5<- sum(dataframe_5$MENINGGAL)

#6 SUMATERA SELATAN
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SUMATERA_SELATAN.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_6 <- data.frame(dataset_use)

kasus_6 <- sum(dataframe_6$KASUS)
sembuh_6<- sum(dataframe_6$SEMBUH)
meninggal_6<- sum(dataframe_6$MENINGGAL)

#7 KEPULAUAN RIAU
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_BENGKULU.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_7 <- data.frame(dataset_use)

kasus_7 <- sum(dataframe_7$KASUS)
sembuh_7<- sum(dataframe_7$SEMBUH)
meninggal_7<- sum(dataframe_7$MENINGGAL)

#8 LAMPUNG
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_LAMPUNG.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_8 <- data.frame(dataset_use)

kasus_8 <- sum(dataframe_8$KASUS)
sembuh_8<- sum(dataframe_8$SEMBUH)
meninggal_8<- sum(dataframe_8$MENINGGAL)

#9 BANGKA BELITUNG
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KEPULAUAN_BANGKA_BELITUNG.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_9 <- data.frame(dataset_use)

kasus_9 <- sum(dataframe_9$KASUS)
sembuh_9<- sum(dataframe_9$SEMBUH)
meninggal_9<- sum(dataframe_9$MENINGGAL)

#10 KEPULAUAN RIAU
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KEPULAUAN_RIAU.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_10 <- data.frame(dataset_use)

kasus_10 <- sum(dataframe_10$KASUS)
sembuh_10<- sum(dataframe_10$SEMBUH)
meninggal_10<- sum(dataframe_10$MENINGGAL)


#11 BANTEN
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_11 <- data.frame(dataset_use)

kasus_11 <- sum(dataframe_11$KASUS)
sembuh_11<- sum(dataframe_11$SEMBUH)
meninggal_11<- sum(dataframe_11$MENINGGAL)

#12 JAWA BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_12 <- data.frame(dataset_use)

kasus_12 <- sum(dataframe_12$KASUS)
sembuh_12<- sum(dataframe_12$SEMBUH)
meninggal_12<- sum(dataframe_12$MENINGGAL)

#13 JAWA TENGAH
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_13 <- data.frame(dataset_use)

kasus_13 <- sum(dataframe_13$KASUS)
sembuh_13 <- sum(dataframe_13$SEMBUH)
meninggal_13 <- sum(dataframe_13$MENINGGAL)

#14 DAERAH ISTIMEWA YOGYAKARTA
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_DAERAH_ISTIMEWA_YOGYAKARTA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_14 <- data.frame(dataset_use)

kasus_14 <- sum(dataframe_14$KASUS)
sembuh_14 <- sum(dataframe_14$SEMBUH)
meninggal_14 <- sum(dataframe_14$MENINGGAL)

#15 JAWA TIMUR
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_15 <- data.frame(dataset_use)

kasus_15 <- sum(dataframe_15$KASUS)
sembuh_15 <- sum(dataframe_15$SEMBUH)
meninggal_15 <- sum(dataframe_15$MENINGGAL)

#16 BANTEN
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_BANTEN.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_16 <- data.frame(dataset_use)

kasus_16 <- sum(dataframe_16$KASUS)
sembuh_16 <- sum(dataframe_16$SEMBUH)
meninggal_16 <- sum(dataframe_16$MENINGGAL)

#17 BALI
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_BALI.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_17 <- data.frame(dataset_use)

kasus_17 <- sum(dataframe_17$KASUS)
sembuh_17 <- sum(dataframe_17$SEMBUH)
meninggal_17 <- sum(dataframe_17$MENINGGAL)

#18 NUSA TENGGARA BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_NUSA_TENGGARA_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_18 <- data.frame(dataset_use)

kasus_18 <- sum(dataframe_18$KASUS)
sembuh_18<- sum(dataframe_18$SEMBUH)
meninggal_18<- sum(dataframe_18$MENINGGAL)

#19 NUSA TENGGARA TMUR
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_NUSA_TENGGARA_TIMUR.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_19 <- data.frame(dataset_use)

kasus_19 <- sum(dataframe_19$KASUS)
sembuh_19 <- sum(dataframe_19$SEMBUH)
meninggal_19 <- sum(dataframe_19$MENINGGAL)

#20 KALIMANTAN BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_20 <- data.frame(dataset_use)

kasus_20 <- sum(dataframe_20$KASUS)
sembuh_20 <- sum(dataframe_20$SEMBUH)
meninggal_20 <- sum(dataframe_20$MENINGGAL)

#21 KALIMANTAN TENGAH
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TENGAH.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_21 <- data.frame(dataset_use)

kasus_21 <- sum(dataframe_21$KASUS)
sembuh_21<- sum(dataframe_21$SEMBUH)
meninggal_21<- sum(dataframe_21$MENINGGAL)

#22 KALIMANTAN SELATAN
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_SELATAN.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_22 <- data.frame(dataset_use)

kasus_22 <- sum(dataframe_22$KASUS)
sembuh_22<- sum(dataframe_22$SEMBUH)
meninggal_22<- sum(dataframe_22$MENINGGAL)

#23 KALIMANTAN TIMUR 
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TIMUR.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_23 <- data.frame(dataset_use)

kasus_23 <- sum(dataframe_23$KASUS)
sembuh_23<- sum(dataframe_23$SEMBUH)
meninggal_23<- sum(dataframe_23$MENINGGAL)

#24 KALIMANTAN UTARA 
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_UTARA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_24 <- data.frame(dataset_use)

kasus_24 <- sum(dataframe_24$KASUS)
sembuh_24<- sum(dataframe_24$SEMBUH)
meninggal_24<- sum(dataframe_24$MENINGGAL)

#25 SULAWESI UTARA 
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_UTARA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_25 <- data.frame(dataset_use)

kasus_25 <- sum(dataframe_25$KASUS)
sembuh_25<- sum(dataframe_25$SEMBUH)
meninggal_25<- sum(dataframe_25$MENINGGAL)

#26 SULAWESI TENGAH
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_TENGAH.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_26 <- data.frame(dataset_use)

kasus_26 <- sum(dataframe_26$KASUS)
sembuh_26<- sum(dataframe_26$SEMBUH)
meninggal_26<- sum(dataframe_26$MENINGGAL)

#27 SULAWESI SELATAN
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_SELATAN.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_27 <- data.frame(dataset_use)

kasus_27 <- sum(dataframe_27$KASUS)
sembuh_27<- sum(dataframe_27$SEMBUH)
meninggal_27<- sum(dataframe_27$MENINGGAL)

#28 SULAWESI TENGGARA
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_TENGGARA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_28 <- data.frame(dataset_use)

kasus_28 <- sum(dataframe_28$KASUS)
sembuh_28 <- sum(dataframe_28$SEMBUH)
meninggal_28 <- sum(dataframe_28$MENINGGAL)

#29 GORONTALO
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_GORONTALO.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_29 <- data.frame(dataset_use)

kasus_29 <- sum(dataframe_29$KASUS)
sembuh_29 <- sum(dataframe_29$SEMBUH)
meninggal_29 <- sum(dataframe_29$MENINGGAL)

#30 SULAWESI BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_SULAWESI_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_30 <- data.frame(dataset_use)

kasus_30 <- sum(dataframe_30$KASUS)
sembuh_30 <- sum(dataframe_30$SEMBUH)
meninggal_30 <- sum(dataframe_30$MENINGGAL)

#31 MALUKU
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_MALUKU.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_31 <- data.frame(dataset_use)

kasus_31 <- sum(dataframe_31$KASUS)
sembuh_31<- sum(dataframe_31$SEMBUH)
meninggal_31<- sum(dataframe_31$MENINGGAL)

#32 MALUKU UTARA
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_MALUKU_UTARA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_32 <- data.frame(dataset_use)

kasus_32 <- sum(dataframe_32$KASUS)
sembuh_32<- sum(dataframe_32$SEMBUH)
meninggal_32<- sum(dataframe_32$MENINGGAL)

#33 PAPUA BARAT
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_PAPUA_BARAT.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_33 <- data.frame(dataset_use)

kasus_33 <- sum(dataframe_33$KASUS)
sembuh_33<- sum(dataframe_33$SEMBUH)
meninggal_33<- sum(dataframe_33$MENINGGAL)

#34 PAPUA 
url_use <- GET("https://data.covid19.go.id/public/api/prov_detail_PAPUA.json")
data_raw <- content(url_use, as = "parsed", simplifyVector = TRUE)

dataset_use <- data_raw$list_perkembangan

dataset_use$tanggal = as.POSIXct(dataset_use$tanggal / 1000, origin = "1970-01-01")
dataset_use$tanggal = as.Date(dataset_use$tanggal)

dataframe_34 <- data.frame(dataset_use)

kasus_34 <- sum(dataframe_34$KASUS)
sembuh_34 <- sum(dataframe_34$SEMBUH)
meninggal_34 <- sum(dataframe_34$MENINGGAL)

konfirmasi <- rbind(kasus_1,kasus_2,kasus_3,kasus_4,kasus_5,
                    kasus_6,kasus_7,kasus_8,kasus_9,kasus_10,
                    kasus_11,kasus_12,kasus_13,kasus_14,kasus_15,
                    kasus_16,kasus_17,kasus_18,kasus_19,kasus_20,
                    kasus_21,kasus_22,kasus_23,kasus_24,kasus_25,
                    kasus_26,kasus_27,kasus_28,kasus_29,kasus_30,
                    kasus_31,kasus_32,kasus_33,kasus_34)

sembuh <- rbind(sembuh_1, sembuh_2, sembuh_3, sembuh_4, sembuh_5,
                sembuh_6, sembuh_7, sembuh_8, sembuh_9, sembuh_10,
                sembuh_11, sembuh_12, sembuh_13, sembuh_14, sembuh_15,
                sembuh_16, sembuh_17, sembuh_18, sembuh_19, sembuh_20,
                sembuh_21, sembuh_22, sembuh_23, sembuh_24, sembuh_25,
                sembuh_26, sembuh_27, sembuh_28, sembuh_29, sembuh_30,
                sembuh_31, sembuh_32, sembuh_33, sembuh_34)

meninggal <- rbind(meninggal_1, meninggal_2, meninggal_3, meninggal_4, meninggal_5,
                   meninggal_6, meninggal_7, meninggal_8, meninggal_9, meninggal_10,
                   meninggal_11, meninggal_12, meninggal_13, meninggal_14, meninggal_15,
                   meninggal_16, meninggal_17, meninggal_18, meninggal_19, meninggal_20,
                   meninggal_21, meninggal_22, meninggal_23, meninggal_24, meninggal_25,
                   meninggal_26, meninggal_27, meninggal_28, meninggal_29, meninggal_30,
                   meninggal_31, meninggal_32, meninggal_33, meninggal_34)

data_map_prov_indo <- read.csv("https://raw.githubusercontent.com/eliyanto29/My-Personal-Projects/master/Sep_2_COVID_19_Indonesia_Shiny_DashBoard/map_data_Indonesia.csv")

data_map_prov_indo$konfirmasi<- konfirmasi
data_map_prov_indo$sembuh<- sembuh
data_map_prov_indo$meninggal<- meninggal

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


write.csv(new_dataframe,'CovidIndonesia.csv')
write.csv(data_map_prov_indo,'CovidIndonesiaProv.csv')

