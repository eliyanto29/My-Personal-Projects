###################################################################################################
#----------------------------------------Crawling Data Twitter------------------------------------#
#--------------------------------------------Joko Eliyanto----------------------------------------#
###################################################################################################

#----------------------------------Install package jika diperlukan--------------------------------#
install.packages("here")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")

#---------------------------------------Mengaktifkan Library--------------------------------------#
library(here)
library(twitteR)
library(ROAuth)
library(RCurl)

#Note:   Menentukan folder kerja
setwd(here())

#Note:   Download sertifikat dari curl
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#Note:   Meminta izin kepada twitter
requrl <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "phgDZq2Rrb6PjIhRNhiRWXvII"
CUSTOMER_SECRET <- "DTxC7YarQOz1rFfmwM0BlZ13Zbk27ml2KUawhwve0i8HHEETCl"
ACCES_TOKEN <- "955350081544142848-OzXYrlANBDHGSeA7p6Jokw07Q7FXVPa"
ACCES_secret <- "YtCTOsit1DchlWii7DKBilY5iSkXvuJ3LCJmuxQosuIKO"

#Note:   Men-setup authorisasi
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCES_TOKEN, ACCES_secret)
1

#Note:   Mengambil data dari twitter

bd<-searchTwitter("PSBB", n=15000, lang='id', locale=F)
df_id<-do.call("rbind", lapply(bd, as.data.frame))
View(df_id)

#Note:   Mengeksport ke dalam csv file
write.csv(df_id, file='F:/PSBB.csv', row.names = F)

#Note:   Mengambil Twiter berdasarkan range waktu
pemiluapril2019<-searchTwitter('covid19',since="2020-6-1", until="2020-6-30")
df_id<-do.call("rbind", lapply(pemiluapril2019, as.data.frame))
View(df_id)

#Note:   Mengeksport ke dalam csv file
write.csv(df_id, file='F:/PSBB.csv', row.names = F)