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


dashboardPage(dashboardHeader(title = "COVID-19 Indonesia Shiny Dashboard", titleWidth = 700), 
              
              sidebar <- dashboardSidebar(
                sidebarMenu(
                  menuItem("Beranda",  tabName = "beranda", icon = icon("dashboard")),
                  menuItem("Nasional",  tabName = "nasional", icon = icon("stats",lib='glyphicon')),
                  menuItem("Provinsi",  tabName = "provinsi", icon = icon("stats",lib='glyphicon')),
                  menuItem("Visit Developer Profile", icon = icon("send",lib='glyphicon'), 
                           href = "https://github.com/eliyanto29")
                )
              ), #Sidebar
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "beranda",
                          h1(),
                          mainPanel(uiOutput("plot1"))
                  ),
                  tabItem(tabName = "nasional",
                          fluidRow(
                            infoBoxOutput("tanggal_up"),
                            infoBoxOutput("bulan_up"),
                            infoBoxOutput("tahun_up")
                          ),
                          fluidRow(
                            valueBoxOutput("value1")
                            ,valueBoxOutput("value2")
                            ,valueBoxOutput("value3")
                          ),
                          fluidPage(
                            box(
                              title = "Peta Sebaran COVID-19 ",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              leafletOutput("mapplot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "10 Provinsi dengan Jumlah Kasus Baru Terbanyak",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotOutput("provinsi10b_plot")
                            )
                          ),
                          fluidPage(
                            box (title = "Data COVID-19 34 Provinsi Indonesia",
                                 width = 1150,
                                 status = "primary",
                                 solidHeader = TRUE, 
                                 collapsible = TRUE,
                                 DT::dataTableOutput("coviddata"))
                          ),
                          fluidPage(
                            box(
                              title = "Perkembangan Kasus Terkonfirmasi Positif COVID-19 Per-Hari ",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("konf_plot")
                            )
                          ),
                          
                          fluidPage(
                            box(
                              title = "Perkembangan Kasus Sembuh Per-Hari ",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("sembuh_plot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "Perkembangan Kasus Meninggal Per-Hari ",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("meninggal_plot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "Perkembangan Kasus Per-Hari Gabungan",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("gabungan1_plot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "Kasus Aktif, Sembuh, dan Meninggal Per-Hari",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("gabungan2_plot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "Kasus Aktif, Sembuh, dan Meninggal Per-Bulan",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE, 
                              plotlyOutput("gabungan3_plot")
                            )
                          ),
                          fluidPage(
                            box(
                              title = "Tren Nasional(Akumulasi Data)",
                              width = 1150,
                              status = "primary",
                              solidHeader = TRUE, 
                              collapsible = TRUE,
                              plotlyOutput("barnasplot")
                              )
                              
                          )
                          
                          
                ),
                tabItem(tabName = "provinsi",
                        box(
                          title = "Pilih Provinsi "
                          ,width = 1150
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE 
                          ,fluidPage(selectInput(inputId="prov",
                                                 label=" ",
                                                 choices = c("Aceh"="ACEH",
                                                             "Sumatera Utara"="SUMATERA_UTARA",
                                                             "Sumatera Barat"="SUMATERA_BARAT",
                                                             "Riau" = "RIAU",
                                                             "Jambi" = "JAMBI",
                                                             "Sumatera Selatan"= "SUMATERA_SELATAN",
                                                             "Bengkulu" = "BENGKULU",
                                                             "Lampung" = "LAMPUNG",
                                                             "Bangka Belitung"= "KEPULAUAN_BANGKA_BELITUNG",
                                                             "Kepulauan Riau" = "KEPULAUAN_RIAU",
                                                             "DKI Jakarta" = "DKI_JAKARTA",
                                                             "Jawa Barat" = "JAWA_BARAT",
                                                             "Jawa Tengah" = "JAWA_TENGAH", 
                                                             "D.I. Yogyakarta" = "DAERAH_ISTIMEWA_YOGYAKARTA",
                                                             "Jawa Timur" = "JAWA_TIMUR",
                                                             "Banten" = "BANTEN",
                                                             "Bali" = "BALI", 
                                                             "Nusa Tenggara Barat" = "NUSA_TENGGARA_BARAT",
                                                             "Nusa Tenggara Timur" = "NUSA_TENGGARA_TIMUR",
                                                             "Kalimantan Barat" = "KALIMANTAN_BARAT",
                                                             "Kalimantan Tengah" = "KALIMANTAN_TENGAH",
                                                             "Kalimantan Selatan" = "KALIMANTAN_SELATAN",
                                                             "Kalimantan Timur" = "KALIMANTAN_TIMUR",
                                                             "Kalimantan Utara" = "KALIMANTAN_UTARA",
                                                             "Sulawesi Utara" = "SULAWESI_UTARA",
                                                             "Sulawesi Tengah" = "SULAWESI_TENGAH",
                                                             "Sulawesi Selatan" = "SULAWESI_SELATAN",
                                                             "Sulawesi Tenggara" = "SULAWESI_TENGGARA",
                                                             "Gorontalo" = "GORONTALO",
                                                             "Sulawesi Barat" = "SULAWESI_BARAT",
                                                             "Maluku" = "MALUKU",
                                                             "Maluku Utara" = "MALUKU_UTARA",
                                                             "Papua Barat" = "PAPUA_BARAT",
                                                             "Papua" = "PAPUA"),
                                                 selected =  "Aceh",multiple = F))
                        ),
                        fluidRow(
                          valueBoxOutput("value1_a")
                          ,valueBoxOutput("value2_a")
                          ,valueBoxOutput("value3_a")
                        ),
                        fluidPage(
                          box (title = "Data COVID-19 Provinsi",
                               width = 1150,
                               status = "primary",
                               solidHeader = TRUE, 
                               collapsible = TRUE,
                               DT::dataTableOutput("coviddata_prov"))
                        ),
                        fluidPage(
                          box(
                            title = "Perkembangan Kasus Terkonfirmasi Positif COVID-19 Per-Hari ",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("konf_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Perkembangan Kasus Sembuh Per-Hari ",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("sembuh_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Perkembangan Kasus Meninggal Per-Hari ",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("meninggal_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Perkembangan Kasus Per-Hari Gabungan",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("gabungan1_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Kasus Aktif, Sembuh, dan Meninggal Per-Hari",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("gabungan2_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Kasus Aktif, Sembuh, dan Meninggal Per-Bulan",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotlyOutput("gabungan3_plot_a")
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Jenis Kelamin Positif COVID-19",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            tabsetPanel(
                              tabPanel("Semua",plotlyOutput("jkNas1_plot_a")),
                              tabPanel("Positif",plotlyOutput("jkNas2_plot_a")),
                              tabPanel("Dirawat/Isolasi Mandiri",plotlyOutput("jkNas3_plot_a")),
                              tabPanel("Sembuh",plotlyOutput("jkNas4_plot_a")),
                              tabPanel("Meninggal",plotlyOutput("jkNas5_plot_a"))
                            )
                            
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Kelompok Umur Positif COVID-19",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            tabsetPanel(
                              tabPanel("Semua",plotOutput("KUNas1_plot_a")),
                              tabPanel("Positif",plotOutput("KUNas2_plot_a")),
                              tabPanel("Dirawat/Isolasi Mandiri",plotOutput("KUNas3_plot_a")),
                              tabPanel("Sembuh",plotOutput("KUNas4_plot_a")),
                              tabPanel("Meninggal",plotOutput("KUNas5_plot_a"))
                            )
                            
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Gejala Positif COVID-19",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            tabsetPanel(
                              tabPanel("Positif",plotlyOutput("GPNas1_plot_a")),
                              tabPanel("Sembuh",plotlyOutput("GPNas2_plot_a")),
                              tabPanel("Meninggal",plotlyOutput("GPNas3_plot_a"))
                            )
                            
                          )
                        ),
                        fluidPage(
                          box(
                            title = "Kondisi Penyerta Positif COVID-19",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE,
                            plotlyOutput("KPNas1_plot_a")
                            )
                        ),
                        fluidPage(
                          box(
                            title = "Tren Provinsi(Akumulasi Data)",
                            width = 1150,
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE,
                            plotlyOutput("barnasplot_a")
                          )
                          
                        )
                        
                        ###
                        
                        
                )
              )
              ),#DashboardBody 
              
              
              skin='blue')



