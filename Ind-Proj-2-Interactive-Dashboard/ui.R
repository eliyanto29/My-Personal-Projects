
                    # Title
ui <- dashboardPage(title = "Data Retail Dashboard",
                    
                    # Header
                    dashboardHeader(title = "Retail Dashboard",titleWidth = 200),
                    
                    # Sidebar of the Dashboard
                    dashboardSidebar(width=200,
                      sidebarMenu(
                        menuItem("Sales",
                                 icon = icon("money"),
                                 tabName ="sales"),
                        menuItem("Quantity",
                                 icon = icon("box"),
                                 tabName ="quantity"),
                        menuItem("Profit",
                                 icon = icon("database"),
                                 tabName ="profit"),
                        menuItem("Author",  
                                 tabName = "author", 
                                 icon = icon("user")),
                        menuItem("Visit Author GitHub Profile", 
                                 icon = icon("send",lib='glyphicon'), 
                                 href = "https://github.com/eliyanto29")
                      )# Sidebar menu
                    ), #DashboardSidebar
                    #
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="sales",
                                box(title = "", 
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = T, 
                                    collapsible = F,
                                    h1("Sales Report", align = "center")
                                    ),
                                fluidRow(
                                  column(4, 
                                         box(title = "Segment", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (radioButtons("segmen_pilihan1", 
                                                                     #key variabel *segmen_pilihan*
                                                                     "",
                                                                     #Label "Segment" tampil di layar
                                                                     choices=c("Consumer"      = "Consumer",
                                                                       "Corporate"     = "Corporate",
                                                                       "Home Office"   = "Home Office"),
                                                                     selected = "Corporate",
                                                                     inline=T)))
                                         ),
                                  column(4, 
                                         box(title = "Category", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Tahun
                                               selectInput("category_pilihan1", 
                                                           #key variabel *category_pilihan*,
                                                           "",
                                                           choices=c("Furniture", 
                                                                     "Office Supplies",
                                                                     "Technology"), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "Technology"
                                                           #Pilihan default
                                               )))
                                         ),
                                  column(4, 
                                         box(title = "Tahun", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Category
                                               selectInput("tahun_pilihan1", 
                                                           #key variabel *category_pilihan*
                                                           "",
                                                           choices=c(2014,
                                                                     2015,
                                                                     2016,
                                                                     2017), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "Technology"
                                                           #Pilihan default
                                               )))
                                         )),
                                #Membuat box untuk hasil output
                                box(title = "Sales", 
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotSales1"
                                    fluidPage (plotlyOutput("PlotSales1", height = 250))),
                                #Membuat box untuk hasil output
                                box(title = "Sub-Category",
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotSubCat1"
                                    fluidPage (plotlyOutput("PlotSubCat1", height = 250)))
                                ),
                        tabItem(tabName="quantity",
                                box(title = "", 
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = T, 
                                    collapsible = F,
                                    h1("Quantity Report", align = "center")
                                ),
                                fluidRow(
                                  column(4, 
                                         box(title = "Segment", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (radioButtons("segmen_pilihan2", 
                                                                     #key variabel *segmen_pilihan*
                                                                     "",
                                                                     #Label "Segment" tampil di layar
                                                                     choices=c("Consumer"      = "Consumer",
                                                                               "Corporate"     = "Corporate",
                                                                               "Home Office"   = "Home Office"),
                                                                     selected = "Corporate",
                                                                     inline=T)))
                                  ),
                                  column(4, 
                                         box(title = "Category", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Tahun
                                               selectInput("category_pilihan2", 
                                                           #key variabel *category_pilihan*,
                                                           "",
                                                           choices=c("Furniture", 
                                                                     "Office Supplies",
                                                                     "Technology"), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "Technology"
                                                           #Pilihan default
                                               )))
                                  ),
                                  column(4, 
                                         box(title = "Tahun", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Category
                                               selectInput("tahun_pilihan2", 
                                                           #key variabel *category_pilihan*
                                                           "",
                                                           choices=c(2014,
                                                                     2015,
                                                                     2016,
                                                                     2017), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "Technology"
                                                           #Pilihan default
                                               )))
                                  )),
                                box(title = "Quantity", 
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotSales1"
                                    fluidPage (plotlyOutput("PlotQuantity2", height = 250))),
                                #Membuat box untuk hasil output
                                box(title = "Sub-Category",
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotSubCat1"
                                    fluidPage (plotlyOutput("PlotSubCat2", height = 250)))),
                        tabItem(tabName="profit",
                                box(title = "", 
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = T, 
                                    collapsible = F,
                                    h1("Profit Report", align = "center")
                                ),
                                fluidRow(
                                  column(4, 
                                         box(title = "Segment", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (radioButtons("segmen_pilihan3", 
                                                                     #key variabel *segmen_pilihan*
                                                                     "",
                                                                     #Label "Segment" tampil di layar
                                                                     choices=c("Consumer"      = "Consumer",
                                                                               "Corporate"     = "Corporate",
                                                                               "Home Office"   = "Home Office"),
                                                                     inline=T)))
                                  ),
                                  column(4, 
                                         box(title = "Category", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Tahun
                                               selectInput("category_pilihan3", 
                                                           #key variabel *category_pilihan*,
                                                           "",
                                                           choices=c("Furniture", 
                                                                     "Office Supplies",
                                                                     "Technology"), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "Technology"
                                                           #Pilihan default
                                               )))
                                  ),
                                  column(4, 
                                         box(title = "Tahun", 
                                             #Label/Title(Tampil di layar)
                                             width = 1150,
                                             height = 150,
                                             status = "primary", 
                                             solidHeader = TRUE, 
                                             collapsible = TRUE,
                                             #Hasil plotlyoutput dengan key "PlotSales1"
                                             fluidPage (#Select Input untuk Category
                                               selectInput("tahun_pilihan3", 
                                                           #key variabel *category_pilihan*
                                                           "",
                                                           choices=c(2014,
                                                                     2015,
                                                                     2016,
                                                                     2017), 
                                                           #Pilihan Category
                                                           multiple = FALSE, 
                                                           #Hanya bisa memilih 1 opsi
                                                           selected = "2017"
                                                           #Pilihan default
                                               )))
                                  )),
                                box(title = "Profit",
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotProfit3"
                                    fluidPage (plotlyOutput("PlotProfit3", height = 250))),
                                #Membuat box untuk hasil output
                                box(title = "Sub-Category",
                                    #Label/Title(Tampil di layar)
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    #Hasil plotlyoutput dengan key "PlotSubCat3"
                                    fluidPage (plotlyOutput("PlotSubCat3", height = 250)))
                                ),
                        tabItem(tabName = "author",
                                h1(),
                                mainPanel(uiOutput("plot1")))
                      )#Tab Items
                      
                    )
                    
                    
                    
                    )#Dashboard Page