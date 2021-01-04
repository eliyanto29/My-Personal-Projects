
source("global.R", local = TRUE)

ui <- dashboardPage(title = "Data Retail Dashboard",
                    
                    # Header
                    dashboardHeader(title = "Retail Dashboard",titleWidth = 200),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Static Dashboard",
                                 icon = icon("dashboard"),
                                 tabName = "beranda"),
                        menuItem("Data Table",
                                 icon = icon("database"),
                                 tabName ="data_table"),
                        menuItem("Author",  
                                 tabName = "author", 
                                 icon = icon("user")),
                        menuItem("Visit Author GitHub Profile", 
                                 icon = icon("send",lib='glyphicon'), 
                                 href = "https://github.com/eliyanto29")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "beranda",
                                fluidRow(valueBoxOutput("profit_VB"),
                                         valueBoxOutput("time_VB"),
                                         valueBoxOutput("quantity_VB")),
                                box(title = "Sales VS Quantity",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (plotlyOutput("Plot1", height = 250))
                                    ),
                                box(title = "Segment & Category",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (plotlyOutput("Plot2", height = 250))
                                ),
                                box(title = "Sub-Category",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (plotlyOutput("Plot3", height = 250))
                                ),
                                box(title = "Top 10 Purchasing Consumers",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (plotlyOutput("Plot4", height = 250))
                                ),
                                box(title = "Top 10 Selling Products",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (plotlyOutput("Plot5", height = 250))
                                )
                                
                                ),
                        tabItem(tabName="data_table",
                                box(title = "Data Table",
                                    width = 1150,
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    fluidPage (DT::dataTableOutput("datatable"))
                                )
                                ),
                        tabItem(tabName = "author",
                                h1(),
                                mainPanel(uiOutput("plot1")))
                    ))
)
