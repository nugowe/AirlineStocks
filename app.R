library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(shinybusy)
library(quantmod)
library(bsplus)
library(ICON)

ui <- dashboardPage(
    dashboardHeader(
        tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/nosa-ugowe-02010318/", icon("linkedin"))),
        
        title = 'PUBLICLY TRADED AIRLINE STOCKS', titleWidth = 400
    
    ),
    dashboardSidebar(width = 400,
        sidebarMenu(id = "sidebarmenu",
                    
                    menuItem("NYSE QUOTED AIRLINE STOCKS", tabName = "a",  icon = icon ("plane", class = "fa-spin")),
                    menuItem("NASDAQ QUOTED AIRLINE STOCKS", tabName = "b", icon = icon ("plane", class = "fa-spin")),
                   
                    conditionalPanel("input.sidebarmenu === 'a'",
                                     prettyCheckboxGroup(
                                         inputId = "checkgroup1",
                                         label = "", 
                                         choices = c("Alaska Air Group, Inc","Azul S.A.","China Eastern Airlines Corporation Ltd.","China Southern Airlines Company Limited","Controladora Vuela Compania de Aviacion, S.A.B. de C.V.","Copa Holdings, S.A.","Delta Air Lines, Inc.","Gol Linhas Aereas Inteligentes S.A.","Southwest Airlines Company","Spirit Airlines, Inc."),
                                         icon = icon("check-square-o"), 
                                         selected = "Alaska Air Group, Inc",
                                         status = "danger",
                                         outline = TRUE,
                                         bigger = FALSE,
                                         fill = TRUE,
                                         animation = "rotate"
                                     ),
                                     bs_carousel(id = "the_beatles", use_indicators = TRUE) %>%
                                         bs_append(
                                             content = bs_carousel_image(src = "Nasdaq-Logo.jpeg"),
                                             
                                         ) %>%
                                         bs_append(
                                             content = bs_carousel_image(src = "ny.jpeg")
                    )),
                    
                            
                    conditionalPanel("input.sidebarmenu === 'b'",
                                     prettyCheckboxGroup(
                                         inputId = "checkgroup2",
                                         label = "", 
                                         choices = c("Allegiant Travel Company","American Airlines Group, Inc.","Hawaiian Holdings, Inc.","JetBlue Airways Corporation","Mesa Air Group, Inc.","Ryanair Holdings plc","SkyWest, Inc.","Spirit Airlines, Inc.","United Airlines Holdings, Inc."),
                                         
                                         icon = icon("check-square-o"), 
                                         status = "danger",
                                         fill = TRUE,
                                         outline = TRUE,
                                         animation = "rotate"
                                         
                                     ),
                                     bs_carousel(id = "the_beatles", use_indicators = TRUE) %>%
                                         bs_append(
                                             content = bs_carousel_image(src = "Nasdaq-Logo.jpeg"),
                                             
                                         ) %>%
                                         bs_append(
                                             content = bs_carousel_image(src = "ny.jpeg"),
                    )
        )
    
        )),
    
    dashboardBody(
        tags$head(tags$style(HTML('
                               
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #420420;font-family:"Open Sans";font-weight: bold;font-size:22px;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #000000;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #000000;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #000000;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #000000;
                              }
                              
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #000000;
                              color: #FFFFFF;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #000000;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #000000;
                              }
                              '
        ))),
        
        use_googlefont("Open Sans"),
        use_theme(create_theme(
            theme = "darkly",
            bs_vars_font(
                family_sans_serif = "'Open Sans'", size_base = "13px"
            )
        )),
        tabItems(
            # First tab content
            tabItem(tabName = "a",
                    column(width = 12,
                           
                           
                           
                           add_busy_spinner(spin = "intersecting-circles"),
                           addSpinner(plotOutput('Data1'), spin = "bounce", color = "black"), height = 900),
                           
                    tabItem(tabName = "b",
                            column(width = 12,
                                   
                                   
                                   
                                   add_busy_spinner(spin = "intersecting-circles"),
                                   addSpinner(plotOutput('Data2'), spin = "bounce", color = "black"), height = 900),
                    
            )))))                  
                

server <- function(input, output) {
    
    dataInput <- reactive({
        req(input$checkgroup1)
        if(input$checkgroup1 == "Alaska Air Group, Inc"){
            
        getSymbols("ALK", auto.assign = F)
        
            
        }else if(input$checkgroup1 == "Azul S.A."){
            Azul <- getSymbols("AZUL", auto.assign = F)
            return(Azul)
        }else if(input$checkgroup1 == "China Eastern Airlines Corporation Ltd."){
            getSymbols("ZNH", auto.assign = F)
        }else if(input$checkgroup1 == "China Southern Airlines Company Limited"){
            getSymbols("CEA", auto.assign = F)
        }else if(input$checkgroup1 == "Controladora Vuela Compania de Aviacion, S.A.B. de C.V."){
            getSymbols("VLRS", auto.assign = F)
        }else if(input$checkgroup1 == "Copa Holdings, S.A."){
            getSymbols("CPA", auto.assign = F)
        }else if(input$checkgroup1 == "Delta Air Lines, Inc."){
            getSymbols("DAL", auto.assign = F)
        }else if(input$checkgroup1 == "Gol Linhas Aereas Inteligentes S.A."){
            getSymbols("GOL", auto.assign = F)
        }else if(input$checkgroup1 == "Southwest Airlines Company"){
            getSymbols("LUV", auto.assign = F)
        }else if(input$checkgroup1 == "Spirit Airlines, Inc."){
            getSymbols("SAVE", auto.assign = F)
        }
    })
    
    
    
    
    output$Data1 <- renderPlot({ 
        NYSE_AIRLINE_STOCKS <- dataInput()
        chartSeries(NYSE_AIRLINE_STOCKS, main = NYSE_AIRLINE_STOCKS)
    })
    
    
    dataInput1 <- reactive({
        req(input$checkgroup2)
        
        if(input$checkgroup2 == "Allegiant Travel Company"){
            getSymbols("ALGT", auto.assign = F)
        }else if(input$checkgroup2 == "American Airlines Group, Inc."){
            getSymbols("AAL", auto.assign = F)
        }else if(input$checkgroup2 == "Hawaiian Holdings, Inc."){
            getSymbols("HA", auto.assign = F)
        }else if(input$checkgroup2 == "JetBlue Airways Corporation"){
            getSymbols("JBLU", auto.assign = F)
        }else if(input$checkgroup2 == "Mesa Air Group, Inc."){
            getSymbols("MESA", auto.assign = F)
        }else if(input$checkgroup2 == "Ryanair Holdings plc"){
            getSymbols("RYAAY", auto.assign = F)
        }else if(input$checkgroup2 == "SkyWest, Inc."){
            getSymbols("GOL", auto.assign = F)
        }else if(input$checkgroup2 == "Spirit Airlines, Inc."){
            getSymbols("SAVE", auto.assign = F)
        }else if(input$checkgroup2 == "United Airlines Holdings, Inc."){
            getSymbols("UAL", auto.assign = F)
        }
    })
    
    output$Data2 <- renderPlot({ 
        NASDAQ_AIRLINE_STOCKS <- dataInput1()
        chartSeries(NASDAQ_AIRLINE_STOCKS, main = NASDAQ_AIRLINE_STOCKS)
    })
    
}

shinyApp(ui, server)
