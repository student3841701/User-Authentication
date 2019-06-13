rm(list=ls(all=T))


#----------packages------------#
library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)
library(shinyWidgets)
library(shinyjs)
library(digest)
library(shinyalert)


loginUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("login_ui"))
}

login <- function(input, output, session, user_base, log_out) {
  credentials <- reactiveValues(user_auth = FALSE,
                                permission = NULL)
  observe({
    if(log_out() > 0) {
      credentials$user_auth <- FALSE
      credentials$permission <- NULL
    }
  })
  
  output$login_ui <- renderUI({
    if(credentials$user_auth == TRUE) return(NULL)
    fluidPage(
      div(style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
          wellPanel(
            h2("Welcome", class = "text-center", style = "padding-top: 0;"),
            textInput(session$ns("user_name"), "user"),
            passwordInput(session$ns("password"), "password"),
            div(
              style="text-align: center;",
              actionButton(session$ns("login_button"), "Log in", class = "btn-primary", style = "color:blue;")
            ),
            useShinyalert(),
            hidden(actionButton(session$ns("login_error"),''))
          ),
          renderTable({ user_base })))})
  
  observeEvent(input$login_button, {
    
    user_base <- mutate(user_base,  password = map_chr(password, digest))
    
    row_username <- which(user_base$user == input$user_name)
    row_password <- which(user_base$password == digest(input$password))
    
    if (length(row_username) == 1 && length(row_password) >= 1 && (row_username %in% row_password)) {
      credentials$user_auth <- TRUE
      credentials$permission <- user_base$permissions[row_username]
    } else {
        output$login_error <- 
          shinyalert("Oops!", "Invalid username or password!", type = "error")
      }
  })
  return(reactive(reactiveValuesToList(credentials)))
}

user_base <- tibble::tibble(
  user = c("admin", "user"),
  password = c("passa", "passu"), 
  permissions = c("admin", "standard")
)


#----------ui----------#
logo_blue_gradient <- shinyDashboardLogoDIY(
  boldText = "User Auth",mainText = "",textSize = 16,
  badgeText = "demo",badgeTextColor = "white",badgeTextSize = 2,
  badgeBackColor = "#779dbf",badgeBorderRadius = 3)

theme_purple_gradient <- shinyDashboardThemeDIY(
  appFontFamily = "Microsoft JhengHei"
  ,appFontColor = "rgb(53, 76, 96)"   
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(255,255,255)"
  ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(70,130,180)"
  ,dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(49,56,107)"
  ,headerButtonBackColor = "rgb(49,56,107)"
  ,headerButtonIconColor = "rgb(62,133,179)"
  ,headerButtonBackColorHover = "rgb(49,56,107)"
  ,headerButtonIconColorHover = "rgb(255,255,255)"
  
  ,headerBackColor = "rgb(49,56,107)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(49,56,107)"
    ,colorMiddle = "rgb(71,59,109)"
    ,colorEnd = "rgb(78,88,149)"
    ,colorStartPos = 0
    ,colorMiddlePos = 70
    ,colorEndPos = 100
  )
  
  ,sidebarShadowRadius = ""
  ,sidebarPadding = 10
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarMenuBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(48,103,157)"
    ,colorMiddle = "rgb(65,79,129)"
    ,colorEnd = "rgb(55,70,120)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 20
  
  ,sidebarUserTextColor = "rgb(128,177,221)"
  
  ,sidebarSearchBackColor = "rgb(40,70,115)"
  ,sidebarSearchIconColor = "rgb(50,115,145)"
  ,sidebarSearchBorderColor = "rgb(30,60,105)"
  
  ,sidebarTabTextColor = "rgb(128,177,221)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "30px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(56,137,189)"
    ,colorMiddle = "rgb(65,95,145)"
    ,colorEnd = "rgb(68,84,137)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "30px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 16
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(49,56,107)"
  ,boxPrimaryColor = "rgb(60,90,153)"
  ,boxInfoColor = "rgb(20,100,160)"
  ,boxSuccessColor = "rgb(64,186,170)"
  ,boxWarningColor = "rgb(255,217,144)"
  ,boxDangerColor = "rgb(249,144,144)"
  
  ,tabBoxTabColor = "rgb(80,95,155)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(128,177,221)"
  ,tabBoxTabTextColorSelected = "rgb(255,255,255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(62,117,167)"
  ,tabBoxBorderRadius = 15
  
  ### inputs
  ,buttonBackColor = "rgb(255,255,255)"
  ,buttonTextColor = "rgb(40,63,106)"
  ,buttonBorderColor = "rgb(72, 155, 229)"
  ,buttonBorderRadius = 20
  
  ,buttonBackColorHover = "rgb(255,255,255)"   
  ,buttonTextColorHover = "rgb(115,210,240)" ####
  ,buttonBorderColorHover = "rgb(115,210,240)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(30,60,105)"
  ,textboxBorderRadius = 20
  ,textboxBackColorSelect = "rgb(40,70,115)"
  ,textboxBorderColorSelect = "rgb(30,60,105)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(255,255,255)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

header <- dashboardHeader(
  title = logo_blue_gradient,
  dropdownMenuOutput("messageMenu"),
  tags$li(class = "dropdown",
          actionBttn(
            inputId = "Logout",
            label = "Logout",
            style = "gradient", 
            color = "primary",
            icon = icon("sign-out-alt"),
            size='sm')
  ))


ui <- dashboardPage(
  dashboardHeader(title = logo_blue_gradient, uiOutput("logout_button")),
  dashboardSidebar(theme_purple_gradient,
                   collapsed = TRUE, sidebarMenuOutput("sidebar")),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}")),
    loginUI("login"),
      uiOutput("plots")))


#---------server---------#
server <- function(input, output, session) {
  
  rv <- reactiveValues(logout_init = 0)
  
  user_info <- callModule(login, "login", 
                          user_base = user_base, 
                          log_out = reactive(rv$logout_init))
  output$logout_button <- renderUI({
    if(user_info()$user_auth) {
      div(
        tags$li(class = "dropdown",
                  actionBttn(
                    inputId = "logout",
                    label = "Logout",
                    style = "gradient", 
                    color = "primary",
                    icon = icon("sign-out-alt"),
                    size='sm')))
    } else {
      return(NULL)}
    })
  
  observeEvent(input$logout, {
    rv$logout_init <- rv$logout_init + 1
  })
  
  observe({
    if(user_info()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else if (!isTruthy(user_info()$user_auth)) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  output$sidebar <- renderMenu({
    req(user_info()$user_auth)
    if(user_info()$permission == "admin") {
      dashboardSidebar(
        sidebarUserPanel("Jennings Chen", 
                       subtitle = a(href = "#",icon("circle", class = "text-success"),"admin"),
                       image = "https://i.ibb.co/6YSrNMv/123.png"),
      sidebarMenu(id = "sidebar_main",
                  menuItem("upload file", icon=icon("upload"),tabName = "myfile",selected=T),
                  br(),textOutput("currentTime")))} 
    else {
      dashboardSidebar(
      sidebarUserPanel("Will Smith", 
                       subtitle = a(href = "#",icon("circle", class = "text-success"),"user")),
      sidebarMenu(id = "sidebar_main",
                  menuItem("upload file", icon=icon("upload"),tabName = "myfile",selected=T),
                  br(),textOutput("currentTime")))} 
                  })
  
  
  output$plots <- renderUI({
    req(user_info()$user_auth)
    if(user_info()$permission == "admin") {
      tabItems(
        tabItem(tabName = "myfile",
                box(width = NULL, status = "primary", solidHeader = TRUE,title="upload file",                
                    fileInput("file1", "Choose your xlsx file",
                              accept = c(".xlsx"))),
                box(width = NULL, status = "primary", solidHeader = TRUE,title="show data",   
                    tableOutput("contents"))))
      }
    else{
      tabItems(
        tabItem(tabName = "myfile",
                box(width = NULL, status = "primary", solidHeader = TRUE,title="upload file",                
                    fileInput("file1", "Choose your xlsx file",
                              accept = c(".xlsx"))),
                box(width = NULL, status = "primary", solidHeader = TRUE,title="show data",   
                    tableOutput("contents"))))
      }
  })
  
  if(Sys.getenv('SHINY_PORT') == "") 
    options(shiny.maxRequestSize=10000*1024^2)
  
  output$contents <- renderTable ({
    if(is.null(input$file1)) return(NULL)
    df <- readxl::read_excel(input$file1$datapath)
    return(head(df))
  })
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
}

#---------run--------------#
shinyApp(ui=ui,server=server)
