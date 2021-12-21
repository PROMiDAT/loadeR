#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyAce
#' @import data.table
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @importFrom stats na.omit
#' @importFrom DT tableHeader formatStyle
#' @importFrom shinydashboard dashboardBody menuItem menuSubItem sidebarMenu tabBox tabItem tabItems infoBox
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
   
    dashboardPage(
      title = "PROMiDAT",
      dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href xv= "https://promidat.com" target = "_blank">
              <img src = "img/logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        )), controlbarIcon = icon("cogs")
      ),
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style = "padding-top:10px;"),
          menuItem(labelInput("data"), icon = icon("database"),
                   tabName = "cargar"),
          hr(),
          menu.idioma(),
          tags$div(style = "display:none;",
                   sliderInput(inputId = "aux", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10)
                   #radioSwitch("deleteNAaux", "eliminanaaux", c("eliminarai", "impsutar")),
          )
        )
      ),
      dashboardBody(
        
        tabItems(
          # Carga de Datos
          tabItem(tabName = "cargar", mod_carga_datos_ui("carga_datos_ui_1"))
        )
      ),
      
      dashboardControlbar(
        width = 500,
        div(
          style = "margin-right: 15px; margin-left: 15px;", 
          h3(labelInput('code')), hr(), 
          codigo.monokai("fieldCode", height = "70vh"),
          downloadButton("btn_code", NULL, style = "width: 100%;")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'readeR'
    )
  )
}
