#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyAce
#' @import echarts4r
#' @import data.table
#' @import htmlwidgets
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @importFrom grDevices hcl
#' @importFrom stats na.omit
#' @importFrom colourpicker colourInput
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom DT tableHeader formatStyle styleEqual
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
        )), controlbarIcon = icon("gears")
      ),
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style = "padding-top:10px;"),
          menuItem(labelInput("data"), icon = icon("database"),
                   tabName = "cargar"),
          menuItem(labelInput("basi"), tabName = "parte1",
                   icon = icon("table-list"),
                   menuSubItem(labelInput("resu"), "resumen",
                               icon = icon("arrow-down-1-9")),
                   menuSubItem(labelInput("norm"), "normalidad",
                               icon = icon("chart-bar")),
                   menuSubItem(labelInput("disp"), "dispersion",
                               icon = icon("chart-line")),
                   menuSubItem(labelInput("dist"), "distribucion",
                               icon = icon("chart-area")),
                   menuSubItem(labelInput("corr"), "correlacion",
                               icon = icon("table"))
          ),
          menuItem(labelInput("acerca"), tabName = "acercaDe",
                   icon = icon("info")),
          hr(),
          menu.idioma(),
          hr(),
          img(src = "img/loadeR.png",
              style = paste0("margin-left: auto;",
                             "margin-right: auto;display: block;width: 80%;")),
          tags$div(style = "display:none;",
                   sliderInput(inputId = "aux", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10),
                   colourpicker::colourInput(
                     "auxColor", NULL, value = "red", allowTransparent = TRUE),
                   radioSwitch("deleteNAaux", "eliminanaaux", c("eliminarai", "impsutar"))
          )
        )
      ),
      dashboardBody(
        
        tabItems(
          # Carga de Datos
          tabItem(tabName = "cargar", mod_carga_datos_ui(
            "carga_datos_ui_1", labelInput('data'), paquete = paquete)),
          
          # Resumen Numérico
          tabItem(tabName = "resumen", mod_r_numerico_ui("r_numerico_ui_1")),
          
          # Test de Normalidad
          tabItem(tabName = "normalidad", mod_normal_ui("normal_ui_1")),
          
          # Dispersión
          tabItem(tabName = "dispersion",
                  mod_dispersion_ui("dispersion_ui_1")),
          
          # Distribuciones
          tabItem(tabName = "distribucion", 
                  mod_distribuciones_ui("distribuciones_ui_1")),
          
          # Correlaciones
          tabItem(tabName = "correlacion", 
                  mod_correlacion_ui("correlacion_ui_1")),
          
          # Acerca De
          tabItem(tabName = "acercaDe", mod_acercade_ui("acercade_ui_1"))
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
golem_add_external_resources <- function() {
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'loadeR'
    ),
    shinyjs::useShinyjs()
  )
}
