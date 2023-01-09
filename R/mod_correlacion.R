#' correlacion UI Function
#'
#' @param id Internal parameters for {shiny}.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny ui module.
#' @export mod_correlacion_ui
#' @import shiny
#' @import shinydashboardPlus
#' 
mod_correlacion_ui <- function(id){
  ns <- NS(id)
  
  opts_cor <- tabsOptions(tabs.content = list(
    list(
      options.run(ns("run_cor")), tags$hr(style = "margin-top: 0px;"),
      div(
        col_4(
          colourpicker::colourInput(
            ns("col_max"), labelInput("selcolor"), "#2E86C1",
            allowTransparent = TRUE)
        ),
        col_4(
          colourpicker::colourInput(
            ns("col_med"), labelInput("selcolor"), "#F8F5F5",
            allowTransparent = TRUE)
        ),
        col_4(
          colourpicker::colourInput(
            ns("col_min"), labelInput("selcolor"), "#FF5733",
            allowTransparent = TRUE)
        )
      )
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = ns("tabCor"), opciones = opts_cor, title = NULL,
      tabPanel(
        title = labelInput("corr"), value = "correlacion",
        echarts4rOutput(ns('plot_cor'), height = "70vh")),
      tabPanel(
        title = labelInput("nres"), value = "cor.salida",
        div(style = "height: 75vh;overflow-y: scroll;",
            withLoader(verbatimTextOutput(ns("txt_cor")), 
                       type = "html", loader = "loader4")))
    )
  )
}
    
#' correlacion Server Function
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#' @param codedioma shiny reactive values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny server module.
#' @import shiny
#' @importFrom stats cor
#' @export mod_correlacion_server
#' 
mod_correlacion_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Gráfico de Correlaciones
    output$plot_cor <- renderEcharts4r({
      input$run_cor
      datos <- var.numericas(updateData$datos)
      colores <- list(isolate(input$col_min), isolate(input$col_med), isolate(input$col_max))
      
      tryCatch({
        cod <- code.cor(colores)
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        datos.plot <- round(cor(datos), 3)
        e_cor(datos.plot, colores)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Resultados numéricos de Correlaciones
    output$txt_cor <- renderPrint(print(cor(var.numericas(updateData$datos))))
  })
}
    
## To be copied in the UI
# mod_correlacion_ui("correlacion_ui_1")
    
## To be copied in the server
# mod_correlacion_server("correlacion_ui_1")
 
