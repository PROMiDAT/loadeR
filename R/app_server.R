#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'), emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  
  ##################################  Variables  ##############################
  updateData <- rv(
    datos = NULL, originales = NULL, datos.tabla = NULL, 
    idioma = NULL, datos.prueba = NULL, datos.aprendizaje = NULL,
    variable.predecir = NULL, indices = NULL, numGrupos = NULL, 
    numValC = NULL, grupos = NULL)
  
  ###################################  Update  ################################
  
  # Update on Language
  observeEvent(input$idioma, {
    updateData$idioma = input$idioma
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
  
  mod_carga_datos_server("carga_datos_ui_1", updateData)
}
