#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  ##################################  Options  ################################
  
  old <- options()
  on.exit(options(old))
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
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
    datos = NULL, originales = NULL, datos.tabla = NULL, datos.prueba = NULL, 
    datos.aprendizaje = NULL, variable.predecir = NULL, indices = NULL, 
    numGrupos = NULL, numValC = NULL, grupos = NULL)
  
  codedioma <- rv(idioma = "es", code = list())
  
  modelos <- rv(
    svm = NULL, knn = NULL, bayes = NULL, rl = NULL, rlr = NULL, xgb = NULL,
    boosting = NULL, rf = NULL, nn = NULL, dt = NULL)
  
  ###################################  Update  ################################
  
  # Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    updateLabelInput(session, labels_loadeR(), tr(labels_loadeR(), input$idioma))
  })
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg <- input$idioma
    
    keys <- c(
      'doccarga', 'doctt', 'doccv', 'docresumen', 'dochist', 'docqq', 
      'docnormal', 'docdisp', 'docdistnum', 'docdistcat', 'doccor',
      'docrename', 'doctrans', 'doceliminar')
    
    for (k in keys) {
      codigo <- gsub(k, tr(k, idioma = lg), codigo, fixed = TRUE)
    }
    
    codigo.completo <- paste0(
      "library(readxl)\n", "library(caret)\n",
      "library(echarts4r)\n", "library(loadeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  ##################################  Modules  ################################
  
  mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, codedioma, paquete)
  
  mod_r_numerico_server("r_numerico_ui_1", updateData, codedioma)
  
  mod_normal_server("normal_ui_1", updateData, codedioma)
  
  mod_dispersion_server("dispersion_ui_1", updateData, codedioma)
  
  mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  
  mod_correlacion_server("correlacion_ui_1", updateData, codedioma)
}
