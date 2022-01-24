#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  ##################################  Options  ################################
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
    datos = NULL, originales = NULL, datos.tabla = NULL, idioma = NULL, 
    datos.prueba = NULL, datos.aprendizaje = NULL, variable.predecir = NULL, 
    indices = NULL, numGrupos = NULL, numValC = NULL, grupos = NULL, 
    code = NULL, codenew = NULL)
  
  modelos <-  rv(
    svm = NULL, knn = NULL, bayes = NULL, rl = NULL, rlr = NULL, xgb = NULL,
    boosting = NULL, rf = NULL, nn = NULL, dt = NULL)
  
  ###################################  Update  ################################
  
  # Update on Language
  observeEvent(input$idioma, {
    updateData$idioma = input$idioma
    updateLabelInput(session, labels_readeR(), tr(labels_readeR(), input$idioma))
  })
  
  # Update Code
  observeEvent(c(updateData$code, updateData$codenew, input$idioma), {
    todo  <- updateData$code
    nuevo <- updateData$codenew
    lg    <- input$idioma
    
    comp <- todo[["comp"]]
    todo[["comp"]] <- NULL
    
    cod <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(echarts4r)\n", "library(readeR)\n\n"
    )
    for (modulo in todo) {
      for (n in names(modulo)) {
        cod <- paste0(cod, "### ", tr(n, lg), "\n", modulo[[n]], "\n\n")
      }
    }
    for (n in names(comp)) {
      cod <- paste0(cod, "### ", tr(n, lg), "\n", comp[[n]], "\n\n")
    }
    if(!is.null(nuevo)) {
      cod <- paste0(cod, "############  ", tr("news", lg), "  ###########\n\n")
    }
    for (n in names(nuevo)) {
      cod <- paste0(cod, "### ", tr(n, lg), "\n", nuevo[[n]], "\n\n")
    }
    updateAceEditor(session, "fieldCode", value = cod)
  })
  
  mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, paquete)
  
  mod_r_numerico_server("r_numerico_ui_1", updateData)
  
  mod_normal_server("normal_ui_1", updateData)
  
  mod_dispersion_server("dispersion_ui_1", updateData)
  
  mod_distribuciones_server("distribuciones_ui_1", updateData)
  
  mod_correlacion_server("correlacion_ui_1", updateData)
}
