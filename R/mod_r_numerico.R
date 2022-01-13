#' r_numerico UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_r_numerico_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(uiOutput(ns("resumennumerico")))
  )
}

#' r_numerico Server Function
#' @keywords internal
mod_r_numerico_server <- function(id, updateData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$resumennumerico = renderUI({
      datos  <- updateData$datos
      idioma <- updateData$idioma
      numeric.names <- colnames(var.numericas(datos))
      res    <- vector(mode = "list", length = ncol(datos))
      res <- list(res, lapply(colnames(datos), function(col.name) {
        data.summary <- NULL
        data.type    <- NULL
        if(col.name %in% numeric.names){
          data.summary <- resumen.numerico(datos, col.name, idioma)
          data.type    <- "num"
          icon.name    <- "fa-sort-numeric-up"
        }
        else{
          data.summary <- resumen.categorico(datos, col.name)
          data.type    <- "cat"
          icon.name    <- "fa-font"
        }
        list(div(col_3(box(
          title  = col.name, status = "primary", width  = 12, solidHeader = T,
          collapsible = T, fluidRow(
            class = "summ-row", 
            col_12(
              tags$span(class = "wrapper-tag", style = "font-size: 11px;",
                        tags$i(class = paste0("fa ", icon.name)), 
                        tr(data.type, idioma)), hr(class = "summ-hr")), 
            lapply(data.summary, function(val) {
              list(col_6(tags$span(val$Label)), 
                   col_6(tags$span(val$Value), style = "text-align: end;"))
            })
          ))
        )))
      }))
      
      res <- tags$div(style = "height: 80vh; overflow-y: scroll;",
                      do.call(tagList, res))
      
      return(res)
    })
  })
}

## To be copied in the UI
# mod_r_numerico_ui("r_numerico_ui_1")

## To be copied in the server
# mod_r_numerico_server("r_numerico_ui_1")

