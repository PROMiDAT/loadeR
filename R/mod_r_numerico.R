#' r_numerico UI Function
#'
#' @param id Internal parameters for {shiny}.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny ui module.
#' @export mod_r_numerico_ui
#' @import shiny
#' @import htmltools
#' @import shinydashboardPlus
#' 
mod_r_numerico_ui <- function(id) {
  ns <- NS(id)
  
  # declare dependencies
  shiny::addResourcePath(
    "resumenNumerico-lib", system.file("assets", "resumenNumerico", package = "loadeR"))
  
  deps <- list(
    htmltools::htmlDependency(
      "resumenNumerico-lib", "0.1.0", c(href = "resumenNumerico-lib"),
      stylesheet = "resumenNumerico.css"
    )
  )
  
  inputTag <- tagList(
    div(uiOutput(ns("resumennumerico")))
  )
  
  return(htmltools::attachDependencies(inputTag, deps))
}

#' r_numerico Server Function
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#' @param codedioma shiny reactive values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny server module.
#' @import shiny
#' @importFrom stats median
#' @export mod_r_numerico_server
#' 
mod_r_numerico_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$resumennumerico = renderUI({
      datos  <- updateData$datos
      idioma <- codedioma$idioma
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
          title  = col.name, status = "primary", width  = 12, solidHeader = TRUE,
          collapsible = TRUE, fluidRow(
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
      
      cod <- paste0("### docresumen\n", "summary(datos)\n")
      isolate(codedioma$code <- append(codedioma$code, cod))
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

