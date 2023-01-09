#' normal UI Function
#'
#' @param id Internal parameters for {shiny}.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny ui module.
#' @export mod_normal_ui
#' @import shiny
#' @import shinydashboardPlus
#' 
mod_normal_ui <- function(id) {
  ns <- NS(id)
  
  opc_hist <- tabsOptions(tabs.content = list(
    list(options.run(ns("run_normal")), tags$hr(style = "margin-top: 0px;"),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalPlot'",
           div(
             col_6(
               colourpicker::colourInput(
                 ns("col_hist_bar"), labelInput("selcolbar"),
                 value = "steelblue", allowTransparent = TRUE)
             ),
             col_6(
               colourpicker::colourInput(
                 ns("col_hist_line"), labelInput("selcolline"),
                 value = "#555555", allowTransparent = TRUE)
             )
           )
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabQPlot'",
           div(
             col_6(
               colourpicker::colourInput(
                 ns("col_qq_point"), labelInput("selcolpoint"),
                 value = "steelblue", allowTransparent = TRUE)
             ),
             col_6(
               colourpicker::colourInput(
                 ns("col_qq_line"), labelInput("selcolline"),
                 value = "#555555", allowTransparent = TRUE)
             )
           )
         ),
         conditionalPanel(
           "input.BoxNormal == 'tabNormalCalc'",
           sliderInput(ns("slide_inter"), labelInput("alfa"), 
                       min = 0, max = 0.2, step = 0.01, value = 0.05)
         )
    )
  ))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxNormal", opciones = opc_hist, 
      title = tags$div(
        class = "multiple-select-var",
        selectInput(inputId = ns("sel_normal"), label = NULL, choices =  "")
      ), 
      tabPanel(
        title = labelInput("pnorm"), value = "tabNormalPlot",
        echarts4rOutput(ns('plot_normal'), height = "70vh")),
      tabPanel(
        title = "Qplot + Qline", value = "tabQPlot",
        echarts4rOutput(ns('plot_qq'), height = "70vh")),
      tabPanel(
        title = labelInput("norm"), value = "tabNormalCalc",
        withLoader(DT::DTOutput(ns('calc_normal')), 
                   type = "html", loader = "loader4"))
    )
  )
}
    
#' normal Server Function
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#' @param codedioma shiny reactive values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny server module.
#' @import shiny
#' @export mod_normal_server
#' 
mod_normal_server <- function(id, updateData, codedioma) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update on load data
    observeEvent(updateData$datos, {
      datos     <- updateData$datos
      numericos <- var.numericas(datos)
      
      updateSelectInput(session, "sel_normal", choices = colnames(numericos))
    })
    
    # Grafico Test de normalidad
    output$plot_normal <- renderEcharts4r({
      input$run_normal
      var       <- input$sel_normal
      datos     <- updateData$datos[, var]
      colorBar  <- isolate(input$col_hist_bar)
      colorLine <- isolate(input$col_hist_line)
      nombres   <- c(tr("hist", codedioma$idioma), 
                     tr("curva", codedioma$idioma))
      
      tryCatch({
        cod <- paste0(
          "### dochist\n",
          "e_histnormal(datos[['", var, "']], '", colorBar,
          "', '", colorLine, "', c('", nombres[1], "', '", 
          nombres[2], "'))\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_histnormal(datos, colorBar, colorLine, nombres)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Grafico qqplot + qqline
    output$plot_qq <- renderEcharts4r({
      input$run_normal
      var        <- input$sel_normal
      datos      <- updateData$datos[, var]
      colorPoint <- isolate(input$col_qq_point)
      colorLine  <- isolate(input$col_qq_line)
      
      tryCatch({
        cod <- paste0(
          "### docqq\n",
          "e_qq(datos[['", var, "']], '", colorPoint,
          "', '", colorLine, "')\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        e_qq(datos, colorPoint, colorLine)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    # Resumen Test de normalidad
    output$calc_normal <- DT::renderDT({
      input$run_normal
      datos <- updateData$datos
      alfa <- isolate(as.numeric(input$slide_inter))
      noms  <- c(tr('asim', isolate(codedioma$idioma)),
                 tr('norm', isolate(codedioma$idioma)),
                 tr('sigue', isolate(codedioma$idioma)),
                 tr('pvalue', isolate(codedioma$idioma)),
                 tr('tasim', isolate(codedioma$idioma)))
      
      tryCatch({
        cod <- paste0("### docnormal\n", "dfnormal(datos)\n")
        isolate(codedioma$code <- append(codedioma$code, cod))
        res <- dfnormal(datos)
        
        res <- res[, c(1, 5)]
        res <- round(res, 3)
        res$asimetria <- res$fisher > 0
        res$asimetria <- ifelse(res$asimetria, '<i class="fa fa-plus" style="color: green;"></i>', 
                                '<i class="fa fa-minus" style="color: red;"></i>')
        res$normal <- res$shapiro > alfa
        res$normal <- ifelse(res$normal, '<i class="fa fa-check" style="color: green;"></i>', 
                             '<i class="fa fa-times" style="color: red;"></i>')
        res$shapiro <- paste0(res$shapiro, " > ", alfa)
        res <- res[, c(1, 3, 2, 4)]
        
        sketch <- htmltools::withTags(table(
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Variables'), 
              tags$th(colspan = 2, style = "text-align: center;", 
                      labelInput('asim', noms[1])),
              tags$th(colspan = 2, style = "text-align: center;", 
                      labelInput('norm', noms[2]))
            ),
            tags$tr(
              tags$th(labelInput('tasim', noms[5])), tags$th(labelInput('asim', noms[1])),
              tags$th(labelInput('pvalue', noms[4])), tags$th(labelInput('sigue', noms[3]))
            )
          )
        ))
        
        DT::datatable(
          res, selection = 'none', container = sketch, escape = FALSE,
          options = list(dom = 'frtip', scrollY = "50vh")
        )
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })
}

## To be copied in the UI
# mod_normal_ui("normal_ui_1")

## To be copied in the server
# mod_normal_server("normal_ui_1")