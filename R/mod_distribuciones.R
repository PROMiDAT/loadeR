#' distribuciones UI Function
#'
#' @param id Internal parameters for {shiny}.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny ui
#' @export mod_distribuciones_ui
#' @import shiny
#' @import shinydashboardPlus
#' 
mod_distribuciones_ui <- function(id){
  ns <- NS(id)
  
  titulo_dist <- tags$div(
    class = "multiple-select-var", conditionalPanel(
      condition = "input.tabDyA == 'numericas'",
      selectInput(inputId = ns("sel_dya_num"), label = NULL, choices = "")),
    conditionalPanel(
      condition = "input.tabDyA == 'categoricas'",
      selectInput(inputId = ns("sel_dya_cat"), label = NULL, choices = ""))
  )
  
  opc_dist <- tabsOptions(
    botones = list(icon("cog"), icon("info")),
    widths = c(100, 100), heights = c(50, 50),
    tabs.content = list(
      list(
        options.run(ns("run_dist")), tags$hr(style = "margin-top: 0px;"),
        conditionalPanel(
          condition = "input.tabDyA == 'numericas'",
          div(
            col_6(
              colourpicker::colourInput(
                ns("col_dist_bar"), labelInput("selcolbar"), value = "steelblue", 
                allowTransparent = T)
            ),
            col_6(
              colourpicker::colourInput(
                ns("col_dist_point"), labelInput("selcolpoint"), value = "red",
                allowTransparent = T)
            )
          )
        )
      ),
      list(DT::dataTableOutput(ns("mostrar.atipicos")))
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "tabDyA", title = titulo_dist, opciones = opc_dist,
      tabPanel(
        title = labelInput("nums"), value = "numericas",
        echarts4rOutput(ns('plot_num'), height = "75vh")),
      tabPanel(
        title = labelInput("cats"), value = "categoricas",
        echarts4rOutput(ns('plot_cat'), height = "75vh"))
    )
  )
}
    
#' distribuciones Server Function
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny server
#' @import shiny
#' @export mod_distribuciones_server
#' 
mod_distribuciones_server <- function(id, updateData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update on load data
    observeEvent(updateData$datos, {
      datos       <- updateData$datos
      numericos   <- var.numericas(datos)
      categoricos <- var.categoricas(datos)
      
      updateSelectInput(session, "sel_dya_num", choices = colnames(numericos))
      updateSelectInput(session, "sel_dya_cat", choices = colnames(categoricos))
    })
    
    # Gráfico de Distribuciones (Númericas)
    output$plot_num = renderEcharts4r({
      input$run_dist
      datos      <- updateData$datos
      var        <- input$sel_dya_num
      colorBar   <- isolate(input$col_dist_bar)
      colorPoint <- isolate(input$col_dist_point)
      titulos <- c(
        tr("minimo", updateData$idioma),
        tr("q1", updateData$idioma),
        tr("mediana", updateData$idioma),
        tr("q3", updateData$idioma),
        tr("maximo", updateData$idioma)
      )
      
      tryCatch({
        cod <- paste0("e_histboxplot(datos[['", var, "']], '", var, "', '", 
                      colorBar, "', '", colorPoint, "', c('", 
                      paste(titulos, collapse = "', '"), "'))\n")
        isolate(updateData$code[['basico']][['docdistnum']] <- cod)
        e_histboxplot(datos[[var]], var, colorBar, colorPoint, titulos)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    
    output$mostrar.atipicos = DT::renderDataTable({
      datos <- updateData$datos
      var   <- input$sel_dya_num
      atipicos <- boxplot.stats(datos[, var])
      datos <- datos[datos[, var] %in% atipicos$out, var, drop = F]
      datos <- datos[order(datos[, var]), , drop = F]
      DT::datatable(datos, options = list(
        dom = 't', scrollX = TRUE, scrollY = "28vh", pageLength = nrow(datos)))|>
        formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
    })
    
    # Gráfico de Distribuciones (Categóricas)
    output$plot_cat = renderEcharts4r({
      var  <- input$sel_dya_cat
      validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
      
      tryCatch({
        datos.plot <- updateData$datos[, var]
        
        cod <- code.dist.cat(var)
        isolate(updateData$code[['basico']][['docdistcat']] <- cod)
        
        datos.plot <- data.frame (
          label = levels(datos.plot),
          value = summary(datos.plot, maxsum = length(levels(datos.plot)))
        )
        
        datos.plot|> e_charts(label)|> e_bar(value, name = var)|>
          e_tooltip()|> e_datazoom(show = F)|> e_show_loading()
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })
}
    
## To be copied in the UI
# mod_distribuciones_ui("distribuciones_ui_1")
    
## To be copied in the server
# mod_distribuciones_server("distribuciones_ui_1")
 
