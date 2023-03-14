#' carga_datos UI Function
#'
#' @param id Internal parameters for {shiny}.
#' @param title Display title for tab.
#' @param paquete indicates if the data is going to be used for exploratory, predictive, or regression analysis.
#' 
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return shiny ui module.
#' @export mod_carga_datos_ui
#' @import shiny
#' @import htmltools
#' @import shinydashboardPlus
#' 
mod_carga_datos_ui <- function(id, title, paquete = "predictoR") {
  ns <- NS(id)
  
  # declare dependencies
  shiny::addResourcePath(
    "cargaDatos-lib", system.file("assets", "cargaDatos", package = "loadeR"))
  
  deps <- list(
    htmltools::htmlDependency(
      "cargaDatos-lib", "0.1.0", c(href = "cargaDatos-lib"),
      script = "cargaDatos.js",
      stylesheet = "cargaDatos.css"
    )
  )
  
  carga <- list(
    tabsetPanel(
      type = "tabs", id = ns("file_type"),
      tabPanel(
        labelInput("texf"),
        col_10(fileInput(
          ns('archivo'), labelInput("selfile"), width = "100%",
          placeholder = "", buttonLabel = labelInput("subi"),
          accept = c('text/csv', '.csv', '.txt'))),
        col_2(actionButton(ns("prevfile"), NULL, icon = icon("eye"), style = "margin-top: 25px;width: 100%;")),
        col_6(checkboxInput(ns('header'), labelInput("selhead"), value = TRUE)),
        col_6(checkboxInput(ns('rowname'), labelInput("selrow"), value = TRUE)),
        col_6(
          radioButtons(
            ns('sep'), labelInput("selsep"), inline = TRUE,
            choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
          )
        ),
        col_6(
          radioButtons(ns('dec'), labelInput("seldec"), c(',', '.'), inline = TRUE)
        ),
        radioSwitch(ns("deleteNA"), label = "selna", c("elim", "impu")), hr(),
        wellPanel(style = "height: 25vh; overflow: auto;",
                  withLoader(DT::dataTableOutput(ns('previewdatos')), 
                             type = "html", loader = "loader4")),
        hr()
      ),
      tabPanel(
        "Excel",
        fluidRow(
          style = "margin-right: 0px;margin-left: 0px;",
          col_6(fileInput(
            ns('archivo_xslx'), labelInput("selfile"), width = "100%",
            placeholder = "", buttonLabel = labelInput("subi"),
            accept = c('.xls', '.xlsx'))),
          col_6(numericInput(ns("num_hoja"), labelInput("nhoj"), 1, 1))
        ),
        fluidRow(
          style = "margin-right: 0px;margin-left: 0px;",
          col_6(checkboxInput(ns('header_xlsx'), labelInput("selhead"), value = TRUE)),
          col_6(checkboxInput(ns('rowname_xlsx'), labelInput("selrow"), value = TRUE))
        ),
        fluidRow(
          style = "margin-right: 0px;margin-left: 0px;",
          col_6(
            tags$b(labelInput("scell")),
            fluidRow(
              style = "margin-right: 0px;margin-left: 0px;",
              col_6(numericInput(ns("fila_inicio"), labelInput("row"), 1, 0)),
              col_6(numericInput(ns("col_inicio"), labelInput("col"), 1, 0))
            )
          ),
          col_6(
            tags$b(labelInput("ecell")),
            fluidRow(
              style = "margin-right: 0px;margin-left: 0px;",
              col_6(numericInput(ns("fila_final"), labelInput("row"), 0, 0)),
              col_6(numericInput(ns("col_final"), labelInput("col"), 0, 0))
            )
          )
        ),
        radioSwitch(ns("deleteNA_xlsx"), label = "selna", c("elim", "impu")), hr(),
        wellPanel(style = "height: 25vh; overflow: auto;",
                  withLoader(DT::dataTableOutput(ns('previewxlsx')), 
                             type = "html", loader = "loader4")),
        hr()
      )
    )
  )
  
  carga[[1]]$children[[1]] <- htmltools::tagAppendChild(
    carga[[1]]$children[[1]], tags$li(class = "header pull-right", tags$button(
      id = ns("run_data"), type = "button", class = "run-button action-button", 
      icon("play"), tags$a(labelInput("run"), style = "color:white"))))
  
  particion <- list(
    options.run(ns("run_pred")), tags$hr(style = "margin-top: 0px;"),
    selectInput(ns("sel.predic.var"), label = labelInput("selpred"), choices = ""),
    tabsetPanel(
      type = "tabs", id = ns("part_metodo"),
      tabPanel(
        labelInput("tt"),
        tags$b(labelInput("seed")),
        div(
          col_6(radioSwitch(ns("aseed"), NULL, c("habi", "desh"),
                            val.def = FALSE)),
          col_6(numericInput(ns("seed"), NULL, value = 5, width = "100%"))
        ),
        sliderInput(ns("n_tt"), label = div(
          div(style = 'float: left; color: #428bca;', labelInput('train')),
          div(style = 'float: right; color: #91cc75;', labelInput('test'))),
          5, 95, 80, 5)
      ),
      tabPanel(
        labelInput("cros"),
        div(
          col_6(numericInput(ns("numGrupos"), labelInput("ngr"), 5, 
                             width = "100%", min = 1)),
          col_6(numericInput(ns("numVC"), labelInput("nvc"), 1, 
                             width = "100%", min = 1))
        )
      )
    )
  )
  
  iconos    <- list(paste(labelInput("doccarga"), icon("database")), 
                    paste(labelInput("opts"), icon("gear")))
  widths    <- c(50, 50)
  heights   <- c(100, 100)
  contenido <- list(carga, particion)
  if(paquete == "discoveR") {
    iconos  <- list(paste(labelInput("doccarga"), icon("database")))
    widths  <- c(50)
    heights <- c(100)
    contenido <- list(carga)
  }
  
  opc_load <- tabsOptions(botones = iconos, widths = widths, 
                          heights = heights, tabs.content = contenido, 
                          id = "tabscarga")
  
  if(paquete == "discoveR") {
    open <- "tab-content box-option-open-center"
  } else {
    open <- "tab-content box-option-open-left"
  }
  
  inputTag <- tagList(
    tabBoxPrmdt(
      id = "data", title = NULL, opciones = opc_load, open = open,
      tabPanel(
        title = title,
        div(style = "height: 72vh; overflow: auto;",
            withLoader(DT::dataTableOutput(ns('tabladatos')), 
                       type = "html", loader = "loader4")))
    )
  )
  
  return(htmltools::attachDependencies(inputTag, deps))
}

#' carga_datos Server Functions.
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#' @param modelos shiny reactive values.
#' @param codedioma shiny reactive values.
#' @param paquete indicates if the data is going to be used for exploratory, predictive, or regression analysis.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return shiny server module.
#' @import caret
#' @import shiny
#' @export mod_carga_datos_server
#' 
mod_carga_datos_server <- function(id, updateData, modelos, codedioma, paquete = "predictoR") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    disyuntivas <- rv(valor = list(), nombre = NULL)
    sampleopt   <- rv(valor = 1)
    
    # Habilitada o deshabilitada la semilla
    observeEvent(input$aseed, {
      if (input$aseed) {
        shinyjs::enable("seed")
      } else {
        shinyjs::disable("seed")
      }
    })
    
    # Habilitada o deshabilitada la semilla
    observeEvent(input$prevfile, {
      ruta <- isolate(input$archivo)
      if(is.null(ruta)) {
        showNotification("ERROR CD035: Debe cargar un archivo.", 
                         type = "error")
      } else {
        con = file(ruta$datapath, "r")
        prev <- ""
        for (i in 1:10) {
          line = readLines(con, n = 1)
          if ( length(line) == 0 ) {
            break
          }
          prev <- paste0(prev, line, "<br>")
        }
        close(con)
        showModal(
          modalDialog(
            HTML(prev), style = "overflow: auto;", easyClose = TRUE,
            title = tr("vfil", codedioma$idioma), footer = NULL, size = "xl"
          )
        )
      }
    })
    
    # Renombrar columna tabla de datos.
    renombrar <- function(indice, nuevo_nombre) {
      nom.column <- colnames(updateData$datos.tabla)[indice]
      if(nom.column %not_in% colnames(updateData$datos)) {
        showNotification("ERROR CD040: Cant rename an eliminated column.",
                         type = "error")
      } else {
        pos1 <- which(colnames(updateData$datos) == nom.column)
        pos2 <- which(colnames(updateData$datos.tabla) == nom.column)
        pos3 <- which(colnames(updateData$originales) == nom.column)
        colnames(updateData$datos)[pos1]       <- nuevo_nombre
        colnames(updateData$datos.tabla)[pos2] <- nuevo_nombre
        colnames(updateData$originales)[pos3]  <- nuevo_nombre
        
        cod <- paste0(
          "### docrename\n", 
          "colnames(datos)[", pos1, "] <- ", nuevo_nombre, "\n")
        codedioma$code <- append(codedioma$code, cod)
      }
    }
    
    # Transformar columna tabla de datos.
    transformar <- function(indice, nuevo_tipo) {
      datos       <- updateData$datos
      datos.tabla <- updateData$datos.tabla
      originales  <- updateData$originales
      nom.column  <- colnames(datos.tabla)[indice]
      cod <- "### doctrans\n"
      
      if(nom.column %not_in% colnames(datos)) {
        showNotification("ERROR CD050: Cant transform an eliminated column.",
                         type = "error")
      } else {
        if(nom.column %in% colnames(originales)) {
          if(nuevo_tipo == "cat" & 
             class(datos[, nom.column]) %in% c("numeric", "integer")) {
            datos[, nom.column]       <- as.factor(datos[, nom.column])
            datos.tabla[, nom.column] <- as.factor(datos.tabla[, nom.column])
            cod <- paste0(cod, "datos[, '", nom.column, "'] <- as.factor(datos[, '", nom.column, "'])\n")
          }
          if(nuevo_tipo == "num" & 
             !(class(datos[, nom.column]) %in% c("numeric", "integer"))) {
            nueva.var <- gsub(",", ".", as.character(datos[, nom.column]))
            nueva.var <- as.numeric(nueva.var)
            if(any(is.na(nueva.var))) {
              showNotification("ERROR CD050: Can't transform text to numeric. To do this, apply disjunctive code.",
                               type = "error")
              cod <- ""
            } else {
              datos[, nom.column]       <- nueva.var
              datos.tabla[, nom.column] <- nueva.var
              cod <- paste0(
                cod, "nueva.var <- gsub(',', '.', as.character(datos[, ", nom.column, "]))\n",
                "datos[, '", nom.column, "'] <- as.numeric(nueva.var)\n")
            }
          }
          if(nuevo_tipo == "dis") {
            tipo.original <- ifelse(class(datos[, nom.column]) %in% c("numeric","integer"), "num", "cat")
            disyuntivas$valor[[nom.column]] <- list(
              original = datos[, nom.column], 
              nuevo    = valores.disyuntivos(datos, nom.column),
              tipo     = tipo.original)
            datos[, nom.column]       <- NULL
            datos.tabla[, nom.column] <- NULL
            
            for (cat in disyuntivas$valor[[nom.column]]$nuevo$categorias) {
              datos[, cat]       <- disyuntivas$valor[[nom.column]]$nuevo$valores[[cat]]
              datos.tabla[, cat] <- disyuntivas$valor[[nom.column]]$nuevo$valores[[cat]]
            }
            
            cod <- paste0(
              cod, "datos <- datos.disyuntivos(datos, '", nom.column,"')\n",
              "datos[, '", nom.column, "'] <- NULL\n")
          }
        } else {
          nom.split <- unlist(strsplit(nom.column, ".", fixed = TRUE))
          nom.aux   <- nom.split[1]
          for (i in 2:length(nom.split)) {
            if(nom.aux %in% colnames(originales))
              break
            else
              nom.aux <- paste0(nom.aux, "." , nom.split[i])
          }
          
          cod <- paste0(
            cod, "datos <- devolver.disyuntivos(datos, '", nom.aux,"')\n")
          
          if(nuevo_tipo == "cat") {
            datos[, nom.aux]       <- as.factor(disyuntivas$valor[[nom.aux]]$original)
            datos.tabla[, nom.aux] <- as.factor(disyuntivas$valor[[nom.aux]]$original)
            
            for (cat in disyuntivas$valor[[nom.aux]]$nuevo$categorias) {
              datos[, cat]       <- NULL
              datos.tabla[, cat] <- NULL
            }
            
            cod <- paste0(
              cod, "datos[, '", nom.aux, "'] <- as.factor(datos[, '", nom.aux, "'])\n")
          }
          if(nuevo_tipo == "num") {
            datos[, nom.aux]       <- as.numeric(as.character(disyuntivas$valor[[nom.aux]]$original))
            datos.tabla[, nom.aux] <- as.numeric(as.character(disyuntivas$valor[[nom.aux]]$original))
            
            for (cat in disyuntivas$valor[[nom.aux]]$nuevo$categorias) {
              datos[, cat]       <- NULL
              datos.tabla[, cat] <- NULL
            }
            
            cod <- paste0(cod, "datos[, '", nom.aux, "'] <- as.numeric(as.character(datos[, '", nom.aux, "']))\n")
          }
        }
      }
      updateData$datos       <- datos
      updateData$datos.tabla <- datos.tabla
      codedioma$code <- append(codedioma$code, cod)
    }
    
    # Ordena columna tabla de datos.
    ordenar <- function(indice, decreasing) {
      orden <- order(updateData$datos.tabla[[indice]], decreasing = decreasing)
      updateData$datos.tabla <- updateData$datos.tabla[orden, ]
    }
    
    # Seleccionar validación columna tabla de datos.
    seleccionar <- function(indice, i) {
      grupos <- updateData$grupos[[as.numeric(i)]]
      nom.grupo  <- vector(mode = "character", length = nrow(updateData$datos.tabla))
      for (grupo in 1:length(grupos)) {
        nom.grupo[grupos[[grupo]]] <- paste0("Gr_", grupo)
      }
      sampleopt$valor <- i
      
      updateData$datos.tabla$part <- as.factor(nom.grupo)
    }
    
    # Eliminar columna tabla de datos.
    eliminar <- function(indice) {
      originales  <- updateData$originales
      datos.tabla <- updateData$datos.tabla
      nom.col     <- colnames(datos.tabla)[indice]
      
      cod <- "### doceliminar\n"
      
      if(nom.col %in% colnames(originales)) {
        if(nom.col %not_in% colnames(updateData$datos)) {
          updateData$datos[, nom.col] <- datos.tabla[, nom.col]
          cod <- paste0(cod, "datos[['", nom.col,"']] <- ", nom.col, "\n")
          codedioma$code <- append(codedioma$code, cod)
          showNotification("Column successfully restored.", type = "message")
        } else {
          if(dim(updateData$datos)[2] > 2) {
            updateData$datos[, nom.col] <- NULL
            cod <- paste0(cod, nom.col, " <- datos[['", nom.col,"']]\n",
                          "datos[['", nom.col,"']] <- NULL\n")
            codedioma$code <- append(codedioma$code, cod)
          }
          else {
            showNotification("ERROR CD070: The dataset must have at least 2 columns.", type = "warning")
          }
        }
      } else {
        showNotification("ERROR CD060: Cant remove a disyuntive column.", type = "message")
      }
    }
    
    observeEvent(input$accion, {
      acciones <- input$accion
      nombre   <- colnames(updateData$datos.tabla)[as.numeric(acciones[1])]
      
      if(acciones[2] == "s") {
        seleccionar(as.numeric(acciones[1]), acciones[3])
      } else if(acciones[2] == "a") {
        ordenar(as.numeric(acciones[1]), F)
      } else if(acciones[2] == "d") {
        ordenar(as.numeric(acciones[1]), T)
      } else if(nombre != "part") {
        if(acciones[2] == "e") {
          eliminar(as.numeric(acciones[1]))
        } else if(acciones[2] == "r") {
          renombrar(as.numeric(acciones[1]), acciones[3])
        } else if(acciones[2] == "t") {
          transformar(as.numeric(acciones[1]), acciones[3])
        }
        
        restaurar.validacion(updateData)
        restaurar.segmentacion(updateData)
      } else {
        showNotification("ERROR CD008: Cant transform the selected column.", type = "message")
      }
    })
    
    # Load Button Function
    observeEvent(input$run_data, {
      updateData$datos       <- NULL
      updateData$datos.tabla <- NULL
      updateData$originales  <- NULL
      disyuntivas$valor      <- NULL
      disyuntivas$nombre     <- NULL
      
      restaurar.segmentacion(updateData)
      restaurar.validacion(updateData)
      
      tryCatch({
        if(input$file_type == "<span data-id=\"texf\"></span>") {
          rowname    <- isolate(input$rowname)
          ruta       <- isolate(input$archivo)
          sep        <- isolate(input$sep)
          dec        <- isolate(input$dec)
          encabezado <- isolate(input$header)
          deleteNA   <- isolate(input$deleteNA)
          
          updateData$originales <- carga.datos(
            rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
          
          cod <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
          codedioma$code <- append(codedioma$code, cod)
        } else {
          ruta        <- isolate(input$archivo_xslx)
          num_hoja    <- isolate(input$num_hoja)
          encabezado  <- isolate(input$header_xlsx)
          rowname     <- isolate(input$rowname_xlsx)
          fila_inicio <- isolate(input$fila_inicio)
          col_inicio  <- isolate(input$col_inicio)
          fila_final  <- isolate(input$fila_final)
          col_final   <- isolate(input$col_final)
          deleteNA    <- isolate(input$deleteNA_xlsx)
          
          updateData$originales <- carga.datos.excel(
            ruta$datapath, num_hoja, encabezado, fila_inicio, col_inicio, 
            fila_final, col_final, rowname, deleteNA)
          
          cod <- code.carga.excel(
            ruta$name, num_hoja, encabezado, fila_inicio, col_inicio, 
            fila_final, col_final, rowname, deleteNA)
          codedioma$code <- append(codedioma$code, cod)
        }
        
        if(ncol(updateData$originales) <= 1) {
          showNotification("ERROR CD010: Check Separators", duration = 10, type = "error")
          updateData$originales  <- NULL
          updateData$datos       <- NULL
          updateData$datos.tabla <- NULL
        } else {
          updateData$datos       <- updateData$originales
          updateData$datos.tabla <- updateData$originales
          shinyjs::runjs("document.getElementById('tabscarga').parentElement.className = 'tab-content';")
        }
      }, error = function(e) {
        updateData$originales  <- NULL
        updateData$datos       <- NULL
        updateData$datos.tabla <- NULL
        showNotification(paste0("ERROR CD020: ", e), type = "error")
      })
    })
    
    # Update preview data on table
    output$previewdatos <- DT::renderDataTable({
      rowname    <- input$rowname
      ruta       <- input$archivo
      sep        <- input$sep
      dec        <- input$dec
      encabezado <- input$header
      deleteNA   <- input$deleteNA
      
      idioma <- codedioma$idioma
      tipos  <- c(tr("num", idioma), tr("cat", idioma))
      
      tryCatch({
        if(is.null(ruta)) {
          return(NULL)
        }
        preview <- carga.datos(
          rowname, ruta$datapath, sep, dec, encabezado, deleteNA, T)
        DT::datatable(
          preview, options = list(dom = 'rt', ordering = FALSE), 
          selection = 'none', container = prevsketch(preview, tipos)
        )
      }, error = function(e) {
        stop(e)
      })
    })
    
    # Update preview xlsx on table
    output$previewxlsx <- DT::renderDataTable({
      ruta        <- input$archivo_xslx
      num_hoja    <- input$num_hoja
      encabezado  <- input$header_xlsx
      rowname     <- input$rowname_xlsx
      fila_inicio <- input$fila_inicio
      col_inicio  <- input$col_inicio
      fila_final  <- input$fila_final
      col_final   <- input$col_final
      deleteNA    <- input$deleteNA_xlsx
      
      idioma <- codedioma$idioma
      tipos  <- c(tr("num", idioma), tr("cat", idioma))
      
      tryCatch({
        if(is.null(ruta)) {
          return(NULL)
        }
        preview <- carga.datos.excel(
          ruta$datapath, num_hoja, encabezado, fila_inicio, col_inicio, 
          fila_final, col_final, rowname, deleteNA, T)
        DT::datatable(
          preview, options = list(dom = 'rt', ordering = FALSE), 
          selection = 'none', container = prevsketch(preview, tipos)
        )
      }, error = function(e) {
        stop("ERROR: ", e)
      })
    })
    
    # Update data on table
    output$tabladatos <- DT::renderDataTable({
      datos        <- updateData$datos
      datos.tabla  <- updateData$datos.tabla
      originales   <- updateData$originales
      idioma       <- codedioma$idioma
      tipos  <- c(tr("num", idioma), tr("cat", idioma))
      res  <- NULL
      
      tryCatch({
        if(!is.null(datos.tabla) && !is.null(datos)) {
          tipo.columnas <- sapply(colnames(datos.tabla), function(i)
            ifelse(class(datos.tabla[,i]) %in% c("numeric", "integer"),
                   paste0("<span data-id='num'><i class='fa fa-sort-numeric-up wrapper-tag'></i><br>", tipos[1], "</span>"),
                   paste0("<span data-id='cat'><i class='fa fa-font wrapper-tag'></i><br>", tipos[2], "</span>")))
          
          if(colnames(datos.tabla)[1] == "part") {
            if("Gr_1" %in% datos.tabla$part) {
              tipo.columnas[1] <- paste0(
                '<div>\n',
                '  <span>\n',
                '    ', tr("vali", idioma), '\n',
                '  </span>\n',
                selectInputGroup(ns('accion'), datos.tabla, 1, idioma, length(updateData$grupos), isolate(sampleopt$valor)),
                '</div>'
              )
            } else {
              tipo.columnas[1] <- "<span></span>"
            }
          }
          
          nombres <- setdiff(colnames(datos.tabla), colnames(datos))
          res     <- DT::datatable(
            datos.tabla, selection = 'none', editable = TRUE,
            extensions = 'Buttons',
            container = sketch(
              ns('accion'), datos.tabla, datos, originales, idioma, "part", tipo.columnas),
            options = list(dom = 'Bfrtip', ordering = FALSE, buttons = list(list(
              extend = 'csv', filename = "data", header = TRUE,
              exportOptions = list(
                modifier = list(page = "all"),
                format = list(
                  header = DT::JS(paste0(
                    "function ( data, columnIdx ) {\n",
                    "  aux = ['ID', '", paste(colnames(datos.tabla), collapse = "', '"), "']\n",
                    "  return aux[columnIdx];\n", 
                    "}"))
                )
              ),
              text = '<i class="fa fa-file-csv"></i>'),
              list(
                extend = 'excel', filename = "data", header = TRUE,
                exportOptions = list(
                  modifier = list(page = "all"),
                  format = list(
                    header = DT::JS(paste0(
                      "function ( data, columnIdx ) {\n",
                      "  aux = ['ID', '", paste(colnames(datos.tabla), collapse = "', '"), "']\n",
                      "  return aux[columnIdx];\n", 
                      "}"))
                  )
                ),
                text = '<i class="fa fa-file-excel"></i>')))
            ) |>
            formatStyle(columns = nombres, color = 'black', background = '#CAC9C9')
          
          if("part" %in% colnames(datos.tabla)) {
            colores <- gg_color_hue(length(unique(datos.tabla[["part"]])))
            res$x$data$part <- tr(as.character(res$x$data$part), idioma)
            
            if(tr("train", idioma) %in% unique(res$x$data$part)) {
              res <- res |> formatStyle(
                columns = "part", 
                backgroundColor = styleEqual(c(tr("train", idioma), tr("test", idioma)), colores))
            } else {
              res <- res |> formatStyle(
                columns = "part", 
                backgroundColor = styleEqual(levels(datos.tabla[["part"]]), colores))
            }
          }
        }
        res
      }, error = function(e) {
        showNotification(paste0("ERROR CD030: ", e), type = "error")
        return(NULL)
      })
    }, server = FALSE)
    
    # Update Predict Variable
    observeEvent(updateData$datos, {
      datos <- updateData$datos
      if(paquete == "predictoR") {
        vars  <- rev(colnames.empty(var.categoricas(datos)))
        updateSelectInput(session, "sel.predic.var", choices = vars)
      } else if(paquete == "regressoR") {
        vars  <- rev(colnames.empty(var.numericas(datos)))
        updateSelectInput(session, "sel.predic.var", choices = vars)
      }
    })
    
    # Segment Button Function
    observeEvent(input$run_pred, {
      for (nom in names(modelos)) {
        modelos[[nom]] <- NULL
      }
      restaurar.segmentacion(updateData)
      restaurar.validacion(updateData)
      
      variable <- isolate(input$sel.predic.var)
      datos    <- updateData$datos
      idioma   <- isolate(codedioma$idioma)
      tryCatch({
        if(variable != "") {
          
          updateData$datos.tabla[["part"]] <- NULL
          if(input$part_metodo == "<span data-id=\"tt\"></span>") {
            porcentaje <- isolate(input$n_tt)
            variable   <- isolate(input$sel.predic.var)
            seed       <- isolate(input$seed)
            aseed      <- isolate(input$aseed)
            
            res <- segmentar.datos(datos, variable, porcentaje, seed, aseed)
            updateData$variable.predecir <- variable
            updateData$datos.prueba      <- res$test
            updateData$datos.aprendizaje <- res$train
            nom.part <- vector(mode = "character", length = nrow(datos))
            nom.part[res$indices]  <- "train"
            nom.part[-res$indices] <- "test"
            updateData$datos.tabla <- cbind(part = as.factor(nom.part), updateData$datos.tabla)
            updateData$indices     <- res$indices
            
            cod <- code.segment.tt(variable, porcentaje, seed, aseed)
            codedioma$code <- append(codedioma$code, cod)
            
          } else {
            sampleopt$valor <- 1
            num.grupos <- isolate(input$numGrupos)
            num.valC   <- isolate(input$numVC)
            grupos     <- vector(mode = "list", length = num.valC)
            tabla.aux  <- updateData$datos.tabla
            nom.grupo  <- vector(mode = "character", length = nrow(tabla.aux))
            
            if(num.valC > 20 | num.valC < 1) {
              msg <- paste0(
                "ERROR (CD040): El numero de validaciones cruzadas no es valida. ",
                "Debe ser un valor entre 1 y 20.")
              showNotification(msg, type = "error")
              stop()
            }
            if(num.grupos > nrow(datos) | num.grupos < 1) {
              msg <- paste0(
                "ERROR (CD045): La cantidad de grupos no es valida. ",
                "Debe ser un valor mayor 1 y menor a la cantidad de filas de los datos.")
              showNotification(msg, type = "error")
              stop()
            }
            
            for(i in 1:num.valC) {
              grupo     <- createFolds(datos[, variable], num.grupos)  
              grupos[i] <- list(grupo)
            }
            updateData$variable.predecir <- variable
            updateData$numGrupos         <- num.grupos
            updateData$grupos            <- grupos
            updateData$numValC           <- num.valC
            
            grupos <- updateData$grupos[[1]]
            for (grupo in 1:length(grupos)) {
              nom.grupo[grupos[[grupo]]] <- paste0("Gr_", grupo)
            }
            
            updateData$datos.tabla <- cbind(part = as.factor(nom.grupo), updateData$datos.tabla)
            
            cod <- code.segment.vc(variable, num.valC, num.grupos)
            codedioma$code <- append(codedioma$code, cod)
          }
        }
      }, error = function(e) {
        restaurar.segmentacion(updateData)
        showNotification(paste0("ERROR (CD050): ", e), type = "error")
      })
    })
    
    # Descarga tabla de datos
    output$downloaDatos <- downloadHandler(
      filename = function() {
        input$archivo$name
      },
      content = function(file) {
        fwrite(updateData$datos, file, row.names = TRUE)
      }
    )
  })
}

## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")

## To be copied in the server
# mod_carga_datos_server("carga_datos_ui_1")
