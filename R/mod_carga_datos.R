#' carga_datos UI Function
#'
#' @param id Internal parameters for {shiny}.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny ui
#' @export mod_carga_datos_ui
#' @examples
#' mod_carga_datos_ui("carga_datos_ui_1")
#' 
mod_carga_datos_ui <- function(id){
  ns <- NS(id)
  
  opc_load <- tabsOptions(
    botones = list(icon("database"), icon("cog")),
    widths = c(50, 50), heights = c(100, 100),
    tabs.content = list(
      list(
        options.run(ns("run_data")), tags$hr(style = "margin-top: 0px;"),
        fileInput(
          ns('archivo'), labelInput("selfile"), width = "100%",
          placeholder = "", buttonLabel = labelInput("subi"),
          accept = c('text/csv', '.csv', '.txt')),
        col_6(checkboxInput(ns('header'), labelInput("selhead"), value = T)),
        col_6(checkboxInput(ns('rowname'), labelInput("selrow"), value = T)),
        col_6(
          radioButtons(
            ns('sep'), labelInput("selsep"), inline = T,
            choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
          )
        ),
        col_6(
          radioButtons(ns('dec'), labelInput("seldec"), c(',', '.'), inline = T)
        ),
        radioSwitch(ns("deleteNA"), label = "selna", c("elim", "impu")),
        hr(),
        wellPanel(style = "height: 25vh; overflow: auto;",
                  withLoader(DT::dataTableOutput(ns('previewdatos')), 
                             type = "html", loader = "loader4")),
        hr()
      ),
      list(
        options.run(ns("run_pred")), tags$hr(style = "margin-top: 0px;"),
        selectInput(ns("sel.predic.var"), label = labelInput("selpred"), choices = ""),
        tabsetPanel(
          type = "tabs", id = ns("part_metodo"),
          tabPanel(
            labelInput("tt"),
            numericInput(ns("seed"), labelInput("seed"), 
                         value = 5, width = "100%"),
            # div(
            #   col_6(numericInput(ns("seed"), labelInput("seed"), 
            #                      value = 5, width = "100%")), 
            #   col_6(radioSwitch(ns("aseed"), NULL, 
            #                     c("habilitada", "deshabilitada"), val.def = F))
            # ),
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
              col_6(numericInput(ns("numVC"), labelInput("nvc"), 5, 
                                 width = "100%", min = 1))
            )
          )
        )
      )
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = "data", title = NULL, opciones = opc_load,
      open = "tab-content box-option-open-left",
      tabPanel(
        title = labelInput("data"),
        div(style = "height: 72vh; overflow: auto;",
            withLoader(DT::dataTableOutput(ns('tabladatos')), 
                       type = "html", loader = "loader4")))
    )
  )
}

#' carga_datos Server Functions
#'
#' @param id Internal parameters for {shiny}.
#' @param updateData shiny reactive values.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return shiny server
#' @import caret
#' @export mod_carga_datos_server
#' 
mod_carga_datos_server <- function(id, updateData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    disyuntivas <- rv(valor = list(), nombre = NULL)
    
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
      }
    }
    
    # Transformar columna tabla de datos.
    transformar <- function(indice, nuevo_tipo) {
      datos       <- updateData$datos
      datos.tabla <- updateData$datos.tabla
      originales  <- updateData$originales
      nom.column  <- colnames(datos.tabla)[indice]
      
      if(nom.column %not_in% colnames(datos)) {
        showNotification("ERROR CD050: Cant transform an eliminated column.",
                         type = "error")
      } else {
        if(nom.column %in% colnames(originales)) {
          if(nuevo_tipo == "cat" & 
             class(datos[, nom.column]) %in% c("numeric", "integer")) {
            datos[, nom.column]       <- as.factor(datos[, nom.column])
            datos.tabla[, nom.column] <- as.factor(datos.tabla[, nom.column])
          }
          if(nuevo_tipo == "num" & 
             !(class(datos[, nom.column]) %in% c("numeric", "integer"))) {
            datos[, nom.column]       <- as.numeric(as.character(datos[, nom.column]))
            datos.tabla[, nom.column] <- as.numeric(as.character(datos.tabla[, nom.column]))
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
          }
        } else {
          nom.split <- unlist(strsplit(nom.column, ".", fixed = TRUE))
          nom.aux   <- nom.split[1]
          for (i in 2:length(nom.split)) {
            if(nom.aux %in% colnames(originales))
              break
            else
              nom.aux <- paste0(nom.aux,"." ,nom.split[i])
          }
          if(nuevo_tipo == "cat") {
            datos[, nom.aux]       <- as.factor(disyuntivas$valor[[nom.aux]]$original)
            datos.tabla[, nom.aux] <- as.factor(disyuntivas$valor[[nom.aux]]$original)
            
            for (cat in disyuntivas$valor[[nom.aux]]$nuevo$categorias) {
              datos[, cat]       <- NULL
              datos.tabla[, cat] <- NULL
            }
          }
          if(nuevo_tipo == "num") {
            datos[, nom.aux]       <- as.numeric(as.character(disyuntivas$valor[[nom.aux]]$original))
            datos.tabla[, nom.aux] <- as.numeric(as.character(disyuntivas$valor[[nom.aux]]$original))
            
            for (cat in disyuntivas$valor[[nom.aux]]$nuevo$categorias) {
              datos[, cat]       <- NULL
              datos.tabla[, cat] <- NULL
            }
          }
        }
      }
      updateData$datos       <- datos
      updateData$datos.tabla <- datos.tabla
    }
    
    # Eliminar columna tabla de datos.
    eliminar <- function(indice) {
      originales  <- updateData$originales
      datos.tabla <- updateData$datos.tabla
      nom.col     <- colnames(datos.tabla)[indice]
      
      if(nom.col %in% colnames(originales)) {
        if(nom.col %not_in% colnames(updateData$datos)) {
          updateData$datos[, nom.col] <- datos.tabla[,nom.col]
          showNotification("Columna restaurada correctamente.", type = "message")
        } else {
          if(dim(updateData$datos)[2] > 2)
            updateData$datos[, nom.col] <- NULL
          else
            showNotification("ERROR CD070: The dataset must have at least 2 columns.", type = "warning")
        }
      } else {
        showNotification("ERROR CD060: Cant remove a disyuntive column.", type = "message")
      }
    }
    
    observeEvent(input$accion, {
      acciones <- input$accion
      nombres  <- colnames(updateData$datos.tabla)
      nombre   <- colnames(updateData$datos.tabla)[as.numeric(acciones[1])]
      
      if(tr("part", updateData$idioma) != nombre) {
        if(acciones[2] == "e") {
          eliminar(as.numeric(acciones[1]))
        } else if(acciones[2] == "r") {
          renombrar(as.numeric(acciones[1]), acciones[3])
        } else if(acciones[2] == "t") {
          transformar(as.numeric(acciones[1]), acciones[3])
        }
        
        #restaurar.validacion()
        #restaurar.segmentacion()
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
      
      rowname    <- isolate(input$rowname)
      ruta       <- isolate(input$archivo)
      sep        <- isolate(input$sep)
      dec        <- isolate(input$dec)
      encabezado <- isolate(input$header)
      deleteNA   <- isolate(input$deleteNA)
      
      tryCatch({
        codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
        updateAceEditor(session, "fieldCode", value = codigo)
        
        updateData$originales <- carga.datos(
          rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
        if(ncol(updateData$originales) <= 1) {
          showNotification("ERROR CD010: Check Separators", duration = 10, type = "error")
          updateData$originales  <- NULL
          updateData$datos       <- NULL
          updateData$datos.tabla <- NULL
        } else {
          updateData$datos       <- updateData$originales
          updateData$datos.tabla <- updateData$originales
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
      
      idioma <- updateData$idioma
      tipos  <- c(tr("num", idioma), tr("cat", idioma))
      
      tryCatch({
        if(is.null(ruta)) {
          return(NULL)
        }
        preview <- carga.datos(
          rowname, ruta$datapath, sep, dec, encabezado, deleteNA, T)
        DT::datatable(
          preview, options = list(dom = 'rt', ordering = F), 
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
      idioma       <- updateData$idioma
      tipos  <- c(tr("num", idioma), tr("cat", idioma))
      part <- tr("part", idioma)
      res  <- NULL
      tryCatch({
        if(!is.null(datos.tabla) && !is.null(datos)) {
          tipo.columnas <- sapply(colnames(datos.tabla), function(i)
            ifelse(class(datos.tabla[,i]) %in% c("numeric", "integer"),
                   paste0("<span data-id='num'><i class='fa fa-sort-numeric-up wrapper-tag'></i><br>", tipos[1], "</span>"),
                   paste0("<span data-id='cat'><i class='fa fa-font wrapper-tag'></i><br>", tipos[2], "</span>")))
          
          if(colnames(datos.tabla)[1] == part) {
            tipo.columnas[1] <- "<span></span>"
          }
          
          nombres <- setdiff(colnames(datos.tabla), colnames(datos))
          res     <- DT::datatable(
            datos.tabla, selection = 'none', editable = TRUE,  
            container = sketch(datos.tabla, datos, originales, idioma, part, tipo.columnas), 
            options = list(dom = 'frtip', ordering = F)) |>
            formatStyle(columns = nombres, color = 'black', background = '#CAC9C9')
          
          if(tr("part", idioma) %in% colnames(datos.tabla)) {
            colores <- gg_color_hue(length(unique(datos.tabla[[tr("part", idioma)]])))
            
            if(tr("train", idioma) %in% unique(datos.tabla[[tr("part", idioma)]])) {
              res <- res |> formatStyle(
                columns = tr("part", idioma), 
                backgroundColor = styleEqual(c(tr("train", idioma), tr("test", idioma)), colores))
            } else {
              res <- res |> formatStyle(
                columns = tr("part", idioma), 
                backgroundColor = styleEqual(levels(datos.tabla[[tr("part", idioma)]]), colores))
            }
          }
        }
        res
      }, error = function(e) {
        showNotification(paste0("ERROR CD030: ", e), type = "error")
        return(NULL)
      })
    }, server = T)
    
    # Update Predict Variable
    observeEvent(updateData$datos, {
      datos <- updateData$datos
      vars  <- rev(colnames.empty(var.categoricas(datos)))
      updateSelectInput(session, "sel.predic.var", choices = vars)
    })
    
    # Segment Button Function
    observeEvent(input$run_pred, {
      # for (nom in names(modelos)) {
      #   modelos[[nom]] <- NULL
      # }
      # restaurar.segmentacion()
      # restaurar.validacion()
      
      variable <- isolate(input$sel.predic.var)
      datos    <- updateData$datos
      idioma   <- isolate(updateData$idioma)
      tryCatch({
        if(variable != "") {
          # codigo.editor <- code.segment(porcentaje,
          #                               variable,
          #                               semilla,
          #                               permitir.semilla)
          # updateAceEditor(session, "fieldCodeSegment", value = codigo.editor)
          
          if(input$part_metodo == "<span data-id=\"tt\"></span>") {
            porcentaje <- isolate(input$n_tt)
            variable   <- isolate(input$sel.predic.var)
            semilla    <- isolate(input$seed)
            permitir.semilla <- F
            #permitir.semilla <- isolate(input$permitir.semilla)
            
            res <- segmentar.datos(datos, variable, porcentaje, semilla, permitir.semilla)
            updateData$datos.prueba      <- res$test
            updateData$datos.aprendizaje <- res$train
            nom.part <- vector(mode = "character", length = nrow(datos))
            nom.part[res$indices]  <- tr("train", idioma)
            nom.part[-res$indices] <- tr("test", idioma)
            tabla.aux <- updateData$datos.tabla
            updateData$datos.tabla <- NULL
            if(tr("part", idioma) %in% colnames(tabla.aux))
              tabla.aux[[tr("part", idioma)]] <- NULL
            aux           <- data.frame(as.factor(nom.part))
            colnames(aux) <- tr("part", idioma)
            tabla.aux     <- cbind(aux, tabla.aux)
            updateData$indices     <- res$indices
            updateData$datos.tabla <- tabla.aux
            
          } else {
            num.grupos <- isolate(input$numGrupos)
            num.valC   <- isolate(input$numVC)
            grupos     <- vector(mode = "list", length = num.valC)
            tabla.aux <- updateData$datos.tabla
            nom.grupo  <- vector(mode = "character", length = nrow(tabla.aux))
            
            for(i in 1:num.valC) {
              grupo     <- createFolds(datos[, variable], num.grupos)  
              grupos[i] <- list(grupo)
            }
            updateData$variable.predecir <- variable
            updateData$numGrupos         <- num.grupos
            updateData$grupos            <- grupos
            
            grupos <- updateData$grupos[[1]]
            for (grupo in 1:length(grupos)) {
              nom.grupo[grupos[[grupo]]] <- paste0("Gr_", grupo)
            }
            if(tr("part", idioma) %in% colnames(tabla.aux))
              tabla.aux[[tr("part", idioma)]] <- NULL
            
            aux           <- data.frame(as.factor(nom.grupo))
            colnames(aux) <- tr("part", idioma)
            tabla.aux     <- cbind(aux, tabla.aux)
            updateData$datos.tabla <-  tabla.aux
          }
        }
      }, error = function(e) {
        #borrar.modelos(updateData)
        showNotification(paste0("ERROR al segmentar los datos: ", e), type = "error")
      })
    })
    
    # Descarga tabla de datos
    output$downloaDatos <- downloadHandler(
      filename = function() {
        input$archivo$name
      },
      content = function(file) {
        fwrite(updateData$datos, file, row.names = T)
      }
    )
  })
}

## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")

## To be copied in the server
# mod_carga_datos_server("carga_datos_ui_1")
