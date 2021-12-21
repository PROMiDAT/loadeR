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
  
  tagList(
    box(
      width = 12, title = labelInput("data"), closable = F,
      status = "primary", solidHeader = TRUE, collapsible = F,
      sidebar = boxSidebar(
        id = "boxdata", width = 40, startOpen = TRUE,
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
        wellPanel(style = "height: 30vh;overflow: auto;",
            withLoader(DT::dataTableOutput(ns('previewdatos')), 
                       type = "html", loader = "loader4")),
        hr(),
        actionButton(ns("loadButton"), labelInput("carg"), width = "100%")
      ),
      div(style = "height: 80vh;",
          withLoader(DT::dataTableOutput(ns('tabladatos')), 
                     type = "html", loader = "loader4"))
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
    observeEvent(input$loadButton, {
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
            colores <- alpha(gg_color_hue(length(unique(datos.tabla[[tr("part", idioma)]]))), 0.6)
            
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
