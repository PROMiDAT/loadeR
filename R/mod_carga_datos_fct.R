accion.NAs <- function(datos, deleteNA = T) {
  if(deleteNA) {
    return(na.omit(datos))
  } else {
    moda <- function(x) x[which.max(summary(x))]
    
    for (var in colnames(datos)) {
      if(any(is.na(datos[, var]))) {
        if(class(datos[, var]) %in% c('numeric', 'integer')) {
          datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)
        } else {
          datos[, var][is.na(datos[, var])] <- moda(datos[, var])
        }
      }
    }
    return(datos)
  }
}

carga.datos <- function(
  nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", 
  encabezado = T, deleteNA = T, preview = F) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  
  if(preview) {
    res <- fread(
      ruta, sep = separador, dec = sep.decimal, header = encabezado, 
      stringsAsFactors = T, data.table = F, check.names = T, nrows = 10)
  } else {
    res <- fread(
      ruta, sep = separador, dec = sep.decimal, header = encabezado, 
      stringsAsFactors = T, data.table = F, check.names = T)
  }
  
  if(nombre.filas) {
    row.names(res) <- res[[1]]
    res[[1]] <- NULL
  }
  return(accion.NAs(res, deleteNA))
}

carga.datos.excel <- function(
  ruta, sheet = 1, header = T, startRow = 0, startCol = 0, endRow = 0,
  endCol = 0, row_names = T, deleteNA = T, preview = F) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  
  if(preview) {
    if(endRow < 10) {
      endRow <- endRow
    } else {
      endRow <- 10
    }
  }
  res <- readWorksheetFromFile(
    ruta, sheet = sheet, header = header, startRow = startRow,
    startCol = startCol, endRow = endRow, endCol = endCol)
  
  if(row_names) {
    row.names(res) <- res[[1]]
    res[[1]] <- NULL
  }
  return(accion.NAs(res, deleteNA))
}

valores.disyuntivos <- function(data, var) {
  if(is.null(data)) {
    return(NULL)
  }
  
  categorias <- as.character(unique(data[, var]))
  valores    <- vector(mode = "list", length(categorias))
  
  for (i in 1:length(categorias)) {
    valores[[i]]  <- as.numeric(data[, var] == categorias[i])
    categorias[i] <- paste0(var, '.', categorias[i])
  }
  names(valores) <- categorias
  
  return(list(categorias = categorias, valores = valores))
}

selectInputTrans <- function(datos, var, idioma = "es", originales) {
  if(class(datos[, var]) %in% c("numeric", "integer")) {
    cat <- tags$input(
      type = "radio", value = "cat", name = paste0("radio_", var),
      onclick = paste0("accion(", var, ", 't', this.value)"))
  } else {
    cat <- tags$input(
      type = "radio", value = "cat", name = paste0("radio_", var), 
      onclick = paste0("accion(", var, ", 't', this.value)"), checked = "")
  }
  
  if(colnames(datos)[var] %in% colnames(originales)){
    dis <- tags$input(
      type = "radio", value = "dis", name = paste0("radio_", var),
      onclick = paste0("accion(", var, ", 't', this.value)"))
  } else {
    dis <- tags$input(
      type = "radio", value = "dis", name = paste0("radio_", var), 
      onclick = paste0("accion(", var, ", 't', this.value)"), checked = "")
  }
  
  tags$div(
    class = "radio-trans",
    tags$label(
      tr("num", idioma), class = "label-trans",
      tags$input(type = "radio", value = "num", name = paste0("radio_", var),
                 onclick = paste0("accion(", var, ", 't', this.value)"),
                 checked = "")
    ),
    tags$label(tr("cat", idioma), class = "label-trans", cat),
    tags$label(tr("dis", idioma), class = "label-trans", dis)
  )
}

btnInputArrange <- function(datos, var, idioma = "es") {
  if(class(datos[, var]) %in% c("numeric", "integer")) {
    asc <- tags$button(
      type = "button", class = "btn btn-default action-button",
      style = "margin-bottom: 10px;width: 70%;margin-right: 5px;",
      onclick = paste0("accion(", var, ", 'a')"),
      tags$i(class = "fa fa-sort-numeric-down", role = "presentation", 
             `aria-label` = "sort-numeric-down icon"), tr("asc", idioma))
    dsc <- tags$button(
      type = "button", class = "btn btn-default action-button",
      style = "margin-bottom: 10px;width: 70%;margin-right: 5px;",
      onclick = paste0("accion(", var, ", 'd')"),
      tags$i(class = "fa fa-sort-numeric-down-alt", role = "presentation", 
             `aria-label` = "sort-numeric-down-alt icon"), tr("dsc", idioma))
  } else {
    asc <- tags$button(
      type = "button", class = "btn btn-default action-button",
      style = "margin-bottom: 10px;width: 70%;margin-right: 5px;",
      onclick = paste0("accion(", var, ", 'a')"),
      tags$i(class = "fa fa-sort-alpha-down", role = "presentation", 
             `aria-label` = "sort-alpha-down icon"), tr("asc", idioma))
    dsc <- tags$button(
      type = "button", class = "btn btn-default action-button",
      style = "margin-bottom: 10px;width: 70%;margin-right: 5px;",
      onclick = paste0("accion(", var, ", 'd')"),
      tags$i(class = "fa fa-sort-alpha-down-alt", role = "presentation", 
             `aria-label` = "sort-alpha-down-alt icon"), tr("dsc", idioma))
  }
  
  tags$div(tags$div(asc), tags$div(dsc))
}

selectInputGroup <- function(datos, var, idioma = "es", n, sel = 1) {
  opts <- ""
  for (i in 1:n) {
    if(i == sel) {
      opts <- paste0(opts, '<option value="', i, '" selected>', i, '</option>\n')
    } else {
      opts <- paste0(opts, '<option value="', i, '">', i, '</option>\n')
    }
  }
  
  res <- paste0(
    '<div class = "form-group shiny-input-container">\n',
    '  <select ', 
    'style = "width: 70%;margin: 5px;height: 34px;border: 1px solid #cccccc;text-align: center;line-height: 34px;border-radius: 4px;"',
    'onchange = "accion(', var, ', \'s\', this.value)">\n',
    opts,
    '  </select>\n',
    '</div>'
  )
  
  return(res)
}

prevsketch <- function(datos, tipos) {
  htmltools::withTags(table(thead(
    tr(
      th("ID"),
      lapply(1:length(colnames(datos)), function(i) {
        th(colnames(datos)[i])
      })
    ),
    tr(
      th("ID"),
      lapply(1:length(colnames(datos)), function(i) {
        res <- th(tipos[2])
        if(class(datos[[i]]) %in% c("numeric", "integer")) {
          res <- th(tipos[1])
        }
        res
      })
    )
  )))
}

sketch <- function(data.tabla, datos, originales, idioma, part, tipo.columnas) {
  rena <- tr("rena", idioma)
  tran <- tr("tran", idioma)
  orde <- tr("orde", idioma)
  elim <- tr("elim", idioma)
  vali <- tr("vali", idioma)
  labelpart <- tr("part", idioma)
  htmltools::withTags(table(thead(tr(
    th('ID'), lapply(1:length(colnames(data.tabla)), function(i) {
      if(colnames(data.tabla)[i] == part) {
        th(labelpart, class = "tablaHead",
           div(class = "dropdown-content",
               span(icon("sort-amount-down"), orde),
               btnInputArrange(data.tabla, i, idioma)
           )
        )
      } else {
        th(colnames(data.tabla)[i], class = "tablaHead",
           div(class = "dropdown-content",
               span(icon("pencil-alt"), rena),
               tags$input(
                 type = "text", class = "form-control", 
                 value = colnames(data.tabla)[i],
                 onchange = paste0("accion(", i, ", 'r', this.value)"),
                 style = "margin-bottom: 10px;width: 70%;display: initial;margin-right: 5px;"),
               hr(style = "margin: 0px;"),
               span(icon("exchange-alt"), tran), 
               selectInputTrans(data.tabla, i, idioma, originales),
               hr(style = "margin: 0px;"),
               span(icon("sort-amount-down"), orde),
               btnInputArrange(data.tabla, i, idioma),
               hr(style = "margin: 0px;"),
               span(icon("cut"), elim),
               if(colnames(data.tabla)[i] %in% colnames(datos)) {
                 tags$input(
                   type = "checkbox",
                   onchange = paste0("accion(", i, ", 'e', this.checked)"),
                   style = "margin-bottom: 10px;margin-right: 10px;transform: scale(1.5);")
               } else {
                 tags$input(
                   type = "checkbox", checked = "", 
                   onchange = paste0("accion(", i, ", 'e', this.checked)"),
                   style = "margin-bottom: 10px;margin-right: 10px;transform: scale(1.5);")
               }
           )
        )
      }})
    )),
    tags$tfoot(
      tags$tr(tags$th(), lapply(tipo.columnas, function(i)
        tags$th(shiny::HTML(i))))
    )
  ))
}

# Genera colores al azar
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

restaurar.validacion <- function(updateData) {
  updateData$numGrupos <- NULL
  updateData$numValC   <- NULL
  updateData$grupos    <- NULL
  updateData$datos.tabla[["part"]] <- NULL
}

restaurar.segmentacion <- function(updateData) {
  updateData$datos.prueba      <- NULL
  updateData$datos.aprendizaje <- NULL
  updateData$variable.predecir <- NULL
  updateData$indices           <- NULL
}

############################### Generar Código ################################
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T, incluir.NA = F) {
  res <- paste0(
    "##### doccarga #####\n",
    "datos <- fread('", ruta, "', sep = '", separador, 
    "', dec = '", sep.decimal, "', header = ", encabezado, 
    ", stringsAsFactors = T, data.table = F, check.names = T)\n")
  if(nombre.filas) {
    res <- paste0(res, "row.names(datos) <- datos[[1]]\n")
    res <- paste0(res, "datos[[1]] <- NULL\n")
  }
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

code.carga.xlsx <- function(
  ruta, sheet = 1, header = T, startRow = 0, startCol = 0, endRow = 0,
  endCol = 0, nombre.filas = T, incluir.NA = F) {
  res <- paste0(
    "##### doccarga #####\n",
    "datos <- readWorksheetFromFile('", ruta, "', sheet = '", sheet, 
    "', header = '", header, "', startRow = ", startRow, ", startCol = ", 
    startCol, ", endRow = ", endRow, ", endCol = ", endCol, ")\n")
  if(nombre.filas) {
    res <- paste0(res, "row.names(datos) <- datos[[1]]\n")
    res <- paste0(res, "datos[[1]] <- NULL\n")
  }
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

code.NA <- function(deleteNA = T) {
  res <- ifelse(
    deleteNA, "datos <- na.omit(datos)\n",
    paste0(
      "Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
      "for (var in colnames(datos)) {\n",
      "  if(any(is.na(datos[, var]))){\n",
      "    if(class(datos[, var]) %in% c('numeric', 'integer')) {\n",
      "      datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)\n",
      "    } else {\n",
      "      datos[, var][is.na(datos[, var])] <- Mode(datos[, var])\n",
      "    }\n  }\n}"))
  return(res)
}

code.trans <- function(var, nuevo.tipo) {
  if(nuevo.tipo == "categorico"){
    return(paste0(
      "datos[['", var, "']] <- as.factor(datos[['", var, "']])\n"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(
      "datos[['", var, "']] <- as.numeric(sub(',', '.', datos[['",
      var, "']], fixed = TRUE))\n"))
  } else {
    return(paste0(
      "datos <- datos.disyuntivos(datos, '", var,"')\n", 
      "datos[['", var, "']] <- NULL\n"))
  }
}

code.segment.tt <- function(variable.predecir, porcentaje = 30, semilla = 5, perm.semilla = F) {
  res <- "### doctt\n"
  
  if (perm.semilla) {
    semilla <- ifelse(is.numeric(semilla), semilla, 5)
    res <- paste0(res, "set.seed(", semilla, ")\n")
  }
  
  res <- paste0(
    res, "\n",
    "particion <- createDataPartition(y = datos[, '", variable.predecir, "'], p = ", porcentaje/100, ", list = F)\n",
    "indices <- particion[, 1]\n",
    "test  <- datos[-particion, ]\n",
    "train <- datos[particion, ]\n\n",
  )
  
  return(res)
}

code.segment.vc <- function(variable.predecir, validaciones, grupos) {
  res <- "### doccv\n"
  
  if (perm.semilla) {
    semilla <- ifelse(is.numeric(semilla), semilla, 5)
    res <- paste0(res, "set.seed(", semilla, ")\n")
  }
  
  res <- paste0(
    res, "\n",
    "particion <- createDataPartition(y = datos[, '", variable.predecir, "'], p = ", porcentaje/100, ", list = F)\n",
    "indices <- particion[, 1]\n",
    "test  <- datos[-particion, ]\n",
    "train <- datos[particion, ]\n\n",
  )
  
  return(res)
}