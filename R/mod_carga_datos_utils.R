#' Filter numeric variables of a data.frame.
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object only with its numeric variables.
#' @export var.numericas
#' @examples
#' var.numericas(iris)
#' 
var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

#' Filter category variables of a data.frame.
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object only with its categoric variables.
#' @export var.categoricas
#' @examples
#' var.categoricas(iris)
#' 
var.categoricas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
}

#' Create disjunctive columns to a data.frame.
#'
#' @param data a data.frame object.
#' @param var the column name to apply disjunctive code.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object after apply disjunctive code.
#' @export datos.disyuntivos
#' @examples
#' datos.disyuntivos(iris, "Species")
#' 
datos.disyuntivos <- function(data, var) {
  if(is.null(data)) {
    return(NULL)
  }
  
  for (categoria in unique(data[, var])) {
    nueva.var <- as.numeric(data[, var] == categoria)
    data[, paste0(var, '.', categoria)] <- nueva.var
  }
  
  return(data)
}

#' Back disjunctive column to original.
#'
#' @param data a data.frame object.
#' @param var the column name that is disyunctive.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object before apply disjunctive code.
#' @export devolver.disyuntivos
#' @examples
#' r <- datos.disyuntivos(iris, "Species")
#' devolver.disyuntivos(r, "Species")
#' 
devolver.disyuntivos <- function(data, var) {
  if(is.null(data)) {
    return(NULL)
  }
  
  vars <- colnames(data)[grepl(paste0(var, "."), colnames(data), fixed = TRUE)]
  valores <- rep(NA, nrow(data))
  
  for (x in vars) {
    cat <- paste0(gsub("\\.", "\\\\.", var), ".")
    cat <- gsub(cat, "", x)
    valores[which(data[[x]] == 1)] <- cat
    data[[x]] <- NULL
  }
  
  data[[var]] <- valores
  
  return(data)
}

#' Load data from text file.
#'
#' @param nombre.filas a logical value indicating whether the file contains the names of the rows as its first column.
#' @param ruta the name of the file which the data are to be read from.
#' @param separador the field separator character.
#' @param sep.decimal the character used in the file for decimal points.
#' @param encabezado a logical value indicating whether the file contains the names of the variables as its first line.
#' @param deleteNA a logical value indicating if rows with NA should be removed.
#' @param preview a logical value indicating if only load the first 10 rows.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object with the information of a file.
#' @export carga.datos
#' @examples
#' tf <- tempfile()
#' write.table(iris, tf, sep = ";", dec = ",", row.names = FALSE)
#' carga.datos(ruta = tf, nombre.filas = FALSE, preview = TRUE)
#' 
carga.datos <- function(
  nombre.filas = TRUE, ruta = NULL, separador = ";", sep.decimal = ",", 
  encabezado = TRUE, deleteNA = TRUE, preview = FALSE) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = TRUE)
  }
  
  if(preview) {
    res <- fread(
      ruta, sep = separador, dec = sep.decimal, header = encabezado, 
      stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE, nrows = 10)
  } else {
    res <- fread(
      ruta, sep = separador, dec = sep.decimal, header = encabezado, 
      stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)
  }
  
  if(nombre.filas) {
    if(any(duplicated(res[[1]]))) {
      stop("No se permiten duplicados en los nombres de fila.", call. = F)
    } else {
      row.names(res) <- res[[1]]
      res[[1]] <- NULL
    }
  }
  return(accion.NAs(res, deleteNA))
}

#' Load data from excel.
#'
#' @param ruta the name of the file which the data are to be read from.
#' @param sheet The name or index of the worksheet to read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param startRow The index of the first row to read from. Defaults to 0 meaning that the start row is determined automatically.
#' @param startCol The index of the first column to read from. Defaults to 0 meaning that the start column is determined automatically.
#' @param endRow The index of the last row to read from. Defaults to 0 meaning that the end row is determined automatically.
#' @param endCol The index of the last column to read from. Defaults to 0 meaning that the end column is determined automatically.
#' @param row_names a logical value indicating whether the file contains the names of the rows as its first column.
#' @param deleteNA a logical value indicating if rows with NA should be removed.
#' @param preview a logical value indicating if only load the first 10 rows.
#'
#' @importFrom readxl read_excel cell_limits
#' @importFrom writexl write_xlsx
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return A data.frame object with the information of a file on excel.
#' @export carga.datos.excel
#' @examples
#' \donttest{
#'   tf <- tempfile()
#'   writexl::write_xlsx(iris, paste0(tf, ".xlsx"), TRUE)
#'   carga.datos.excel(ruta = paste0(tf, ".xlsx"), row_names = FALSE, preview = TRUE)
#' }
#' 
carga.datos.excel <- function(
  ruta, sheet = 1, header = TRUE, startRow = 0, startCol = 0, endRow = 0,
  endCol = 0, row_names = TRUE, deleteNA = TRUE, preview = FALSE) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = TRUE)
  }
  
  if(preview) {
    diferencia <- endRow - startRow
    if(diferencia < 10 & diferencia > 0) {
      endRow <- endRow
    } else {
      endRow <- startRow + 10
    }
  }
  
  startRow <- ifelse(startRow == 0, NA, startRow)
  startCol <- ifelse(startCol == 0, NA, startCol)
  endRow   <- ifelse(endRow == 0, NA, endRow)
  endCol   <- ifelse(endCol == 0, NA, endCol)
  
  s   <- cell_limits(c(startRow, startCol), c(endRow, endCol))
  res <- read_excel(ruta, sheet = sheet, col_names = header, range = s)
  res <- data.frame(unclass(res), stringsAsFactors = TRUE)
  
  if(row_names) {
    row.names(res) <- res[[1]]
    res[[1]] <- NULL
  }
  return(accion.NAs(res, deleteNA))
}

# Segmenta los datos
segmentar.datos <- function(datos, variable.predecir, porcentaje = 30, semilla = 5, perm.semilla = FALSE) {
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  
  if (perm.semilla) {
    set.seed(semilla)
  } else {
    set.seed(NULL)
  }
  particion      <- createDataPartition(y = datos[, variable.predecir], p = porcentaje/100, list = FALSE)
  indices        <- particion[, 1]
  test  <- datos[-particion, ]
  train <- datos[particion, ]
  
  return(list(test = test, train = train, indices = indices))
}

# Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res) {
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}
