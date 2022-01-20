#' Filter numeric variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export var.numericas
#' @examples
#' var.numericas(iris)
#' 
var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

#' Filter category variables of a data.frame
#'
#' @param data a data.frame object.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
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
#' @return data.frame
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

# Segmenta los datos
segmentar.datos <- function(datos, variable.predecir, porcentaje = 30, semilla = 5, perm.semilla = F) {
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  
  if (perm.semilla) {
    set.seed(semilla)
  } else {
    rm(.Random.seed, envir = globalenv())
  }
  particion      <- createDataPartition(y = datos[, variable.predecir], p = porcentaje/100, list = F)
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
