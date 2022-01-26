#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' dropNulls(list(1, NULL, 2))
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

########################################################

#' Returns a list of sentences with their translation in different languages.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return list
#' @export translation.readeR
#' @examples
#' translation.readeR()
#'
translation.readeR <- function() {
  ruta <- system.file("app", "lang", package = "readeR")
  load(paste0(ruta, "/translation_readeR.bin"))
  
  return(translation.readeR)
}

translation <- translation.readeR()

#' Returns a translate text (user defined).
#' 
#' @param text text to translate.
#' @param idioma language to use. For example: "en".
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return text
#' @export tr
#' @examples
#' tr("data", "en")
#'
tr <- function(text, idioma = "es") {
  
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
}

#' Returns a vector of keys to translate with tr.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return text
#' @export labels_readeR
#' @examples
#' labels_readeR()
#'
labels_readeR <- function() {
  x <- c("idio", "data", "subi", "carg", "espa", "selhead", "selrow", "selsep",
         "seldec", "selna", "selfile", "selidioma", "num", "cat", "nums", 
         "cats", "rena", "tran", "orde", "elim", "impu", "opts", "run", 
         "selcolbar", "selcolline", "selcolpoint", "alfa", "basi", "resu", 
         "norm", "disp", "dist", "corr", "pnorm", "asim", "tasim", "pvalue",
         "sigue", "nres", "selcolor", "acerca", "selpred", "train", "test", 
         "tt", "seed", "cros", "ngr", "nvc", "part", "info", "copyright", 
         "version", "habi", "desh", "asc", "dsc", "texf", "nhoj", "scell", 
         "ecell", "row", "col", "vali", 'q1', 'q3', 'median', 'min', 'mean',
         'max', 'ds', 'hist', 'curva', 'code')
  return(x)
}

# Función para generar diccionario.
# crear.traslation <- function() {
#   library(plyr)
#   archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#   translation.readeR <- dlply(archivo , .(key), function(s) key = as.list(s))
#   
#   save(translation.readeR, file = "translation_readeR.bin")
# }
