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
#' @import plyr
#' @export translation.readeR
#' @examples
#' translation.readeR()
#'
translation.readeR <- function() {
  arch <- data.frame(
    key = c('acerca', 'alfa', 'asc', 'asim', 'basi', 'carg', 'cat', 'cats',
            'col', 'copyright', 'corr', 'cros', 'data', 'desh', 'dis', 'disp',
            'dist', 'dsc', 'ecell', 'elim', 'espa', 'habi', 'idio', 'impu',
            'info', 'ngr', 'nhoj', 'norm', 'nres', 'num', 'nums', 'nvc',
            'opts', 'orde', 'part', 'pnorm', 'pvalue', 'rena', 'resu', 'row',
            'run', 'scell', 'seed', 'selcolbar', 'selcolline', 'selcolor',
            'selcolpoint', 'seldec', 'selfile', 'selhead', 'selidioma',
            'selna', 'selpred', 'selrow', 'selsep', 'sigue', 'subi', 'tasim',
            'test', 'texf', 'train', 'tran', 'tt', 'vali', 'version', 'q1',
            'q3', 'median', 'min', 'mean', 'max', 'ds', 'hist', 'curva', 
            'code'),
    es  = c('Acerca De', 'Alfa', 'Ascendente', 'Asimetría', 
            'Estadísticas Básicas', 'Cargar', 'Categórica', 'Categóricas',
            'Columna', 'TODOS LOS DERECHOS RESERVADOS A', 'Correlación',
            'Validación Cruzada', 'Datos', 'Deshabilitada', 'Disyuntiva',
            'Dispersión', 'Distribución', 'Descendente', 'Celda final',
            'Eliminar', 'Español', 'Habilitada', 'Idioma', 'Imputar',
            'MÁS INFORMACIÓN', 'Número de Grupos', 'Número de Hoja',
            'Test de Normalidad', 'Resultados Numéricos', 'Numérica',
            'Numéricas', 'Número de Validaciones', 'Opciones', 'Ordenar',
            'Partición', 'Gráfico Normalidad', 'P-valor (Shapiro)',
            'Renombrar', 'Resumen Numérico', 'Fila', 'Ejecutar',
            'Celda inicial', 'Semilla Aleatoria',
            'Seleccione el color de la barra',
            'Seleccione el color de la linea', 'Seleccionar color',
            'Seleccione el color del punto', 'Separador de Decimales',
            'Cargar archivo', 'Nombre de Variables', 'Seleccionar Idioma',
            'Acción para Datos Ausentes (NAs)',
            'Seleccionar la variable a predecir', 'Nombre de Individuos',
            'Separador de Datos', '¿Sigue la normal?', 'Subir',
            'Valor del test', 'Prueba', 'Archivo de texto', 'Aprendizaje',
            'Transformar', 'Aprendizaje - Prueba', 'Seleccionar muestra',
            'VERSIÓN DEL SISTEMA', 'Primer Cuartil', 'Tercer Cuartil', 
            'Mediana', 'Mínimo', 'Promedio', 'Máximo', 'Desviación Estandar',
            'Histograma', 'Curva Normal', 'Código'),
    en  = c('About', 'Alpha', 'Ascendant', 'Asymmetry', 'Basic Statistics',
            'Load', 'Categorical', 'Categoricals', 'Column',
            'ALL RIGHTS RESERVED TO', 'Correlation', 'Cross Validation', 
            'Data', 'Disabled', 'Disyunctive', 'Scatter Plot', 'Distribution',
            'Descendant', 'End Cell', 'Remove', 'Spanish', 'Enabled',
            'Language', 'Impute', 'MORE INFORMATION', 'Number of Groups',
            'Sheet Number', 'Normality Test', 'Numerical Results', 'Numerical',
            'Numericals', 'Number of Cross Validations', 'Options', 'Arrange',
            'Partition', 'Normal Plot', 'P-value (Shapiro)', 'Rename',
            'Numeric Summary', 'Row', 'Run', 'Start Cell', 'Random Seed',
            'Select the bar color', 'Select the line color', 'Select color',
            'Select the point color', 'Decimal Separator', 'Load archive',
            'Variables names', 'Select Language',
            'Action for Missing Data (NAs)',
            'Select the variable to be predicted', 'Row names',
            'Data Separator', 'is normally distributed?', 'Upload',
            'Test value', 'Test', 'Text file', 'Train', 'Transform',
            'Train - Test', 'Select sample', 'SYSTEM VERSION', 
            'First Quartile', 'Third Quartile', 'Median', 'Minimum', 'Mean',
            'Maximum', 'Standard deviation', 'Histogram', 'Normal Curve', 
            'Code')
  )
  
  translation.readeR <- dlply(arch , .(key), function(s) key = as.list(s))
  
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
