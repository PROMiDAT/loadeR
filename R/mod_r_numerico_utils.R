###################### Estadisticas Basicas ###################################
#' Returns HTML for numeric data summary
#'
#' @return HTML
#' @noRd
#' 
#' @examples
#' resumen.numerico(iris, "Sepal.Length")
#' 
resumen.numerico <- function(data, variable, idioma = "es") {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", tr("q1", idioma)), 
      Value = format(round(quantile(data[, variable], .25), 3), scientific = FALSE)
    ),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", tr("q3", idioma)),
      Value = format(round(quantile(data[, variable], .75), 3), scientific = FALSE)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", tr("median", idioma)),
      Value = format(round(median(data[, variable]), 3), scientific = FALSE)),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", tr("min", idioma)),
      Value = format(round(min(data[, variable]), 3), scientific = FALSE)),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", tr("mean", idioma)),
      Value = format(round(mean(data[, variable]), 3), scientific = FALSE)),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", tr("max", idioma)),
      Value = format(round(max(data[, variable]), 3), scientific = FALSE)),
    DS = list(
      id = "ds", Label = tags$span(`data-id`="ds", tr("ds", idioma)), 
      Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3)
    )
  )
  return(datos.numericos)
}

#' Returns HTML for category data summary
#'
#' @return HTML
#' @noRd
#' 
#' @examples
#' resumen.categorico(iris, "Sepal.Length")
#' 
resumen.categorico <- function(data, variable){
  datos.categoricos <- levels(data[, variable])
  res <- lapply(datos.categoricos, function(i) {
    list(Label = i,
         Value = summary(data[, variable])[i])
  })
  return(res)
}