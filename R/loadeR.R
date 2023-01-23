#' @name loadeR
#' @aliases loadeR
#' @docType package
#' @title Read Data System
#' @author
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>\cr
#' \itemize{
#'   \item Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#'   \item Diego Jiménez Alvarado
#'   \item Joseline Quirós Mendez
#' }
#' @description
#' Provides a framework to load text and excel files through a 'shiny' graphical interface. It allows renaming, transforming, ordering and removing variables. It includes basic exploratory methods such as the mean, median, mode, normality test, histogtram and correlation.
#' @details
#' \tabular{ll}{
#' Package: \tab loadeR\cr
#' Type: \tab Package\cr
#' Version: \tab 1.1.3\cr
#' Date: \tab 2022-01-23\cr
#' License: \tab GPL (>=2)\cr
#' }
#' @keywords package
#' @docType package

NULL
utils::globalVariables(c(
  "alpha", "gg_color_hue", "styleEqual", "fwrite", "thead", "th", "x", "n", 
  "z", "hcl", "cor", "y", "label", "value", "median", "paquete"
))
