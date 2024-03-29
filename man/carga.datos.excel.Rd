% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_carga_datos_utils.R
\name{carga.datos.excel}
\alias{carga.datos.excel}
\title{Load data from excel.}
\usage{
carga.datos.excel(
  ruta,
  sheet = 1,
  header = TRUE,
  startRow = 0,
  startCol = 0,
  endRow = 0,
  endCol = 0,
  row_names = TRUE,
  deleteNA = TRUE,
  preview = FALSE
)
}
\arguments{
\item{ruta}{the name of the file which the data are to be read from.}

\item{sheet}{The name or index of the worksheet to read from.}

\item{header}{a logical value indicating whether the file contains the names of the variables as its first line.}

\item{startRow}{The index of the first row to read from. Defaults to 0 meaning that the start row is determined automatically.}

\item{startCol}{The index of the first column to read from. Defaults to 0 meaning that the start column is determined automatically.}

\item{endRow}{The index of the last row to read from. Defaults to 0 meaning that the end row is determined automatically.}

\item{endCol}{The index of the last column to read from. Defaults to 0 meaning that the end column is determined automatically.}

\item{row_names}{a logical value indicating whether the file contains the names of the rows as its first column.}

\item{deleteNA}{a logical value indicating if rows with NA should be removed.}

\item{preview}{a logical value indicating if only load the first 10 rows.}
}
\value{
A data.frame object with the information of a file on excel.
}
\description{
Load data from excel.
}
\examples{
\donttest{
  tf <- tempfile()
  writexl::write_xlsx(iris, paste0(tf, ".xlsx"), TRUE)
  carga.datos.excel(ruta = paste0(tf, ".xlsx"), row_names = FALSE, preview = TRUE)
}

}
\author{
Diego Jimenez <diego.jimenez@promidat.com>
}
