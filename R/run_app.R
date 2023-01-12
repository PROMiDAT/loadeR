#' Run the Shiny Application
#'
#' @param paquete indicates if the data is going to be used for exploratory, predictive, or regression analysis.
#' @param ... A series of options to be used inside the app.
#'
#' @return No return value, run a shiny application.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(paquete = "predictoR", ...) {
  old <- options()
  on.exit(options(old))
  Sys.setenv("LANGUAGE" = "ES")
  if(toupper(.Platform$OS.type) != "WINDOWS") {
    options(encoding = "utf8")
  } else {
    options(encoding = "UTF-8")
  }
  paquete <<- paquete
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = T)
    ),
    golem_opts = list(...)
  )
}
