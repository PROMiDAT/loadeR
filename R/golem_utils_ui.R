#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#' 
#' @return an HTML list
#' @noRd
#' 
#' @examples
#' list_to_li(c("a","b"))
#'
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$li
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$li
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#' 
#' @return An HTML tag
#' @noRd
#' 
#' @examples 
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' 
#' @importFrom shiny tags tagAppendAttributes tagList
#' 
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$p
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$p
    )
    res <- lapply(
      res, 
      function(x) { 
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list, 
      names(list), 
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list), 
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#' 
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#' 
#' @param tag the tag
#' 
#' @return a tag
#' @noRd
#' 
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' 
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) && 
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;", 
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) && 
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", 
      "", 
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#' 
#' @param id the id of the element to hide
#' 
#' @noRd
#' 
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#' 
#' @examples
#' with_red_star("Enter your name here")
#' 
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#' 
#' @examples
#' rep_br(5)
#' 
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' 
#' @importFrom shiny tags
enurl <- function(url, text){
  tags$a(href = url, text)
}

#' Columns wrappers
#' 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' 
#' @noRd
#' 
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_11 <- function(...){
  column(11, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_9 <- function(...){
  column(9, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_7 <- function(...){
  column(7, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}

#' @importFrom shiny column
col_5 <- function(...){
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}



#' Make the current tag behave like an action button
#' 
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  
#'  link <- a(href = "#", "My super link", style = "color: lightblue;") 
#'  
#'  ui <- fluidPage(
#'   make_action_button(link, inputId = "mylink")
#'  )
#'  
#'  server <- function(input, output, session) {
#'    observeEvent(input$mylink, {
#'     showNotification("Pouic!")
#'    })
#'  }
#'  
#'  shinyApp(ui, server)
#'  
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }
  
  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$", 
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  } 
  
  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button") 
  }
  # return tag
  tag
}


# UNCOMMENT AND USE 
# 
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#   
# To use this part of the UI
#   
#' #' Include Content From a File
#' #' 
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #' 
#' #' @rdname includeRMarkdown
#' #' @export
#' #' 
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom shiny HTML
#' includeRMarkdown <- function(path){
#'   
#'   md <- tempfile(fileext = '.md')
#'   
#'   on.exit(unlink(md),add = TRUE)
#'   
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'   
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'   
#'   Encoding(html) <- "UTF-8"
#'   
#'   return(HTML(html))
#' }

#' HTML for language menu.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export menu.idioma
#' @examples
#' menu.idioma()
#' 
menu.idioma <- function() {
  tags$li(
    class = "nodisabled treeview",
    tags$a(
      href = "#shiny-tab-tabdioma",
      tags$i(class="fa fa-language"),
      labelInput("idio"),
      tags$i(class="fa fa-angle-left pull-right")),
    tags$ul(
      class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
      radioButtons('idioma', labelInput("selidioma"),
                   choiceNames = c(tr('espa', 'es'), 'English'),
                   choiceValues = c("es", "en")),
      tags$br()))
}

#' HTML for show code on shiny application.
#' 
#' @param id The input slot that will be used to access the value.
#' @param height The height of the input, e.g. '400px', or '100vh'.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shinyAce
#' @export codigo.monokai
#' @examples
#' codigo.monokai("id", "70vh")
#' 
codigo.monokai <- function(id, height) {
  aceEditor(
    id, mode = "r", theme = "monokai", value = "", 
    readOnly = TRUE, height = height
  )
}

#' Information box.
#' 
#' @param titulo Title text.
#' @param valor The value to display in the box. Usually a number or short text.
#' @param icono An icon tag, created by icon.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export infoBoxPROMiDAT
#' @examples
#' library(shiny)
#' infoBoxPROMiDAT("Title", "Value", icon("info"))
#' 
infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(
    class = "info-box bg-promidat",
    tags$span(class = "info-box-icon", icono),
    tags$div(class="info-box-content", 
             tags$span(class = "info-box-text", titulo),
             tags$span(class = "info-box-number", valor)
    )
  )
}

#' Creates a button to use in a options menu. 
#' 
#' @param runid The input slot that will be used to access the value.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export options.run
#' @examples
#' options.run("id")
#' 
options.run <- function(runid) {
  tags$div(
    style = "display: inline-block; width: 100%", 
    shiny::h4(labelInput("opts"), 
              style = "float: left;margin-bottom: 0px;margin-top: 0px;"),
    tags$button(
      id = runid, type = "button", class = "run-button action-button", 
      icon("play"), tags$a(labelInput("run"), style = "color:white")))
}

#' Tabset panel with options menu.
#' 
#' @param ... tabPanel() elements to include in the tabset.
#' @param id If provided, you can use input$id in your server logic to determine which of the current tabs is active. The value will correspond to the value argument that is passed to tabPanel().
#' @param title Text or input to add on the opposite side of the tabs.
#' @param opciones list of html options to add on the footer of the tabset.
#' @param open Class to assign first option, for example to start open.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export tabBoxPrmdt
#' @examples
#' library(shiny)
#' tabBoxPrmdt(id = "id", title = "title", tabPanel("Tab1"))
#' 
tabBoxPrmdt <- function (..., id = NULL, title = NULL, opciones = NULL, open = NULL) {
  content <- tabsetPanel(..., id = id, selected = NULL)
  content$attribs$class <- "nav-tabs-custom"
  
  if (!is.null(title)) {
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]], tags$li(class = "header pull-right", title))
  }
  if (!is.null(opciones)) {
    pos <- length(content$children[[2]]$children) + 1
    content$children[[2]]$children[[pos]] <- opciones
  }
  if(!is.null(open)) {
    content$children[[2]]$attribs$class <- open
  }
  
  content
}

#' Options menu in footer for tabBoxPrmdt (tabsetPanel).
#' 
#' @param botones list of icons to each option of the menu. Minimum 1, maximum 5.
#' @param widths vector of widths to each option of the menu. Minimum 1, maximum 5.
#' @param heights vector of heights to each option of the menu. Minimum 1, maximum 5.
#' @param tabs.content list of UI elements to include within each menu option. Minimum 1, maximum 5.
#' @param id If provided, you can use input$id in your server logic to get the element.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export tabsOptions
#' @examples
#' tabsOptions()
#' 
tabsOptions <- function(
  botones = list(paste(labelInput("opts"), icon("gear"))), widths = 100,
  heights = 50, tabs.content = list(""), id = NULL
) {
  
  # declare dependencies
  shiny::addResourcePath(
    "tabsOptions-lib", system.file("assets", "tabsOptions", package = "loadeR"))
  
  deps <- list(
    htmltools::htmlDependency(
      "tabsOptions-lib", "0.1.0", c(href = "tabsOptions-lib"),
      script = "tabsOptions.js",
      stylesheet = "tabsOptions.css"
    )
  )
  
  res <- ""
  codeButtons <- ""
  cant <- length(botones)
  if(cant == 1) {widgets <- c("center")}
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {
    widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for (i in 1:cant) {
    res <- paste0(
      res, tags$div(
        class = paste0("box-option box-option-", widgets[i]),
        style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
        tabs.content[[i]]), "\n")
    codeButtons <- paste0(
      codeButtons, "<button style='width:", 100/cant, "%' data-widget='", 
      widgets[i], "'>", botones[[i]], "</button>\n")
  }
  res <- paste0(
    res, tags$div(
      class = "btn-options", style = "position:relative;", 
      width = "100%", HTML(codeButtons))
  )
  inputTag <- tags$div(HTML(res), id = id)
  
  return(htmltools::attachDependencies(inputTag, deps))
}