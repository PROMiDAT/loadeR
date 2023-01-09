radioSwitch <- function(id, label = NULL, names, values = NULL, val.def = TRUE) {
  # declare dependencies
  shiny::addResourcePath(
    "radioSwitch-lib", system.file("assets", "radioSwitch", package = "loadeR"))
  
  deps <- list(
    htmltools::htmlDependency(
      "radioSwitch-lib", "0.1.0", c(href = "radioSwitch-lib"),
      script = "radioSwitch.js",
      stylesheet = "radioSwitch.css"
    )
  )
  
  if(is.null(values)) values <- c(TRUE, FALSE)
  inputTag <- tags$div(
    class = "form-group", `data-shinyjs-resettable-type`="RadioButtons",
    `data-shinyjs-resettable-value` = names[1],
    if(!is.null(label)) {
      tags$label(class = "control-label", `for` = id, `data-id` = label
      )
    },
    tags$div(
      class = "radioGroupButtons btn-group-container-sw", id = id, `data-toggle`="buttons",
      tags$div(
        class = "btn-radiogroup",
        tags$button(
          class = ifelse(val.def, "btn radiobtn btn-radioswitch active",
                         "btn radiobtn btn-radioswitch"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[[1]])
        )
      ),
      tags$div(
        class = "btn-radiogroup", role = "group",
        tags$button(
          class = ifelse(val.def,"btn radiobtn btn-radioswitch",
                         "btn radiobtn btn-radioswitch active"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[[2]])
        )
      )
    )
  )
  
  htmltools::attachDependencies(inputTag, deps)
}

#' Create a label that can be used to show text.
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param value Initial value.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @import htmltools
#' @export labelInput
#' @examples
#' labelInput("id", "data")
#'
labelInput <- function(inputId, value = "") {
  
  # declare dependencies
  shiny::addResourcePath(
    "labelInput-lib", system.file("assets", "labelInput", package = "loadeR"))
  
  deps <- list(
    htmltools::htmlDependency(
      "labelInput-lib", "0.1.0", c(href = "labelInput-lib"),
      script = "labelInput.js"
    )
  )
  
  inputTag <- tags$span(`data-id` = inputId, value)
  
  htmltools::attachDependencies(inputTag, deps)
}

#' Change the value of a label input on the client.
#' 
#' @param session The session object passed to function given to shinyServer. Default is getDefaultReactiveDomain().
#' @param labelid The id of the input object.
#' @param value Initial value.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return An HTML element.
#' @import shiny
#' @export updateLabelInput
#'
updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- labelid,
    ifelse(length(value) == 1, sentvalue <- list(value),
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel',
    message = list(ids = labelid, values = sentvalue))
}