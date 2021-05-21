list_page <- function(pagelist) {
  output <- lapply(pagelist, 
                   function(x) {
                     if (is.null(x$type)) {return(NULL)}
                     if (x$type == "select") {
                       return(selectInput(x$name, 
                                          label = x$label,
                                          selected = x$selected,
                                          choices = x$choices))
                     }
                     if (x$type == "selectize" & !is.null(x$multiple)) {
                       return(selectizeInput(x$name, 
                                          label = x$label,
                                          choices = x$choices,
                                          selected = x$selected,
                                          multiple = x$multiple,
                                          options = x$options))
                     }
                     if (x$type == "selectize" & is.null(x$multiple)) {
                       return(selectizeInput(x$name, 
                                             label = x$label,
                                             choices = x$choices,
                                             selected = x$selected,
                                             options = x$options))
                     }
                     if (x$type == "text") {
                       return(textInput(x$name, 
                                        label = x$label, value = x$value))
                     }
                     if (x$type == "numeric") {
                       return(numericInput(x$name,
                                           label = x$label,
                                           value = x$value))
                     }
                   })
}