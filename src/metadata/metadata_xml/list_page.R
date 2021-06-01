list_page <- function(pagelist) {
  output <- lapply(pagelist, 
                   function(x) {
                     if (is.null(x$type)) {return(NULL)}
                     if (x$type == "select") {
                       return(fluidRow(column(4,selectInput(x$name, 
                                          label = x$label,
                                          selected = x$selected,
                                          choices = x$choices)),
                                       column(4, x$description))
                       )
                     }
                     if (x$type == "selectize" & !is.null(x$multiple)) {
                       return(fluidRow(column(4,selectizeInput(x$name, 
                                          label = x$label,
                                          choices = x$choices,
                                          selected = x$selected,
                                          multiple = x$multiple,
                                          options = x$options)),
                              column(4,x$description))
                       )
                     }
                     if (x$type == "selectize" & is.null(x$multiple)) {
                       return(fluidRow(column(4,selectizeInput(x$name, 
                                             label = x$label,
                                             choices = x$choices,
                                             selected = x$selected,
                                             options = x$options)),
                                       column(4,x$description))
                       )
                     }
                     if (x$type == "text") {
                       return(fluidRow(column(4,textInput(x$name, 
                                        label = x$label, value = x$value)),
                                       column(4, x$description))
                       )
                     }
                     if (x$type == "numeric") {
                       return(fluidRow(column(4,numericInput(x$name,
                                           label = x$label,
                                           value = x$value)),
                                       column(4,x$description))
                       )
                     }
                   })
}