#
# A Shiny Application that transforms input method data into fillable form


library(shiny)
library(XML)
source("create_method_list.R")
source("list_page.R")

ui <- fluidPage(

    titlePanel("Metadata Collector"),
    actionButton("parse", "Parse Instrument Files"),
    actionButton("export", "Export Method to XML"),
    textOutput("dat"),
    tabsetPanel(id = "tabs")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        for (i in names(method)) {
            appendTab(inputId = "tabs",
                    tab = tabPanel(attr(method[[i]], "title"), list_page(method[[i]])))
        }
    observeEvent(input$export, {
        values <- names(input)
        values <- values[grep("xml_", values)]
        cat <- unique(gsub("_[[:print:]]*", "", gsub("xml_", "", values)))
        out <- do.call(c, lapply(cat, function(y) 
            c(paste("\t<", y, ">", sep = ""),
              do.call(c, lapply(values[grep(y,values)], function(x) {
                paste(paste("\t\t<", gsub(paste("xml_", y, "_", sep = ""), "", x), ">", sep = ""), input[[x]], paste("</", gsub(paste("xml_", y, "_", sep = ""), "", x), ">\n", sep = ""), collapse = "")
                })),
              paste("\t</", y, ">", sep = ""))
        ))
        out <- c("<method_xml>", "<method>", out, "</method>", "</method_xml>")
        writeLines(out, "output.xml", sep = "\n")
        output$dat <- renderText("File written to output.xml")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
