library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)

## why aren't these types of things a part of the shiny package?
source('lib/shinyConvenience/shinyConvenience.R')

## Data/defaults.R is where some content of the ui is located (titles, etc)
## Data/defaults.R should also provide or source a load_data() function to load data set
##source('Data/defaults.R')

## you can likely ignore this - only necessary when shiny server redirects to non-persistent instance
if( sc_env$cloudlink != "")  {
    cloud.html = paste0( '<a href="', cloudlink, '">', 'Bookmark', '</a><br/>' )
    cloud.html =  div(HTML(cloud.html), style = "float:right")
} else { cloud.html = "" }


shinyUI(
    fluidPage(
        ## a bit of style information
        theme = shinytheme('flatly'),
        tags$head(tags$style("h6 {font-size: 75%; }")),
        ## shortcut icon, needs to be in ./www/
        tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
        ## render
        cloud.html,
        h4(sc_env$title),
        inline(selectizeInput('posgate', 'Positive Gates', choices = NULL , multiple = TRUE)),
        inline(selectizeInput('neggate', 'Negative Gates', choices = NULL , multiple = TRUE)),
        inline(selectizeInput('genelist', 'Genes to Plot', choices = NULL , multiple = TRUE)),
        inline(selectizeInput('colorby', 'Color By', choices = NULL)),
        br(),
        inline(radioButtons('drawupper', "Upper Graphs", inline = TRUE, choices= c('Points', 'Contour', 'Both'), selected = 'Points' )),
        HTML(nbsp(5)),
        inline(radioButtons('drawlower', "Lower Graphs", inline = TRUE, choices= c('Points', 'Contour', 'Both', 'None'), selected = 'Both' )),
        br(),
        actionButton('go', "Plot"),
        br(),
        conditionalPanel (
            condition = "input.go != 0 && input.genelist != undefined && input.genelist.length > 1",
            withSpinner(proxy.height = '200px', plotOutput( "plot1", height = '1000px')),
            downloadButton('downloadplot', 'Download plot (Tiff)'),
            downloadButton('dlknitr', 'Download Report')
            ),
        withSpinner(uiOutput("filters"), proxy.height = '100px'),
        inline(sliderInput("threshold", label = h6("Gate Point (single value applied to all gates) "), min = 0, max = 12, value = 1, step = 0.1)),
        inline( sliderInput("noise", label = h6("Added Noise"), min = 0, max = 1, value = 0.15, step = 0.025) ),
        br(),
        inline(checkboxInput("maketable", "Report table of correlations of query genes to all genes in genome, given the current gating")),
        inline(checkboxInput("combogates", "Report populations of all combinations of plotted genes")),
        inline(checkboxInput("showlower", "Show lower panel", value = TRUE)),
        h6("Important: There can be a lot of zero values in these data. Some noise has been added to spread out the data and make the low values look 'flow-like'.  However, these zero values are really an undefined mixture of low readings and missing data (due to low-depth rna-seq coverage and/or challenges of single cell rna library preparation).  Keep this in mind, and treat low values (and their population statistics) with some skepticism."),
        conditionalPanel (
            condition = 'input.maketable == true',
            h3('Table of correlations and anti-correlations'),
            downloadButton('cortable', 'Download Correlations'),
            withSpinner(proxy.height = '200px',
                        tableOutput( "table1" ) )
            ),
        conditionalPanel (
            condition = 'input.combogates == true',
            h3('Table of populations for selected genes'),
            downloadButton('combotable', 'Download combo table'),
            tableOutput( "combogates" )
            ),
        h5(  paste('Contributors:', sc_env$credits)  ),
        h5( sc_env$desc ),
        h5( HTML(sc_env$methods) ),
        h5( Sys.Date() ),
        HTML(lots_of_breaks()),
        h6('Below is an area for notes, you can ignore...'),
        verbatimTextOutput('print1')
        )
    )


