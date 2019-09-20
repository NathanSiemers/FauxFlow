
inline = function (x) {  shiny::tags$div(style="display:inline-block;", x)  }

lots_of_breaks = function() {
    paste(
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),
        shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$br()  ) 
}

nbsp = function(n) {
    paste( rep( '&nbsp;', n ), collapse = ' ')
}

## clean up shiny return values...
## returns NULL if blank or NULL
## if you change the order of tests this doesn't work.
## I hate R.  I forget this sometimes
chblnl = function(x) {
    if( length(x) == 1 ) {
        if ( is.null(x) ) return(NULL)
        if( is.na(x) )    return(NA)
        if (x == "")      return(NULL)
    }
    return(x)
}



