library(shiny)
library(Matrix)
library(qlcMatrix)
##
source('lib/scplotlib/scplotlib.R')
## we have a bad dependency of scplotlib on a function below :(
##
source('lib/shinyConvenience/shinyConvenience.R')

all.choices <<- colnames(sc_env$data)

## handy test input variable
if(FALSE){

    input = list(genelist = c('FOXP3', 'CD8A'),
        threshold = 1, combogates = TRUE, posgate = NULL, neggate = NULL,
                 filtervalues = 'Tumor', clin.cols = sc_env$clin.cols  )

}
## create an informative title that will be used in plot
my.title = function(input) {
    paste(
        paste0(sc_env$title, ". Gating:"),
        ifelse( is.null(input$posgate),
               "",
               paste( paste( input$posgate, '+', sep = '')  , collapse = " ") ),
        ifelse( is.null(input$neggate),
               "",
               paste( paste( input$neggate, '-', sep = '' ), collapse = " ") ),
        ifelse( input$colorby == "", "",
               paste('Color:', input$colorby) ) ,
        '\n\n'
        )
}

## we'll demand that output functions take input as function argument
## with as few other arguments as possible
## these functions will do shiny sanity checks, etc
## then we can unwrap to send to a real plotting function
## goal - underlying functions don't need to know about shiny

typeofmatrix = function(x) {
    if( class(x) == 'dgTMatrix' | class(x) == 'dgCMatrix'  )  {
        return('sparse')
    } else {
        return('normal')
    }
}
    

fun_plot1 = function (input, plot1.data =  sc_env$data, default.filterby = sc_env$default.filterby, clin.cols = sc_env$clin.cols ) {
    if ( length( c(input$genelist, input$colorby) )  < 2  ) return ( NULL )
################################################################
    ## get rid of sparse matrix immediately
    if( typeofmatrix(plot1.data)  == 'sparse' ) {
        print("Sparse matrix detected")
        to_get = c( input$genelist, input$posgate, input$neggate, input$colorby, input$default.filterby )
        print(dim(plot1.data))
        plot1.data = as.data.frame( as.matrix( plot1.data[ , colnames(plot1.data) %in% to_get ] ), stringsAsFactors = FALSE )
        plot1.data = plot1.data + 0
        plot1.data = log2(plot1.data + 1)
        print(dim(plot1.data))
        print(head(plot1.data))
    }
################################################################    
    ## filter rows
    if( !is.null(default.filterby) ) {
        print(dim(plot1.data))
        plot1.data = plot1.data [ plot1.data[ , default.filterby] %in% input$filtervalues, ]
        print(dim(plot1.data))
    }
    ## perform positive and negative gating
    for (i in input$posgate) {
        plot1.data = posgate(plot1.data, i, input$threshold)
    }
    for (j in input$neggate) {
        plot1.data = neggate(plot1.data, j,  input$threshold)
    }
    print(dim(plot1.data))
    ## this is the real plotting function
    ##    allpairs(input$genelist, pairdat = plot1.data, colorby = input$colorby, title = my.title(input), threshold = input$threshold, noise = input$noise)
    ## prepare argument list to do.call()
    if(! is.list(input) ) {
        arguments = reactiveValuesToList(input)
    } else {
        arguments = input
    }
    ## see if we to massage to change "" to NULLs
    arguments = sapply(arguments, chblnl)
    arguments$pairdat = plot1.data
    arguments$title = my.title(input)
    arguments$clin.cols = clin.cols
    do.call( allpairs, arguments )
}


bestcor = function (gene, my.dat, n = 10) {
    if( typeofmatrix(my.dat) == 'sparse' ) {
        best.cor = as.data.frame(t( cosSparse( my.dat[ , gene, drop = FALSE], my.dat)))
    } else {
        ind = sapply(my.dat, is.numeric)
        my.dat = my.dat[, ind]
        best.cor = as.data.frame(t(cor( my.dat[ , gene], my.dat)))
    }
    colnames(best.cor) = c('Correlation')
    best.cor = best.cor[order( best.cor$Correlation, decreasing = TRUE ), , drop=FALSE]
    best.cor = best.cor[! is.na(best.cor$Correlation), , drop = FALSE]
    best.cor = rbind( best.cor[2:n, , drop=FALSE], best.cor[( nrow(best.cor) - n):nrow(best.cor), , drop=FALSE] )
    best.cor
}

fun_table1 = function (input, plot1.data = sc_env$data, n = 10) {
    if ( length( input$genelist)  < 2 ) return ( NULL )
    if(input$maketable) {
##        plot1.data = plot1.data [ plot1.data[ , sc_env$default.filterby] %in% input$filtervalues, ]
        ## for (i in input$posgate) {
        ##     plot1.data = posgate(plot1.data, i)
        ## }
        ## for (i in input$neggate) {
        ##     plot1.data = neggate(plot1.data, i)
        ## }
        plyr::ldply(input$genelist, function(i) {
            out = bestcor(i, plot1.data, n = n)
            out$gene = i
            out$Hit = rownames(out)
            data.frame(Query = i, Hit = rownames(out), Cor = out[, 1])
        })
    }
}

## fun_table1 = function (input, plot1.data = sc_env$data) {
##     if ( length( input$genelist)  < 2 ) return ( NULL )
##     if(input$maketable) {
##         plot1.data = plot1.data [ plot1.data[ , sc_env$default.filterby] %in% input$filtervalues, ]
##         for (i in input$posgate) {
##             plot1.data = posgate(plot1.data, i)
##         }
##         for (i in input$neggate) {
##             plot1.data = neggate(plot1.data, i)
##         }
##         plyr::ldply(input$genelist, function(i) {
##             out = bestcor(i, plot1.data)
##             out$gene = i
##             out$Hit = rownames(out)
##             data.frame(Query = i, Hit = rownames(out), Cor = out[, 1])
##         })
##     }
## }

combogates = function( input, pairdat = sc_env$data, ...  ) {
    genelist = input$genelist
    threshold = input$threshold
    if ( length( genelist )  < 2  ) return ( NULL )
    if ( input$combogates ) {
        datcopy = pairdat[ , colnames(pairdat) %in%
            c(input$genelist, input$posgate, input$neggate, input$colorby, sc_env$default.filterby) ]
        print(colnames(datcopy))
        ## filter rows (really)?
        datcopy = datcopy [ datcopy[ , sc_env$default.filterby] %in% input$filtervalues, ]
        for (i in input$posgate) {
            datcopy = posgate(datcopy, i)
        }
        for (i in input$neggate) {
            datcopy = neggate(datcopy, i)
        }
        ##       d = datcopy[ , colnames(datcopy) %in% c( input$colorby, sc_env$default.filterby, genelist ) ]
               d = datcopy[ , colnames(datcopy) %in% c( input$colorby, genelist ) ]
        ind = sapply(d, is.numeric)
        d[, ind] = as.data.frame(apply( d[, ind], 2, function(x) ifelse( x > threshold, 1, 0) ) )
        ind = sapply(d, is.numeric)
        d[ , ind] = apply( d[ , ind], 2, as.integer )
        out = d %>%
            group_by_all %>%
                tally %>%
                    ungroup
        if(!is.null(chblnl(input$colorby) )) {
            out = out %>%
                group_by(!!as.symbol(input$colorby)  )
        }
        out = out %>%
            mutate(pct = round(100 *  n / sum(n), digits = 1) )
        if(!is.null(chblnl(input$colorby))) {
            out = out %>% 
                arrange(desc(!!as.symbol(input$colorby) ), desc(pct) )
        } else {
            out = out %>% 
                arrange( desc(pct) )
        }
        out
    } else {
        return(NULL)
    }
}


shinyServer (
    function(input, output, session) {
        withProgress(message = 'Loading, be patient...', {
            updateSelectizeInput(session, 'posgate',  choices = all.choices,
                                 selected = sc_env$default.posgate,
                                 server = TRUE)
            updateSelectizeInput(session, 'neggate',  choices = all.choices,
                                 selected = sc_env$default.neggate,
                                 server = TRUE)
            updateSelectizeInput(session, 'genelist',  choices = all.choices,
                                 selected = sc_env$default.genelist,
                                 server = TRUE)
            updateSelectizeInput(session, 'colorby',  choices = all.choices,
                                 selected = sc_env$default.colorby,
                                 server = TRUE)
            ## UI rendering, can make this more programmatic/functional in the future
            incProgress(message = 'filters')
            output$filters =
                renderUI ( {
                    if (!is.null(sc_env$default.filterby) ) {
                        checkboxGroupInput("filtervalues", sc_env$default.filterby,
                                       choices = sort( unique(sc_env$data[ , sc_env$default.filterby]) ),
                                       selected = unique(sc_env$data[ , sc_env$default.filterby]),
                                       inline = TRUE
                                           )
                    }
                } )
            fun_docx = function(my.input = input) {
                paste0( 'fauxflow.docx' )
            }
            output$dlknitr = downloadHandler(
                filename = fun_docx(),
                content = function(file) {
                    src <- normalizePath('knit.Rmd')
                    src_template <- normalizePath('template.docx')
                    ## temporarily switch to the temp dir, in case you do not have write
                    ## permission to the current working directory
                    l.input  <<-  reactiveValuesToList(input)
                    ##plist <<- plist
                    owd <- setwd(tempdir())
                    on.exit(setwd(owd))
                    file.copy(src, 'report.Rmd', overwrite = TRUE)
                    file.copy(src_template, 'template.docx', overwrite = TRUE)
                    out <- rmarkdown::render('report.Rmd',
                                             rmarkdown::word_document(reference_docx = 'template.docx') )
                    file.rename(out, file)
                })
        })

        ## works!
        ##        plot1 = eventReactive( input$go, { fun_plot1(input) } )
        ##        output$plot1 = renderPlot({ plot1() })
        ## but if the above works
        ## why not this
        ## oh jesus, note the extra () to trigger the function call :( :(

        observeEvent( input$go, {
                    output$plot1 = renderPlot({  fun_plot1(input)  }) })
##        output$plot1 = renderPlot({  eventReactive( input$go, { fun_plot1(input) } )()  })
        output$table1 =
            renderTable({  eventReactive( input$go, {
                withProgress( message = 'Computing Correlations', {
                    fun_table1(input)
                })   })()
                       })
        output$combogates =
            renderTable( { eventReactive( input$go, {
                withProgress( message = 'Computing Bins', {
                    combogates(input)
                })  })()
                       })
        output$downloadplot = downloadHandler(
            filename = function () { return("mytiff.tiff") },
            content = function(file) {
                tiff(filename = file, res = 300, width = 4000, height = 4000,  compression = 'lzw', units = 'px')
                ##print(fun_plot1(reactiveValuesToList(input)))
                print(fun_plot1(input))
                dev.off()
            })
        output$combotable = downloadHandler(
            filename = function () { return("combotable.csv") },
            content = function(file) {
                write.csv(  combogates(reactiveValuesToList(input)), file, row.names = FALSE )
            })
        output$cortable = downloadHandler(
            filename = function () { return("cortable.csv") },
            content = function(file) {
                write.csv(  fun_table1(reactiveValuesToList(input), n = 1000), file, row.names = FALSE )
            })

        ## a console generally good for debugging
        output$print1 = renderPrint({
            print(reactiveValuesToList(input) )
        })
    })




