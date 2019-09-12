library(viridis)
##library(MASS)  will call directly ::
## otherwise clobbers dplyr select()
library(dplyr)
library(ggplot2); library(ggthemes); library(GGally)
theme_set(theme_gdocs() + theme(text=element_text(size=16,  family="Arial") ) )
scale_colour_discrete  = scale_color_discrete = scale_color_viridis(option = 'plasma', discrete = TRUE)
scale_colour_continuous = scale_color_continuous = scale_color_viridis(option = 'plasma')
scale_fill_discrete = scale_fill_viridis(option = 'plasma', discrete = TRUE)
scale_fill_continuous = scale_fill_viridis(option = 'plasma')

default_point_size = 2.5


allpairs = function(genelist, pairdat, showlower = TRUE,
    maketable = FALSE, noise = 0.15,
    posgate = NULL, neggate = NULL, threshold = 1, filterby = NULL, colorby = NULL, 
    drawupper = 'Points', drawlower = 'Contour', monochrome = FALSE, title = NULL,
    clin.cols = NULL, ...
                    ) {
    ## allpairs shouldn't need to clean up messy shiny data - removed
    ## also removed dependency on other sourced library
    ## colorby = chblnl(colorby); filterby = chblnl(filterby)
    if(drawlower == 'None') showlower = FALSE
    upper_plotter = case_when(
        drawupper == 'Points' & is.null(  colorby  ) ~ 'points_density_fn',
        drawupper == 'Points' & ! is.null(  colorby ) ~ 'points_fn',
        drawupper == 'Contour' ~ 'contour_fn',
        drawupper == 'Both' ~ 'points_and_contour_fn',
        TRUE ~ 'points_density_fn'
        )
    lower_plotter = case_when(
        drawlower == 'Points' & is.null( colorby ) ~ 'points_density_fn',
        drawlower == 'Points' & ! is.null( colorby ) ~ 'points_fn',
        drawlower == 'Contour' ~ 'contour_fn',
        drawlower == 'Both' ~ 'points_and_contour_fn',
        TRUE ~ 'contour_fn'
        )
    if( !is.null(filterby) ) {
        pairdat = pairdat [ pairdat[ , default.filterby] %in% filterby, ]
    }
    for (i in posgate) {
        pairdat = posgate(pairdat, i, threshold)
    }
    for (j in neggate) {
        pairdat = neggate(pairdat, j,  threshold)
    }
    genefetchlist = unique( c(genelist, colorby) )
    genefetchlist = genefetchlist[ ! genefetchlist == "" ]
    pairdat = pairdat[ , genefetchlist]
    ## adjust size now that we know true dimensions
    size_adjust = 1 / ( ( ncol(pairdat) ** 0.5 ) * ( log10(nrow(pairdat)) ** 0.5 ) )
    ##
    pairdat  = add_noise(pairdat, noise)
    colnames(pairdat) = gsub('-', '.', colnames(pairdat))
    gggpairs(dat = pairdat, colorby = colorby,
             ##mapping = aes_string(color = colorby, fill = colorby),
             showlower = showlower, threshold = threshold,
             upper_plotter = upper_plotter, lower_plotter = lower_plotter,
             monochrome = monochrome, size_adjust = size_adjust)
}

gggpairs = function(dat, colorby = NULL, showlower = TRUE, 
    title = "", threshold = 1, monochrome = FALSE, size_adjust = 1,
    lower_plotter = 'points_and_contour_fn', upper_plotter = 'points_fn') {
    lower_plotter = get(lower_plotter)
    lower_plotter = wrap(lower_plotter, size_adjust = size_adjust, colorby = colorby, threshold = threshold)
    ##print(lower_plotter)
    upper_plotter = get(upper_plotter)
    upper_plotter = wrap(upper_plotter, size_adjust = size_adjust, colorby = colorby, threshold = threshold )
    ##print(upper_plotter)
    facethist_plotter = wrap(facethist_fn, colorby = colorby)
    box_no_facet_plotter = wrap(box_no_facet_fn, colorby = colorby)
    facetbar_plotter = wrap(facetbar_fn, colorby = colorby)
    barDiag_plotter = wrap(barDiag_fn, colorby = colorby)
    if(showlower) {
        lower.list = list(continuous = lower_plotter, combo = box_no_facet_plotter, discrete = facetbar_plotter, na = "na")
    } else {
        lower.list = list(continuous = 'blank', combo = 'blank', discrete = 'blank', na = "na")
    }
    ##upper.list = list( continuous = upper_plotter, combo = facethist_plotter, discrete = "facetbar", na = "na" )
    upper.list = list( continuous = upper_plotter, combo = facethist_plotter, discrete = facetbar_plotter, na = "na" )
    p =  ggpairs( dat, cardinality_threshold = 30,
        diag = list(continuous ="densityDiag", discrete = "barDiag", na = "naDiag"),
        lower = lower.list,
        upper = upper.list,
                 threshold = threshold)
    p
}

################################################################
## accessory functions

add_noise = function( mydat, noise, clin.cols = NULL ) {
    if(is.null(clin.cols)) {
        numeric.cols = colnames(mydat)[ sapply(mydat, is.numeric) ]
    } else {
        numeric.cols =
            colnames(mydat)[ ( ! colnames(mydat) %in% clin.cols )  &
            sapply(mydat, is.numeric) ]
    }
    if ( noise > 0.00000001 ) {
        mydat[ , numeric.cols]  =
            mydat[ , numeric.cols ] +
                rnorm( nrow(mydat) * ncol(mydat[ , numeric.cols]), 0, noise )
    }
    mydat
}

################################################################

posgate = function(gate.dat, gene, threshold = 1) {
    gate.dat[  gate.dat[ , gene] > threshold ,  ]
}

################################################################

neggate = function(gate.dat, gene, threshold = 1) {
    gate.dat[  gate.dat[ , gene] < threshold ,  ]
}

################################################################

get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
}

################################################################

add_continuous_quadrants = function(p, size_adjust = 1, threshold = 1) {
    base_size = 18
    ggpb <- ggplot_build(p)
    dat = ggpb$data[[1]]
    if(nrow(dat) == 0){return(p)}
    all.points = nrow(dat)
    maxx = ggpb$layout$panel_scales_x[[1]]$range$range[[2]]  # data range!
    maxy = ggpb$layout$panel_scales_y[[1]]$range$range[[2]]  # data range!
    ##print(paste('maxx', maxx, 'maxy', maxy))
    ##print(colnames(dat))
    a11 = round (length( which ( dat[ , 'x'] < threshold & dat [ , 'y'] < threshold ) ) / all.points  ,  digits = 3 )
    ##print(a11)
    a12 = round (length( which ( dat[ , 'y'] < threshold & dat [ , 'x'] >= threshold )) / all.points ,  digits = 3 )
    ##print(paste('a12', a12))
    a21 = round (length( which ( dat[ , 'y'] >= threshold & dat [ , 'x'] < threshold )) / all.points ,  digits = 3 )
    ##print(paste('a21', a21))
    a22 = round (length( which( dat[ , 'y'] >= threshold & dat [ , 'x'] >= threshold )) / all.points ,  digits = 3 )
    ##print(paste('a22', a22))
    p = p +
        annotate('label', x = 0.2, y = 0.2, label = a11, size = base_size * size_adjust) +
            annotate('label', x = 0.75 * maxx, y = 0.2, label = a12, size = base_size * size_adjust)  +
                annotate('label', x = 0.2, y = 0.75 * maxy, label = a21, size = base_size * size_adjust)  +
                    annotate('label', x = 0.75 * maxx, y = 0.75 * maxy, label = a22, size = base_size * size_adjust)
    p
}

################################################################

add_continuous_labels = function(p, size_adjust = 0.7 ) {
    ggpb <- ggplot_build(p)
    y.range <- ggpb$layout$panel_scales_y[[1]]$range$range  # data range!
    x.range =  ggpb$layout$panel_scales_x[[1]]$range$range  # data range!
    ##x.range = layer_scales(p[i,j])$x$range$range
    ##print(x.range)
    x.min = x.range[[1]]
    ##print(paste( 'xrange', x.range, 'yrange', y.range, collapse = ',' ))
    x.range = x.range[[2]] - x.range[[1]]
    y.range = layer_scales(p)$y$range$range
    ##print(y.range)
    y.min = y.range[[1]]
    y.range = y.range[[2]] - y.range[[1]]
    labelx = ggpb$plot$labels[[1]]
    labely = ggpb$plot$labels[[2]]
    my.label.x = paste( as.character(labelx), sprintf('\u2192')   )
    ##print(paste('label', my.label.x))
    my.label.y = paste( as.character(labely), sprintf('\u2192')   )
    ##print(paste('label', my.label.y))
    p = p +
        annotate('text', x = x.min + (x.range * 0.00)  , y = y.min + ( y.range * 0.45 ), label = my.label.y, size = 24 * size_adjust , angle = 90 ) +
            annotate('text', x = x.min + ( x.range * 0.45 )  , y = y.min + ( y.range * 0.00 ), label = my.label.x, size = 24 * size_adjust)
    p
}

add_gate_line = function(p, threshold = 1) {
    p = p + geom_vline (xintercept = threshold, color = 'black', size = 1, alpha = 0.5) +
        geom_hline (yintercept = threshold, color = 'black', size = 1, alpha = 0.5 )
    p
}


################################################################
## custom plotting functions for continuous data
################################################################

################################################################

barDiag_fn = function(data, mapping, colorby = NULL, ...) {
    if( !is.null(colorby) ) {
        mapping = modifyList(mapping, aes_string(color = colorby) )
    }
    mapping <- mapping_color_to_fill(mapping)
    mapping$y <- NULL
    x_data <- eval_data_col(data, mapping$x)
    numer <- ("continuous" == plotting_data_type(x_data))
    p <- ggplot(data = data, mapping)
    if (is_date(x_data)) {
        p <- p + geom_histogram(...)
    }
    else if (numer) {
        if (identical(rescale, TRUE)) {
            p <- p + geom_histogram(aes(y = ..density../max(..density..) * 
                diff(range(x, na.rm = TRUE)) + min(x, na.rm = TRUE)), 
                ...) + coord_cartesian(ylim = range(eval_data_col(data, 
                mapping$x), na.rm = TRUE))
        }
        else {
            p <- p + geom_histogram(...)
        }
    }
    else {
        p <- p + geom_bar(...)
    }
    p = p +
        scale_color_viridis(option = 'plasma', discrete = TRUE) +
            scale_fill_viridis(option = 'plasma', discrete = TRUE)
    p
}

################################################################

facetbar_fn = function(data, mapping, colorby = NULL, ...) {
    ##print('facetbar_fn')
    ##print(mapping)
    if( !is.null(colorby) ) {
        mapping = modifyList(mapping, aes_string(color = colorby, fill = colorby) )
    }
    ##print(mapping)
    mapping <- mapping_color_to_fill(mapping)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data, mapping) + geom_bar(...) + facet_grid(paste(yVal, 
        " ~ .", sep = ""))
    p = p +
        scale_color_viridis(option = 'plasma', discrete = TRUE) +
            scale_fill_viridis(option = 'plasma', discrete = TRUE)
    p
}


################################################################

box_no_facet_fn = function(data, mapping, colorby = NULL, ...) {
    if(   !is.null(colorby)   ) {
        coloraes = aes_string(color = colorby)
        mapping = modifyList(mapping, coloraes)
    }
    mapping <- mapping_color_to_fill(mapping)
    ##print(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (horizontal) {
    mapping <- mapping_swap_x_y(mapping)
  }
    p <- ggplot(data = data)
    if (TRUE) {
        p <- p + geom_boxplot(mapping, ...)
    } else {
        p <- p + geom_jitter(mapping, ...)
    }
    if (horizontal) {
        p <- p +
            scale_x_discrete(
                limits = rev(levels(eval_data_col(data, mapping$x)))
                ) +
                    coord_flip()
    }
    ##    p = ggally_dot_and_box(data, mapping, ..., boxPlot = TRUE)
    p = p +
        scale_color_viridis(option = 'plasma', discrete = TRUE) +
            scale_fill_viridis(option = 'plasma', discrete = TRUE)
    ##p = p + xlab(mapping$x) + ylab(mapping$y)
    ##p = p + xlab(mapping$x) 
    p
}

################################################################
##works!!
facethist_fn = function (data, mapping, colorby = NULL, ...) {
    ## nathan
    if(   !is.null(colorby)   ) {
        coloraes = aes_string(color = colorby)
        mapping = modifyList(mapping, coloraes)
    }
    ##
    mapping <- mapping_color_to_fill(mapping)
    ##print(mapping)
    mapping <- mapping_color_to_fill(mapping)
    horizontal <- is_horizontal(data, mapping)
    if (!horizontal) {
        mapping <- mapping_swap_x_y(mapping)
    }
    xVal <- mapping_string(mapping$x)
    yVal <- mapping_string(mapping$y)
    mapping$y <- NULL
    p <- ggplot(data = data, mapping)
    p <- p + stat_bin(...)
    if (horizontal) {
        p <- p + facet_grid(paste(yVal, " ~ .", sep = "")) + 
            theme(panel.spacing = unit(0.1, "lines"))
    }
    else {
        p <- p + facet_grid(paste(". ~", yVal, sep = "")) + theme(panel.spacing = unit(0.1, 
            "lines")) + coord_flip()
    }
    p <- p + labs(x = xVal, y = yVal)
    ## nathan
    p = p + scale_color_viridis(option = 'plasma', discrete = TRUE) + scale_fill_viridis(option = 'plasma', discrete = TRUE)
    p
}


################################################################
## plot contours colored by density

contour_fn  = function(data, mapping, size_adjust = 1, monochrome = FALSE, ...){
    ##print("contour_fn")
    matchcall = as.list(match.call())
    matchcall$data = NULL
    size_adjust = matchcall$size_adjust
    colorby = matchcall$colorby
    threshold = matchcall$threshold
    rownumber = nrow(data)
    p <- ggplot(data = data, mapping = mapping) +
        stat_density_2d(aes(fill = log2( stat(level) )  , alpha = ..level..  ) , bins = rownumber /55, geom = 'polygon')
    ## colors are tricky, have to see if color variable is continuous or factor
    if( monochrome) {
        p = p + scale_fill_gradient( high = '#eeeeee', low = '#222222' )
    } else {
        p = p + scale_fill_viridis(option = 'plasma')
    }
    p = add_continuous_labels(p, size_adjust = size_adjust)
    p =  add_continuous_quadrants(p, size_adjust = size_adjust, threshold = threshold)
    p = add_gate_line(p, threshold = threshold)
}

################################################################
## plot points, color by chosen color

##points_fn = function(data, mapping,  colorby = NULL, size_adjust = 1, monochrome = FALSE,  ...) {
points_fn = function(data, mapping, ...) {
    ## I don't fucking believe I have to do this
################################################################
    ## painfully extract function arguments from referenced
    ##GGally::wrap()
    matchcall = as.list(match.call())
    matchcall$data = NULL
    ##print(matchcall)
    size_adjust = matchcall$size_adjust
    colorby = matchcall$colorby
    threshold = matchcall$threshold
    ################################################################
    ##print(paste('size_adjust', size_adjust) )
    ##print(paste('colorby', colorby) )
    point_size = 12 * size_adjust
    ##print(mapping)
    p = ggplot(data = data, mapping = mapping) +
        geom_point(aes_string(fill = colorby), color = 'grey80', size = point_size, shape = 21, alpha = 0.5)
    p = add_continuous_labels(p, size_adjust = size_adjust)
    p = add_continuous_quadrants(p, size_adjust = size_adjust, threshold = threshold)
    p = add_gate_line(p, threshold = threshold)
    if(   is.factor(data[ , colorby]) ) {
        p = p + scale_fill_viridis(option = 'plasma', discrete = TRUE)
    } else {
        p = p + scale_fill_viridis(option = 'plasma', discrete = FALSE)
    }
    p
}

################################################################
## plot both points and contours, color by density

##points_and_contour_fn  = function(data, mapping, colorby = NULL, size_adjust = 1, monochrome = FALSE, point_size = default_point_size, ...) {
points_and_contour_fn  = function(data, mapping, ...) {
    ## I don't fucking believe I have to do this
################################################################
    ## painfully extract function arguments from referenced
    ##GGally::wrap()
    matchcall = as.list(match.call())
    matchcall$data = NULL
    ##print(matchcall)
    size_adjust = matchcall$size_adjust
    colorby = matchcall$colorby
    threshold = matchcall$threshold
    ################################################################
    ##print("points_and_contour_fn")
    charmap = gsub( '~', '', as.character(mapping))
    x = charmap[[1]]
    y = charmap[[2]]
    point_size = 12 * size_adjust
    data$density = get_density( data[, x ]  , data[, y ], n = 300 )
    rownumber = nrow(data)
    p = ggplot(data = data, mapping = mapping) +
        stat_density_2d(aes(fill = log2( stat(level) )  , alpha = ..level..  ) , bins = rownumber / 5, geom = 'polygon') +
            geom_point(aes(fill = log2(density) ), color = "white", alpha = 0.3, size = point_size, shape = 21, color = "black" )
    p = p + scale_fill_viridis(option = 'plasma')
    p = add_continuous_labels(p, size_adjust = size_adjust)
    p =  add_continuous_quadrants(p, size = size_adjust, threshold = threshold)
    p = add_gate_line(p, threshold = threshold )
    p
}

################################################################
## plot points by density

points_density_fn = function(data, mapping, ...){
    ## I don't fucking believe I have to do this
    ################################################################
    ## painfully extract function arguments from referenced
    ##GGally::wrap()
    matchcall = as.list(match.call())
    matchcall$data = NULL
    ##print(matchcall)
    size_adjust = matchcall$size_adjust
    colorby = matchcall$colorby
    threshold = matchcall$threshold
################################################################
    point_size = 12 * size_adjust
    ##print("points_density_fn")
    ##print(mapping)
    charmap = gsub( '~', '', as.character(mapping))
    x = charmap[[1]]
    y = charmap[[2]]
    data$density = get_density( data[, x ]  , data[, y ], n = 300 )
    p = ggplot(data = data, mapping = mapping) +
        geom_point(aes(fill = log2(density) ), color = "white", alpha = 0.3, size = point_size, shape = 21, color = "black" )
    p = p + scale_fill_viridis(option = 'plasma')
    p = add_continuous_labels(p, size = size_adjust)
    p = add_continuous_quadrants(p, size = size_adjust, threshold = threshold)
    p = add_gate_line(p, threshold = threshold)
    p
}




































################################################################
## testing only
## you will need to load the.data from server.R or somewhere else as a global

if(FALSE){

    myd = the.data %>% select(FOXP3, CD8A, Pretreat)

    mapping = aes(x = FOXP3, y = Pretreat, color = Pretreat, fill = Pretreat)
    mapping
    ggally_facethist(myd, mapping)


    yval = mapping_string(mapping$y)
    xval = mapping_string(mapping$x)
    mapping$y = NULL
    mapping
    p = ggplot( myd, mapping = mapping)
    p = p  + stat_bin()
    p =  p + facet_grid(paste(". ~", yval, sep = "")) + theme(panel.spacing = unit(0.1, "lines")) + coord_flip()
    p


    mapping = aes(x = FOXP3, y = Pretreat)
    facethist_fn(myd, mapping)
    facethist_fn(myd, mapping, colorby = 'Pretreat')

    ggally_box_no_facet(myd, mapping)
    box_no_facet_fn(myd, mapping)
    box_no_facet_fn(myd, mapping, colorby = 'Pretreat')

    ggally_facetbar(myd, mapping)
    facetbar_fn(myd, mapping)
    facetbar_fn(myd, mapping, colorby = 'Pretreat')

}
