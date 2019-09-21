# FauxFlow: a graphics library and shiny tool for visualizing single cell data in a flow cytometry-like paradigm.

Nathan Siemers

![alt text](https://github.com/NathanSiemers/FauxFlow/blob/master/image.example.png)
(data from Villani et al. DOI: 10.1126/science.aah4573)

## How to use Fauxflow

### testing the application

I have serveral data sets available for interrogation if you want to quickly test out functionality. 

http://shiny.fiveprime.org/SCHCC (Zheng et al. Liver cancer T cells)
http://shiny.fiveprime.org/SCMel (Tirosh et al. Melanoma)
http://shiny.fiveprime.org/SCBlood (Villani et al. Blood Myeloid)

## Installing FauxFlow 

### library dependencies:

*tidyverse, ggthemes, GGally, lazyeval, shiny, shinyjs, shinythemes, shinycssloaders, viridis, MASS, dplyr, rlang*

*rmarkdown is needed to produce knitted reports (MS Word format), and you also need a modern release of pandoc*

The *data.table* package is also used during the process of data set construction.

### Clone this repository

### out of the box: Zheng HCC data

The tool comes with the Zheng et al. single cell hepatocellular carcinoma T cell dataset available.  http://dx.doi.org/10.1016/j.cell.2017.05.035

If you have a shiny server available, the tool should be ready for use with the Zheng data. If you don't have a shiny server, you can run an instance of the tool as follows:

*R -e 'shiny::runApp("./", port=8888, host="0.0.0.0")'*

### Customization: adding your own data sets

copy Data/defaults.R.template to Data/defaults.R

edit Data/defaults.R:

* Fill in appropriate background information about the data set.
* Make a list of column names that are clinical/sample annotations (sc_env$clin.cols), these columns will be treated differently in some cases (gaussian noise is added to numeric data, but not on the columns specified here).
* write a loader (see the load_data() function)

The loader should return a single data frame of combined rna-seq and clinical information (genes and clinical categories in columns).  There's almost always some tweaking to do.

* Finally: set TESTING to FALSE in global.R, this will source your new Data/defaults.R instead of defaults.template.R



## lib/scplotlib/scplotlib.R

The graphics engine for FauxFlow is contained here in the *allpairs()* and underlying *gggpairs()* functions.  The engine uses GGally::ggpairs extensively, but with heavy customization of underlying plotting functions that get called for each subplot.

## Code updates

You should be able to update FauxFlow from the git repository without harming things, as long as you have made a custom Data/defaults.R file. The only thing that needs editing after an update is setting TESTING = FALSE in globals.R, so that your custom data is used.

## Limitations, things to improve

* Gating is extremely crude - you get only a single gate point for all markers you gate with, both positive and negative.

* If you haven't worked with single cell RNA data before, you need to realize that there can be significant drop-outs in the data.  Gating (especially negative gating) and fractional proportions can be biased by this.


















