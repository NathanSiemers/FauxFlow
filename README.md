# FauxFlow: a graphics library and shiny tool for visualizing single cell data in a flow cytometry-like paradigm.

Nathan Siemers

![alt text](https://github.com/NathanSiemers/FauxFlow/blob/master/image.example.png) 

## How to use Fauxflow

### library dependencies:

*tidyverse, ggthemes, GGally, lazyeval, shiny, shinyjs, shinythemes, shinycssloaders, viridis, MASS, dplyr, rlang*

The *data.table* package is also used during the process of data set construction.


### out of the box: Zheng HCC data

The tool comes with the Zheng et al. single cell hepatocellular carcinoma T cell dataset available.  http://dx.doi.org/10.1016/j.cell.2017.05.035

If you have a shiny server available, the tool should be ready for use with the Zheng data. If you don't have a shiny server, you can run an instance of the tool as follows:

*R -e 'shiny::runApp("./", port=8888, host="0.0.0.0")'*

### adding your own data sets

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

You should be able to pull updates from the git repository without harming things, as long as you have made a custom Data/defaults.R file. The only thing that needs changing after a pull is setting TESTING = FALSE in globals.R, so that your custom data is used.















