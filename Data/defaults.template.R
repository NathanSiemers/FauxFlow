################################################################
## this file may get sourced into global env from global.R
## if TESTING is TRUE, defaults.template.R file is chosen
## otherwise a 'defaults.R' file is used
## this is to keep a new sync from clobbering old configs and loaders
## otherwise defaults.R is used
################################################################
## enter your project defaults here
## these show up in the shiny app
## I'm not convinced storing config in an environment is
## best thing to do
## But I'm doing it...

library(rlang)
sc_env = new.env()
## you must write a loader, no matter how trivial
sc_env$db_rds_name = 'schcc.rds' ## name of final rds file written
## project annotation that shows up in ui

sc_env$PI = "Nathan Siemers"
sc_env$title = "FauxFlow:  Single Cell Hepatocellular Carcinoma"
sc_env$dbname = 'schcc.rds'
sc_env$credits = "Jim Holloway, Nathan Siemers"
sc_env$desc = ""
sc_env$methods =  '
Landscape of Infiltrating T Cells in Liver Cancer Revealed by Single-Cell Sequencing
Chunhong Zheng, Liangtao Zheng, Jae-Kwang Yoo, Huahu Guo, Yuanyuan Zhang, Xinyi Guo, Boxi Kang, Ruozhen Hu, Julie Y. Huang, Qiming Zhang, Zhouzerui Liu, Minghui Dong, Xueda Hu, Wenjun Ouyang.
DOI: <a href="http://dx.doi.org/10.1016/j.cell.2017.05.035"> http://dx.doi.org/10.1016/j.cell.2017.05.035</a>
'
## if you need to offer a stable link for your app you can put it here
## some cloud and other servers redirect you to non-persistent locations
sc_env$cloudlink = ''
## which factor columns are available and suggested for searching and filtering?
## also, these named columns will not get noise added
sc_env$clin.cols = c('Sample', 'Patient', 'Cluster', 'Tissue')
## default settings for gui input
sc_env$default.posgate = c()
sc_env$default.neggate = c()
sc_env$default.genelist = c('CD3E', 'CD8A', 'CD4', 'FOXP3')
sc_env$default.colorby= ""
sc_env$default.filterby = "Tissue"
env_print(sc_env)

## your loader should create/return a data frame with genes and sample annotation in columns

load_data =  function( db_rds_name, create = FALSE, testing = TRUE ) {
    if (create) {
        ## create load data from scratch
        ## how you do this will depend on your files and formats  available
        ## if you harmonize to gene symbol and other identifers supplied
        ## this will involve more effort
        ## you may need to decide how to treat multiple samples with same symbol
        ## below is an example (Bejing University Single Cell Hepatocellular Carcinoma)
        ##library(sqldf)
        library(data.table)
        library(tidyverse)
        ## EXPRESSION MATRIX
        dat = read.csv('Data/default.GSE98638_HCC.TCell.S5063.count.txt.gz', sep = '\t', stringsAsFactors = FALSE )
        dat = dat %>% filter( !is.na(symbol) ) %>% dplyr::select(-geneID) %>%
            as.data.table
        ################################################################
        ## jump into data.table land to transpose, etc
        ## I'm no wiz with data.tables, but recommend for large operations
        dat = dat[, lapply(.SD, sum, na.rm=TRUE), by='symbol', .SDcols=sapply(dat, is.numeric) ]
        ## save symbols and samples for use later
        symbols = dat$symbol; samples = colnames(dat)[-1]
        dat = data.table::transpose(dat[,-1])
        ## transformation? normalization? filtering? do it here.
        dat = log2(dat + 1)
        ## get out of data.table land
        dat = as.data.frame(dat)
        ################################################################
        ## add row, column names
        colnames(dat) = symbols; dat$sample = samples
        ## you keep a copy expression-only data if you wish
        if(FALSE) saveRDS(dat, file='Data/data.rds')
        ## SAMPLE ANNOTATIONS (phenotypes/pheno)
        ## clean up as you wish
        pheno = read.csv('Data/default.pheno.data', header = FALSE, sep = '\t', skip = 1)
        colnames(pheno) = c('sample', 'Patient', 'Cluster', 'Tissue')
        pheno$sample = gsub('-', '.', pheno$sample, fixed = TRUE)
        pheno$Tissue = gsub('PT.*', 'Periph', pheno$Tissue)
        pheno$Tissue = gsub('JT.*', 'J Unk', pheno$Tissue)
        pheno$Tissue = gsub('TT.*', 'Tumor', pheno$Tissue)
        pheno$Tissue = gsub('NT.*', 'Adj Norm', pheno$Tissue)
        ## save a copy if desired
        if(FALSE) saveRDS ( pheno, file = 'Data/pheno.rds' )
        ## MERGE
        dat = pheno %>%
            filter(Tissue != 'J Unk') %>%
                left_join( dat, by = "sample" )
        saveRDS( dat, file.path('Data', db_rds_name ) )
    }
    dat = readRDS( file.path('Data', db_rds_name ) )
    dat
}

## we load data into R memory, as we may be asked to compute
## correlations from slices of it on-the-fly
sc_env$data = load_data( sc_env$db_rds_name )





