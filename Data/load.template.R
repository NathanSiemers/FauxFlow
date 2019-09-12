## your loader should create/return a data frame with genes and sample annotation in columns

load_data =  function( db_rds_name, create = FALSE, testing = TRUE ) {
    if (create) {
        ## create load data from scratch
        ## how you do this will depend on your files and formats  available
        ## if you harmonize to gene symbol and other identifers supplied
        ## this will involve more effort
        ## you may need to decide how to treat multiple samples with same symbol
        ## below is an example (Bejing University Single Cell Hepatocellular Carcinoma)
        library(sqldf)
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




