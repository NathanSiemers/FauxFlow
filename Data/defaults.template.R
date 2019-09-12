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
source('Data/load.template.R')
## we load data into R memory, as we may be asked to compute
## correlations from slices of it on-the-fly
sc_env$data = load_data( sc_env$db_rds_name )

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

