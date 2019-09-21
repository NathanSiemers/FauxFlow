##TESTING = TRUE
##if(TESTING) {

if( file.exists('Data/defaults.R') ) {
   source('Data/defaults.R')
} else {
    source('Data/defaults.template.R')
}

