R -e 'install.packages("lazyeval")'
R -e 'install.packages("ggplot2")'
R -e 'install.packages("GGally")'
R -e 'shiny::runApp("./", port=8888, host="0.0.0.0")'
