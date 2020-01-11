source('scplotlib.R')
dat = readRDS('../../Data/schcc.rds')
mylist = c('FOXP3', 'CD4')
clin.cols = c()
## work
allpairs(mylist, pairdat = dat, colorby = 'ABCA1')

## work
allpairs(c('KIR2DL1', 'KIR2DL3'), colorby = 'KIR2DL4', pairdat = dat, clin.cols = clin.cols)

source(ggplot2)
mapping = aes(x = 'a', y = 'b')

## work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'a')

## not work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'b')

## not work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'c')

## works
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'a')


n = 20000
dat = data.frame(
    a = rnorm(n) + 2,
    b = rnorm(n) + 2,
    c = rnorm(n) + 2,
    d = rnorm(n) + 2,
    e = rnorm(n) + 2
    )
mylist = letters[1:4]
clin.cols = c()

