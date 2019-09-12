source('scplotlib.R')

n = 100
dat = data.frame(
    a = factor(1:n),
    b = 1:n,
    c = rnorm(n) + 2,
    d = rnorm(n) + 2
    e = rnorm(n) + 2
    )
mylist = letters[1:4]
clin.cols = c('a', 'b')

## work
allpairs(mylist, pairdat = dat)

## work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols)

## work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'a')

## not work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'b')

## not work
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'c')

## works
allpairs(mylist, pairdat = dat, clin.cols = clin.cols, colorby = 'a')


