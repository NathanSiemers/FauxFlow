# Single Cell 'Faux Flow' code base

Nathan Siemers

## Instructions:

copy Data/defaults.R.template to Data/defaults.R

customize Data/defaults.R

load_data() should return a single data frame of combined rna-seq and clinical information (genes and clinical categories in columns)

load_data should also push the data frame into the global environment, so that any future load within a shiny session happen instantly.  See the example.

Modify the other variables (site title, default choices, etc).

## This template was written such that you should be able to apply it to an existing directory without damaging the site. You can do this via:

git clone git@biogit.pri.bms.com:siemersn/Single-Cell-Template.git tmp && rsync -av tmp/ .  && rm -rf  tmp


