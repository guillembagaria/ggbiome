# `ggbiome` package

R package with functions to obtain mean annual temperature (MAT), 
mean annual precipitation (MAP) and Whittaker's biome type (Whittaker 1975) 
providing geographical coordinates, and to plot locations over the biome 
diagram.


## Installation

ggbiome is only at GitHub at the moment. You can install it using devtools package:

```r
# install devtools if necessary
install.packages('devtools')

# install ggbiome
devtools::install_github('guillembagaria/ggbiome')
```

## Remarks

Some functionalities of `ggbiome` are based on `BIOMEplot` package 
([https://github.com/kunstler/BIOMEplot](https://github.com/kunstler/BIOMEplot)).  

The biomes MAP and MAT were digitalized from the modified biomes of 
Ricklefs (2008).


Whittaker, R. H. (1975). Communities and Ecosystems. 2d ed. Macmillan New York.

Ricklefs, R. E. (2008). The economy of nature. W. H. Freeman and Company. 
Chapter 5, Biological Communities, The biome concept.
