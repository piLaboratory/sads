# sads


### R package for fitting species abundance distributions

**Paulo I. Prado, Murilo Miranda and Andre Chalom**

### Installation

```r
install.packages('sads')
library(sads)
```
Or download binaries from [CRAN](https://cran.r-project.org/package=sads).

***Developer version***

Master branch on GitHub has the release version, as available on CRAN. 

If you want to install the developer version: 

```r
library(devtools)
install_github(repo = 'piLaboratory/sads', ref= 'dev')
```

An experimental branch implementing Lindsey's (1999) correction for the log-likelihood
of continuous density distributions when used for discrete data can be installed by:

```r
library(devtools)
install_github(repo = 'piLaboratory/sads', ref= 'trueLL')
```


### More info
  - [Project page on GitHub](http://piLaboratory.github.io/sads/)
  - [Research project](http://ecologia.ib.usp.br/let/doku.php?id=engl:projects:sads) at our [Lab wiki](http://ecologia.ib.usp.br/let)

