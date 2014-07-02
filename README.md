# sads


### R package for fitting species abundance distributions

**Paulo I. Prado and Murilo Miranda**

### Installation

Work in progress, planning to publish a first version in CRAN in June 2014.

Meanwhile you can install the current working version with

```r
library(devtools)
install_github('sads', 'piklprado')
```
or from the binaries at our [lab site](http://ecologia.ib.usp.br/let/doku.php?id=engl:tutorials:rcode#species_abundance_distributions)


### Motivation
The distribution of abundances of species is one of the basic patterns of
  ecological communities. The empirical distributions of abundances (SADs)
  or their ranks (RADs) are traditionally
  modelled through probability distributions. Hence, the maximum
  likelihood method can be used to fit and compare competing models for
  SADs and RADs.

### Features
 - Classic SAD models: logseries, lognormal, brokenstick, ... 
 - Classic rank-abundance modelos: geometric, zipf, zipf-mandelbrodt, ...
 - Tools for the diagnostic and comparison of models 
 - Tools to simulate Poisson and Negative Binomial samples from abundances in communities.

### Links
[Research project](http://ecologia.ib.usp.br/let/doku.php?id=engl:projects:sads) at our [Lab wiki](http://ecologia.ib.usp.br/let)

