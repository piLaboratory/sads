# sads


### An R package for fitting species abundance distributions

**Paulo I. Prado and Murilo Miranda**

### Instalation

Work in progress, planning to publish a first version in CRAN in May 2014.

Meanwhile you can install the current working version with

```r
library(devtools)
install_github('sads', 'piklprado')
```
or from the binaries at our [lab site](http://ecologia.ib.usp.br/let/doku.php?id=engl:tutorials:rcode#species_abundance_distributions)


### Motivation
Species abundance distributions (SADs) are one of the basic patterns of ecological communities. The empirical distributions are traditionally modelled through probability distributions. Hence, the maximum likelihood method can be used to fit and compare competing models for SADs. A proper model for SADs is hierarchical: it should take into account not only the abundances in the community but also the sampling process. This can be done by mixing two probability distributions, which results in a new distribution. 

### Features
 - Classic SAD models: logseries, lognormal, brokenstick, ... 
 - Classic rank-abundance modelos: geometric, zipf, zipf-mandelbrodt, ...
 - Tools for the diagnostic and comparison of models 
 - Tools to simulate Poisson and Negative Binomial samples from abundances in communities.

### Links
[Research project](http://ecologia.ib.usp.br/let/doku.php?id=engl:projects:sads) at our [Lab wiki](http://ecologia.ib.usp.br/let)

