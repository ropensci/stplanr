# stplanr

This is an R package from sustainable transport planning in R (stplanr).

It brings together a range of tools for transport planning practitioners and
researchers to better understand transport systems and inform policy.

The initial work on the project was funded by the Department of Transport
([DfT](https://www.gov.uk/government/organisations/department-for-transport))
as part of the National Propensity to Cycle Tool
([NPCT](http://www.cedar.iph.cam.ac.uk/research/modelling/npct-tool/)) project to
identify where bicycle paths are most urgently needed.

## Installation


```r
# you must have the devtools package (e.g. via install.packages("devtools"))
library(devtools) 
install_github("robinlovelace/stplanr")
```

## Getting help

We aim to make this package well-documented to make it easy to use.
R's internal help functions will help here:


```r
library(stplanr)
?dd_iac # get help on an stplanr function
lsf.str("package:stplanr", all = TRUE)
```

```
## age_recat : function (a)  
## age_recat2 : function (c)  
## dd_iac : function (d, a = 0.3, b = 0.2)  
## dd_logcub : function (d, par)  
## dd_logsqr : function (d, a, b, c = 0)  
## disab_recat : function (b)  
## gMapshape : function (dsn, percent)
```

