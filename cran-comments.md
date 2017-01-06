## Test environments
* local Ubuntu 16.04 install, R 3.3.2
* ubuntu 14.04.5 (on travis-ci), R 3.3.2
* rhub checks on:
    * Ubuntu Linux 16.04 LTS, R-devel, GCC - error due to rhub deps:
    `Packages required but not available: ‘rgdal’ ‘rgeos’ ‘httr’`
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel)
    `0 errors 0 warnings 0 notes`
    
## R CMD check:

1 Note:

Maintainer: ‘Robin Lovelace <rob00x@gmail.com>’

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Robin Lovelace and Richard Ellison
  

* This is a minor patch aimed at fixing this issue (due to splitting of tmap into 2 packages):

```
Version: 0.1.7-2
Check: re-building of vignette outputs
Result: WARN
    Error in re-building vignettes:
     ...
    Quitting from lines 108-112 (introducing-stplanr.Rmd)
    Error: processing vignette ‘introducing-stplanr.Rmd’ failed with diagnostics:
    could not find function "read_osm"
    Execution halted
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-windows-ix86+x86_64, r-patched-linux-x86_64, r-release-linux-x86_64, r-release-windows-ix86+x86_64
```