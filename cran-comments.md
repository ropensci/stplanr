## Test environments
* local Ubuntu 16.04 install, R 3.3.2
* ubuntu 14.04.5 (on travis-ci), R 3.3.2
* rhub checks on:
    * Ubuntu Linux 16.04 LTS, R-devel, GCC - error due to rhub deps:
    `Packages required but not available: ‘rgdal’ ‘rgeos’ ‘httr’`
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel)
    `0 errors 0 warnings 0 notes`

## R CMD check results

`0 errors | 0 warnings | 1 notes`

```
Days since last update: 2
```

* This is a minor patch aimed at updating the vignettes
* It also demotes **leaflet** from an import to a suggest, reducing installation time