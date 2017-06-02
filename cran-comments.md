## Test environments
* local Ubuntu 16.04 install, R 3.3.2
* ubuntu 14.04.5 (on travis-ci), R 3.3.2
* rhub checks on:
    * Ubuntu Linux 16.04 LTS, R-devel, GCC - error due to rhub deps:
    `Packages required but not available: ‘rgdal’ ‘rgeos’ ‘httr’`
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel)
    `0 errors 0 warnings 0 notes`
    
## R CMD check:

* This is a minor update with a few bug fixes and new features.

* One note on checks:

```
checking R code for possible problems ... NOTE
onewayid.data.frame: no visible global function definition for ‘n’
sum_network_links: no visible binding for global variable
  ‘stplanr_start’
sum_network_links: no visible binding for global variable ‘stplanr_end’
sum_network_links: no visible binding for global variable
  ‘stplanr_linkid’
Undefined global functions or variables:
  n stplanr_end stplanr_linkid stplanr_start
R CMD check results
```