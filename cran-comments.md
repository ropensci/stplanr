## Test environments
* local OS X install, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.2
* local windows 7 install

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking Rd line widths ... NOTE
Rd file 'calc_catchment.Rd':
  \usage lines wider than 90 characters:
       projection = "+proj=aea +lat_1=90 +lat_2=-18.416667 +lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no ... [TRUNCATED]
       
* checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘sp’
