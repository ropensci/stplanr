This is a major update removing support for objects with classes based on the sp package.

sp has be superseded by sf. This change prepares for the retirement of sp and associated packages.

The package retains an onload message preparing people for this major change. I could remove it but suspect the benefits of keeping the onload message for this release, to be removed on the next release, outweigh the cons.

Happy for any comments on this.

## R CMD check results
checking R code for possible problems ... NOTE
  File ‘stplanr/R/zzz.R’:
    .onLoad calls:

0 errors ✔ | 0 warnings ✔ | 1 notes ✖

R CMD check succeeded

