This version passes tests with the development version of dplyr and fixes an issue with html links.

It also fixes url issues identified thanks to the winbuild service.

I fixed the missing URL reported here: https://win-builder.r-project.org/incoming_pretest/stplanr_0.6.0_20200502_190129/Debian/00check.log

However I think the other check issues are not issues with the package: webshot not being installed on Solaris is not an issue that is in my sphere of influence. Happy to resubmit if there are further issues.

## Test environments

* Linux
  - Local (Ubuntu 18.04) 
  - GH Actions: https://github.com/ropensci/stplanr/actions

* Windows
  - Winbuild: https://win-builder.r-project.org/ppp25v7y6SH5/00check.log
  - Windows: https://github.com/ropensci/stplanr/actions 

## R CMD check:

── R CMD check results ───────────────────────────────── stplanr 0.5.1.9000 ────
Duration: 1m 29.3s

❯ checking installed package size ... NOTE
    installed size is  7.2Mb
    sub-directories of 1Mb or more:
      doc    3.0Mb
      libs   2.1Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded