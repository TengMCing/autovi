## R CMD check environments
* local mac OS X: release
* local Ubuntu Linux: release
* win-builder: devel, release, oldrel
* R-hub (Ubuntu Linux): devel, release

## R CMD check results

There were no ERRORs or WARNINGs.

## Note

This is a patch version. In this version I have:
* attempted to fix a bug in one of the core methods `AUTO_VI$vss()` caused by invalid passed arguments.
* introduced a new method `AUTO_VI$save_plot()` to allow the user to specify arbitrary plot saving method 
