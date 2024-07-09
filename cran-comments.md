## R CMD check environments
* local mac OS X: release
* local Ubuntu Linux: release
* win-builder: devel, release, oldrel
* R-hub (Ubuntu Linux): devel, release

## R CMD check results

There were no ERRORs or WARNINGs.

## Note

* This is a resubmission of the package
* For this resubmission, I have followed the reviewer's comment to avoid starting the description field with words such as "This package" and "Tools for", and have updated the field accordingly.
* Additionally, I have removed all instances of "if(interactive())"" from the examples as suggested by the reviewer. The "try()" function is now used to handle dependency errors in the examples.
* Unfortunately, there is no published paper yet describing the methods implemented in this package. However, I am currently drafting a paper as part of my PhD thesis, which will include a section dedicated to explaining the methodologies used in this package. The paper is planned for completion towards the end of this year, coinciding with the conclusion of my PhD program. I will add the reference to the description field and update the package once it is available.
