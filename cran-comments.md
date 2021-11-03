## Resubmission
This is a resubmission. In this version I have:

* Change codecov URL to https://app.codecov.io/gh/JorgeDelro/MFO?branch=master.
* References have been added to the DESCRIPTION file.
* TRUE and FALSE were written instead of T and F.
* \value have been added to .Rd files.
* functions do not write by default  in the user's home filespace. tempdir() is used.
* \dontrun{} has been removed from the examples.
* User's working directory is saved and setting after the example is executed in a temporary directory

## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)
* windows-x86_64-devel (r-devel)
* ubuntu-gcc-release (r-release)
* fedora-clang-devel (r-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
