### Version 1.3 (2016-05-03)

  - Now using either texi2pdf or custom call to pdflatex (custom call is slightly faster, somehow)
  - When using Solaris OS, the code will force custom call to pdflatex. This should fix error in CRAN CHECK for this platform (tex2pdf does not seems to work in Solaris OS)
  - Added code for testing pairwise hypothesis of cheating based on student's answer sheet. The code is now integrated with package CopyDetect
  - Added a new vignette (Testing for visual cheating with RndTexExams)

### Version 1.2 (2016-04-22)

  - Now using tools::texi2pdf() instead of custom call to pdflatex
  - More input checks
  - Added code for grading tests
  - Improved vignette text

### Version 1.1.1 (2016-02-19)

  - Fixed function for latex compilation by checking latex flavor and type of OS

### Version 1.1.0 (2016-02-18)

  - Redefined pdflatex command (excluding sintex)
  - Fixed bugs in vignette and examples
  - Improved text of vignette
  - Fixed function for latex compilation by checking latex flavor and type of OS
  
### Version 1.0.0 - Initial version (2016-02-01)
