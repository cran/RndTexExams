## ----eval=T--------------------------------------------------------------
library(RndTexExams)


# Get latex file from package
f.in <- system.file("extdata", "MyRandomTest.tex", package = "RndTexExams")

# Breakdown latex file into a a list 
list.out <- rte.analize.tex.file(f.in,
                                 latex.dir.out = 'latexOut',
                                 pdf.dir.out = 'PdfOut') 


# Options for build.rdn.test
list.in <- list.out       # output from rte.analize.tex.file
f.out <- 'MyRandomTest_'  # pattern for names of pdf
n.test <- 10              # number of random tests 
n.question <- 3           # number of questions in each test
pdf.dir.out <- 'PdfOut'   # directory for output

# Builds pdfs
out <- rte.build.rdn.test(list.in = list.in,
                          f.out = f.out,
                          n.test = n.test,
                          n.question = n.question,
                          pdf.dir.out = pdf.dir.out) 

## ----eval=FALSE----------------------------------------------------------
#  setwd('Your path goes here')
#  download.file(url = 'https://gist.github.com/msperlin/ef1b93a8eb9026ba5e9a/raw/MyRandomTest.tex', destfile = 'MyRandomTest.tex' )

## ----eval=FALSE----------------------------------------------------------
#  library(RndTexExams)
#  
#  my.d <- 'Your folder to the tex file here!'
#  setwd(my.d)
#  
#  f.in <- 'MyRandomTest.tex'
#  f.out <- 'RandomTest-'
#  n.test <- 5
#  n.question <- 3
#  latex.dir.out <- 'latexOut'
#  pdf.dir.out <- 'PdfOut'
#  
#  list.out <- rte.analize.tex.file(f.in,
#                                   latex.dir.out = latex.dir.out,
#                                   pdf.dir.out = pdf.dir.out)
#  
#  out <- rte.build.rdn.test(list.in = list.out,
#                            f.out = f.out,
#                            n.test = n.test,
#                            n.question = n.question,
#                            latex.dir.out = latex.dir.out)

