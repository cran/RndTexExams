## ----eval=T--------------------------------------------------------------
library(RndTexExams)

set.seed(10)

# Get latex file from package
f.in <- system.file("extdata", "MyRandomTest.tex", package = "RndTexExams")

# Breakdown latex file into a a list 
list.out <- rte.analize.tex.file(f.in,
                                 latex.dir.out = 'latexOut',
                                 pdf.dir.out = 'PdfOut') 


# Options for build.rdn.test
list.in <- list.out       # output from rte.analize.tex.file
f.out <- 'MyRandomTest_'  # pattern for names of pdfs
n.test <- 10            # number of random tests (usually the number of students) 
n.question <- 4           # number of questions in each test 
pdf.dir.out <- 'PdfOut'   # directory for pdf output

# Builds pdfs
list.build.rdn.exam <- rte.build.rdn.test(list.in = list.in,
                                          f.out = f.out,
                                          n.test = n.test,
                                          n.question = n.question,
                                          pdf.dir.out = pdf.dir.out) 

## ----eval=TRUE-----------------------------------------------------------
print(list.build.rdn.exam$answer.matrix)

#write.csv(x = list.build.rdn.exam$answer.df, file = 'Answer_Matrix_csv')


## ----eval=TRUE-----------------------------------------------------------
#set.seed(11)

# create some (almost) random names
my.names <- c('John', 'Max','Michael','Marcelo','Ricardo', 'Tarcizio')

# official names from the university system
official.names <- c('John A.', 'Max B.','Michael C.','Marcelo P.','Ricardo P.','Tarcizio P.')

# version of the test for each student
ver.test <- sample(seq(1:length(my.names)))

# number of simulated questions (same as before)
n.questions <- 4

# Get the correct answer sheet from previous code 
correct.answer.sheet <- list.build.rdn.exam$answer.matrix

# create simulated answers from students (cheat a little bit!)
q.to.cheat <- floor(n.questions/2)  # get at least half of questions right!
my.answers <- cbind(correct.answer.sheet[ver.test,1:q.to.cheat], 
                    matrix(sample(letters[1:5],                                          
                                  replace = T,
                                  size = length(my.names)*(n.questions-q.to.cheat)),
                           ncol = n.questions-q.to.cheat ))

# grade exams with rte.grade.exams 
list.grade <- rte.grade.exams(exam.names = my.names,
                              official.names = official.names,
                              exam.version = ver.test, 
                              exam.answer.matrix = my.answers,
                              list.build.rdn.exam = list.build.rdn.exam)

## ---- eval= TRUE---------------------------------------------------------
# print results in a bar plot

library(ggplot2)

p <- ggplot(list.grade$df.final.score, aes(y = final.score, x = official.names))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

p <- ggplot(list.grade$df.grade, aes(y = n.question, x = official.names, fill = grade.logical))
p <- p + geom_tile()
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

## ---- eval=FALSE---------------------------------------------------------
#  library(googlesheets)
#  
#  gs_auth()   # will open a broswer for authentication
#  my.gs <- gs_title(x = 'RndTexExam Example Form (respostas)') # or any title of spreadsheet
#  
#  df.exam.answers <- gs_read(ss = my.gs)
#  
#  # ...
#  # rest of code for grading
#  

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
#  n.question <- 4
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

