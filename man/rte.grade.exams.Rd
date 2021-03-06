% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rte_grade_exams.R
\name{rte.grade.exams}
\alias{rte.grade.exams}
\title{Grade exams built using rte.grade.exams}
\usage{
rte.grade.exams(exam.names, exam.version, exam.answer.matrix,
  list.build.rdn.exam, question.points = NULL)
}
\arguments{
\item{exam.names}{A character vector with the names of the students, obtained from the test}

\item{exam.version}{A numeric vector with the version of the exam for each student, obtained from the exam}

\item{exam.answer.matrix}{A matrix with the answers of the students where the rows represent each student and the columns are the answers to each question}

\item{list.build.rdn.exam}{A list with several information of the random exams (output from rte.build.rdn.text)}

\item{question.points}{A numeric vector with the score for each question (default = 1/n.question)}
}
\value{
A list with the following items: \describe{
\item{df.grade}{A dataframe with the partial resuts from grading}
  \item{df.final.score}{A dataframe with the final results for each student} }
}
\description{
This function will take as input the information from the exam and grade it using the framework of \pkg{RndTexExams}
}
\examples{
# define some options
latex.dir.out = 'latexOut' # Name of folder where latex files are going (will create if not exists)
pdf.dir.out = 'PdfOut'     # Name of folder where resulting pdf files are going
f.out <- 'MyRandomTest_'   # Name of pdfs (MyRandomTest_1.pdf, MyRandomTest_2.pdf, ... )
n.test <- 1                # Number of tests to build
n.question <- 2            # Number of questions in each test

# Get latex example from package
f.in <- system.file("extdata", "MyRandomTest_examdesign.tex", package = "RndTexExams")

# Break latex file into a R list
list.out <- rte.analyze.tex.file(f.in,
                                 latex.dir.out = latex.dir.out,
                                 pdf.dir.out = pdf.dir.out)

# Build pdfs
list.build.rdn.exam <- rte.build.rdn.test(list.in = list.out,
                                          f.out = f.out,
                                          n.test = n.test,
                                          n.question = n.question,
                                          latex.dir.out = latex.dir.out,
                                          pdf.dir.out = pdf.dir.out,
                                          do.randomize.questions=TRUE,
                                          do.randomize.answers=TRUE,
                                          do.clean.up = TRUE)

# Grade it!
#' # create some (almost) random names
my.names <- c('John', 'Max','Marcelo')

# version of the test for each student
ver.test <- sample(seq(n.test),size = length(my.names),replace=TRUE)

# Get the correct answer sheet from previous code
correct.answer.sheet <- list.build.rdn.exam$answer.matrix

# create simulated answers from students (cheat a little bit!)
q.to.cheat <- 1  # get at least 1 question right!
my.answers <- cbind(correct.answer.sheet[ver.test,1:q.to.cheat],
                    matrix(sample(letters[1:5],
                                  replace = TRUE,
                                  size = length(my.names)*(n.question-q.to.cheat)),
                           ncol = n.question-q.to.cheat ))

# grade exams with rte.grade.exams
list.grade <- rte.grade.exams(exam.names = my.names,
                              exam.version = ver.test,
                              exam.answer.matrix = my.answers,
                              list.build.rdn.exam = list.build.rdn.exam)

print(list.grade$df.final.score)
}
