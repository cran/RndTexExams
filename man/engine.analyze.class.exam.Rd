% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rte_engine_exam.R
\name{engine.analyze.class.exam}
\alias{engine.analyze.class.exam}
\title{Function that breaks latex file of exam class exam into a dataframe (internal use)}
\usage{
engine.analyze.class.exam(my.text)
}
\arguments{
\item{my.text}{Text of latex file written as an exam template (UTF-8)}
}
\value{
A dataframe with several information (see rte.analyze.tex.file)
}
\description{
Function that breaks latex file of exam class exam into a dataframe (internal use)
}
\examples{
f.in <- system.file("extdata", "MyRandomTest_exam.tex", package = "RndTexExams")
my.text <- stringi::stri_read_lines(f.in)
Encoding(my.text) <- 'UTF-8'

out <- engine.analyze.class.exam(my.text)

}
