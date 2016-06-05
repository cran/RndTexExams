#' Analyze a LaTeX file and convert it into a list
#'
#' This function will take as input a LaTeX file and break its components into a
#' single R List. The class of the latex file should be either exam or examdesign.
#' The code identifies the latex class automatically.
#'
#' @param f.in The latex file with the exam
#' @param latex.dir.out The name of the folder where the files from the latex
#'   compilation should go (will create if not found)
#' @param pdf.dir.out The name of the folder where the pdf from the latex
#'   compilation should go (will create if not found)
#' @return A list that represents the tex file with preamble, questions, answers
#'   and more. This list is later used by function rte.build.rdn.text \describe{
#'   \item{df.questions}{A data.frame with all questions} \item{df.answers}{A
#'   data.frame with all answers} \item{my.begin.mchoice.line}{text with
#'   beggining of mchoice enviroment} \item{my.preamble}{preamble of tex file,
#'   including everything before the beggining of the multiple choice enviroment
#'   } \item{my.last.part}{All of the tex code after the end of the multiple
#'   choice enviroment } }
#' @examples
#' latex.dir.out <- 'latexOut' # Name of folder where latex files are going
#'                             #(will create if it does not exists)
#'
#' pdf.dir.out <- 'PdfOut'     # Name of folder where resulting pdf files are going
#'
#' # Get latex example from package
#' f.in <- system.file("extdata", "MyRandomTest_examdesign.tex", package = "RndTexExams")
#'
#' # Break latex file into a R list
#' list.out <- rte.analyze.tex.file(f.in,
#'                                 latex.dir.out = latex.dir.out,
#'                                 pdf.dir.out = pdf.dir.out)
#'
#' print(list.out)
#' @export
rte.analyze.tex.file <- function(f.in,
                                 latex.dir.out = 'latexOut',
                                 pdf.dir.out = 'PdfOut'){

  cat('\nrte: Changing LaTeX file into dataframe...')

  # create folder for latex output

  if (!file.exists(latex.dir.out)) dir.create(latex.dir.out)

  # create folder for pdf output

  if (!file.exists(pdf.dir.out)) dir.create(pdf.dir.out)

  # clean folders

  my.temp.files <- dir(latex.dir.out, pattern = '*.*', full.names = T)
  if (length(my.temp.files)!=0) file.remove(my.temp.files)

  my.temp.files <- dir(pdf.dir.out, pattern = '*.*', full.names = T)
  if (length(my.temp.files)!=0) file.remove(my.temp.files)

  # read tex file

  my.text <- stringi::stri_read_lines(f.in)
  Encoding(my.text) <- 'UTF-8'

  # exam or examdesign ?

  possible.templates <- c('exam','examdesign')

  idx <- which(stringr::str_detect(string = my.text, stringr::fixed('\\documentclass')))
  temp <- my.text[idx]

  exam.class <- ''
  if (stringr::str_detect(temp,stringr::fixed('{examdesign}'))){
    exam.class <- 'examdesign'
  }

  if (stringr::str_detect(temp,stringr::fixed('{exam}'))){
    exam.class <- 'exam'
  }

  if (!any(exam.class %in% possible.templates)){
    stop(paste(c('The document class of latex file is not one of the following: \n ',
                paste(possible.templates,collapse=', '),
                '\nRndtexexams only work on those, so far..')))
  }

  my.test.vernumber <- any(stringr::str_detect(my.text, stringr::fixed('\\newcommand{\\myversion}{}')))

  if (!my.test.vernumber){
    stop(paste(c('ERROR: could not find in preamble the command: \n\n ',
                 '\\newcommand{\\myversion}{}',
                 '\n\n You should mannualy add it, exactly as it is, to the preamble of tex file as it controls the version of the exam.')))

  }

  # delete all lines with comments

  my.test.comment <- function(str.in){
    first.letter <- stringr::str_sub(str.in,1,1)

    if (first.letter =='%'){
      return(TRUE)
    } else {
      return(FALSE)
    }

  }

  idx <- sapply(my.text,FUN = my.test.comment, USE.NAMES = F)

  my.text<- my.text[!idx]


  if (exam.class=='examdesign'){

    out <- engine.analyze.class.examdesign(my.text)
    out$examclass <- 'examdesign'
  }

  if (exam.class =='exam'){

    out <- engine.analyze.class.exam(my.text)
    out$examclass <- 'exam'
  }

  cat(' Done')
  return(out)

}
