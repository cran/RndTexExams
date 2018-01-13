#' Build random tests from LaTeX file
#'
#' This function will take as input a list from rte.analyze.tex.file and use it
#' to build pdf files of random exams. See the package vignette for details on
#' how to use it.
#'
#' @param list.in A list with all the information of the LaTeX file. Usually the
#'   output from funtion rte.analyze.tex.file()
#' @param f.out The name for the pdf files (e.g. using f.out <- 'RdnTest_', the
#'   code will create files 'RdnTest_1.pdf', 'RdnTest_2.pdf', and so on)
#' @param n.test The number of random exams to be build (usually the number of
#'   students in class)
#' @param n.question The number of questions in each exam (If the LaTeX file has
#'   N questions, the code will randomly select n.question of these)
#' @param latex.dir.out The name of the folder where the files from the latex
#'   compilation should go (will create if not found)
#' @param pdf.dir.out The name of the folder where the pdf files from the latex
#'   compilation should go (will create if not found)
#' @param latex.compile.fct Option for defining function that calls pdflatex: 'texi2pdf' or 'custom' (default).
#' @param do.randomize.questions Do you want the order of the questions to be
#'   random? (TRUE or FALSE)
#' @param do.randomize.answers Do you want the order of the answers to be
#'   random? (TRUE or FALSE)
#' @param do.clean.up Should R clean up all extra files from the LaTeX
#'   compilations and leave only the pdf? (select FALSE if you want see the log
#'   files from latex)
#' @return A list with the following items: \describe{ \item{df.answer.wide}{A dataframe
#'   with the tests, order of questions and correct answers}
#'   \item{answer.matrix}{A matrix with the correct answers (rows = version, columns = questions)} }
#' @examples
#' # define some options
#' latex.dir.out = 'latexOut' # Name of folder where latex files are going (will create if not exists)
#' pdf.dir.out = 'PdfOut'     # Name of folder where resulting pdf files are going
#' f.out <- 'MyRandomTest_'   # Name of pdfs (MyRandomTest_1.pdf, MyRandomTest_2.pdf, ... )
#' n.test <- 1                # Number of tests to build
#' n.question <- 2            # Number of questions in each test
#'
#' # Get latex example from package
#' f.in <- system.file("extdata", "MyRandomTest_examdesign.tex", package = "RndTexExams")
#'
#' # Break latex file into a R list
#' list.out <- rte.analyze.tex.file(f.in,
#'                                  latex.dir.out = latex.dir.out,
#'                                  pdf.dir.out = pdf.dir.out)
#'
#' # Build pdfs
#' result.out <- rte.build.rdn.test(list.in = list.out,
#'                                  f.out = f.out,
#'                                  n.test = n.test,
#'                                  n.question = n.question,
#'                                  latex.dir.out = latex.dir.out,
#'                                  pdf.dir.out = pdf.dir.out)
#'
#' @export
rte.build.rdn.test <- function(list.in,
                               f.out,
                               n.test,
                               n.question,
                               latex.dir.out = 'latexOut',
                               pdf.dir.out = 'PdfOut',
                               latex.compile.fct = 'custom',
                               do.randomize.questions=T,
                               do.randomize.answers=T,
                               do.clean.up = T){

  #require(data.table)
  cat('\nrte: Checking for error in inputs... ')

  # error catching (wrong number of questions)

  if (n.question> nrow(list.in$df.questions)){
    stop('The number of mchoice questions in test (n.question) is higher than the number of mchoice questions found in dataframe')
  }

  # error catching (pdftex not available)

  my.pdftex.flag <- rte.check.pdflatex()

  if (!my.pdftex.flag){
    stop('cant find pdflatex.exe! Check your latex installation and also if the command is available at userpath')
  }

  cat('Done')

  # error catching (input latex.compile.fct)

  if (sum(latex.compile.fct == c('texi2pdf','custom'))==0){
    stop('Input latex.compile.fct should either be "texi2pdf" or "custom" ')
  }


  # END error checking here (many more within engines)

  df.questions <- list.in$df.questions
  df.answers <- list.in$df.answers
  my.preamble <- list.in$my.preamble
  my.last.part <- list.in$my.last.part
  exam.class <- list.in$examclass
  my.begin.mchoice.line <- list.in$my.begin.mchoice.line

  # str patterns for exam classes

  l.def <- rte.get.classes.def(exam.class)

  str.pattern.correct <- l.def$str.pattern.correct
  str.pattern.choice <- l.def$str.pattern.choice
  str.pattern.end.mchoice <- l.def$str.pattern.end.mchoice
  str.pattern.end.question <- l.def$str.pattern.end.question

  df.out <- data.frame()

  cat('\nrte: pdflatex flavor:', rte.check.latex.flavor())
  cat('\nrte: Type of OS:', rte.check.my.os())
  cat('\nrte: Latex compile function:', latex.compile.fct)
  cat('\nrte: Type of exam template:', exam.class)
  cat('\nrte: Number of mchoice questions:', nrow(df.questions))

  for (i.test in seq(1,n.test)){

    cat('\nrte: Building Test #',i.test,'...', sep = '')

    # Set version number in pdf

    my.temp_preamble <- sub(x = my.preamble,
                            pattern = '\\newcommand{\\myversion}{}',
                            replacement = sprintf('\\newcommand{\\myversion}{%s}',i.test), fixed = T )

    f.temp.tex <-  paste0(latex.dir.out,'/',f.out, i.test,'.tex')

    # Index to randomize questions

    if (do.randomize.questions){
      my.rdn.idx.question <- sample(df.questions$n.question)
    } else {
      my.rdn.idx.question <- df.questions$n.question
    }

    my.tex.file <- paste0(my.temp_preamble,'\n',my.begin.mchoice.line)

    correct.answer.original <- character()
    correct.answer.rnd <- character()
    n.version <-c()
    n.possible.versions <-c()

    for (i.q in seq(1,n.question)){


      q.now <- my.rdn.idx.question[i.q]

      q.text <- as.character(df.questions$main.text[df.questions$n.question==q.now])

      q.answers <- df.answers[df.answers$n.question==q.now, ]$text.answer
      n.answers <- length(q.answers)

      # fix correct answers for different cases (use of [ver])

      n.cases <- rte.get.n.cases(paste(q.text,q.answers,collapse = '\n'))


      if (do.randomize.answers){
        case.now <- sample(seq(n.cases),1)
        my.rdn.idx.answers <- sample(seq(n.answers))
      } else {
        case.now <- 1
        my.rdn.idx.answers <- 1:n.answers
      }

      if (n.cases >1){

        idx.correct.switch <-which(stringr::str_detect(q.answers, stringr::fixed(paste0('[',case.now,']'))))

        n.cases.correct.answers <- sum(stringr::str_detect(q.answers, '\\[.*?\\]'))

        if (is.na(n.cases.correct.answers)) n.cases.correct.answers <- 1

        for (i.cases in seq(n.cases.correct.answers)){
          idx <-which(stringr::str_detect(q.answers, stringr::fixed(paste0('[',i.cases,']'))))

          q.answers[idx]<- stringr::str_replace_all(string = q.answers[idx],
                                                    pattern = stringr::fixed(paste0('[',i.cases,']')),
                                                    replacement = '')

        }

        # error control for use of correct choice and

        idx.fix <- which(stringr::str_detect(q.answers, stringr::fixed(str.pattern.correct)))
        q.answers[idx.fix] <- sub(pattern = str.pattern.correct,
                                  x = q.answers[idx.fix],
                                  replacement = str.pattern.choice,
                                  fixed= T)



        q.answers[idx.correct.switch] <- sub(pattern = str.pattern.choice,
                                      x = q.answers[idx.correct.switch],
                                      replacement = str.pattern.correct,
                                      fixed= T)

      }

      # correct answers in original file

      idx.correct.original <-which(stringr::str_detect(q.answers, stringr::fixed(str.pattern.correct)))

      out.list <- rte.The.Randomizer(q.text,q.answers, case.now, my.rdn.idx.answers)

      full.question <- out.list$full.question
      case.now <- out.list$case.now

      # correct answers in randomized file

      idx.correct.rnd <-which(stringr::str_detect(out.list$q.answers.rnd, stringr::fixed(str.pattern.correct)))


      my.tex.file <- paste0(my.tex.file, '\n', full.question)

      correct.answer.original <- c(correct.answer.original, letters[idx.correct.original])
      correct.answer.rnd <- c(correct.answer.rnd, letters[idx.correct.rnd])

      n.version <-c(n.version,case.now)
      n.possible.versions <- c(n.possible.versions, n.cases)

      my.tex.file <- paste0(my.tex.file, '\n', str.pattern.end.question, '\n')

    }


    # paste questions that are not mchoice at the end

    if (exam.class =='exam'){

      my.tex.file <- paste0(my.tex.file, '\n',
                            paste(list.in$df.questions.not.mchoice$q.text,
                                  collapse = '\n'), '\n')

    }

    my.tex.file <- paste0(my.tex.file, '\n', str.pattern.end.mchoice ,'\n',my.last.part)

    stringi::stri_write_lines(my.tex.file,fname = f.temp.tex, encoding = 'UTF-8')

    rte.compile.latex(f.temp.tex,
                      pdf.dir.out = pdf.dir.out,
                      latex.compile.fct = latex.compile.fct)


    df.out <- rbind(df.out, data.frame(n.test = rep(i.test,n.question),
                                       n.question = seq(1,n.question),
                                       rnd.idx.questions = my.rdn.idx.question,
                                       n.version = n.version,
                                       n.possible.versions = n.possible.versions,
                                       correct.answer.original = correct.answer.original,
                                       correct.answer.rnd= correct.answer.rnd))

    cat('Done')
  }


  # clean up files

  if (do.clean.up){
    my.temp.files <- dir(pdf.dir.out, pattern = '*.*', full.names = T)

    my.temp.files <- my.temp.files[which(!stringr::str_detect(my.temp.files,pattern ='.pdf' ))]

    if (length(my.temp.files)!=0) file.remove(my.temp.files)
  }

  cat('\nrte: FINISHED - Check folder', pdf.dir.out, 'for pdf files')

  df.answer.wide <- data.table::dcast(data = data.table::data.table(df.out),
                                      formula = n.test  ~ n.question,
                                      fun.aggregate = function(x) return(x), value.var = 'correct.answer.rnd', fill = NA )


  answer.matrix <- as.matrix(as.data.frame(df.answer.wide)[,c(1+seq(n.question))])

  rownames(answer.matrix) <- paste('Version',seq(n.test))

  list.out <-list(df.answer.wide = df.answer.wide,
                  answer.matrix = answer.matrix,
                  df.answer.long = df.out)

  return(list.out)

}
