#' Build random tests from LaTeX file
#'
#' This function will take as input a list from rte.analize.tex.file and use it
#' to build pdf files of random exams. See the package vignette for details on
#' how to use it.
#'
#' @param list.in A list with all the information of the LaTeX file. Usually the
#'   output from funtion rte.analize.tex.file()
#' @param f.out The name for the pdf files (e.g. using f.out <- 'RdnTest_', the
#'   code will create files 'RdnTest_1.pdf', 'RdnTest_2.pdf', and so on)
#' @param n.test The number of random exams to be build (usually the number of
#'   students in class)
#' @param n.question The number of questions in each exam (If the LaTeX file has
#'   N questions, the code will select n.question of these)
#' @param latex.dir.out The name of the folder where the files from the latex
#'   compilation should go (will create if not found)
#' @param pdf.dir.out The name of the folder where the pdf files from the latex
#'   compilation should go (will create if not found)
#' @param do.randomize.questions Do you want the order of the questions to be
#'   random?
#' @param do.randomize.answers Do you want the order of the answers to be
#'   random?
#' @param do.clean.up Should R clean up all extra files from the LaTeX
#'   compilations and leave only the pdf? (select FALSE if you want see the log
#'   files from latex)
#' @return A list with the following items: \describe{ \item{df.out}{A dataframe
#'   with the tests, order of questions and correct answers}
#'   \item{answer.sheet}{The answer sheet for each random test (a wide version
#'   of table df.out)} }
#' @examples
#' # define some options
#' latex.dir.out = 'latexOut' # Name of folder where latex files are going (will create if not exists)
#' pdf.dir.out = 'PdfOut'     # Name of folder where resulting pdf files are going
#' f.out <- 'MyRandomTest_'   # Name of pdfs (MyRandomTest_1.pdf, MyRandomTest_2.pdf, ... )
#' n.test <- 10               # Number of tests to build
#' n.question <- 3            # Number of questions in each test
#'
#' # Get latex example from package
#' f.in <- system.file("extdata", "MyRandomTest.tex", package = "RndTexExams")
#'
#' # Break latex file into a R list
#' list.out <- rte.analize.tex.file(f.in,
#'                                  latex.dir.out = latex.dir.out,
#'                                  pdf.dir.out = pdf.dir.out)
#'
#' # Build pdfs
#' result.out <- rte.build.rdn.test(list.in = list.out,
#'                                  f.out = f.out,
#'                                  n.test = n.test,
#'                                  n.question = n.question,
#'                                  latex.dir.out = latex.dir.out,
#'                                  pdf.dir.out = pdf.dir.out,
#'                                  do.randomize.questions=TRUE,
#'                                  do.randomize.answers=TRUE,
#'                                  do.clean.up = TRUE)
#'
#' @export
rte.build.rdn.test <- function(list.in,
                               f.out,
                               n.test,
                               n.question,
                               latex.dir.out = 'latexOut',
                               pdf.dir.out = 'PdfOut',
                               do.randomize.questions=T,
                               do.randomize.answers=T,
                               do.clean.up = T){

  #require(data.table)
  cat('\nrte: Checking for error in inputs... ')

  # error catching (wrong number of questions)

  if (n.question> nrow(list.in$df.questions)){
    stop('The number of questions in test (n.question) is higher than the number of questions found in dataframe')
  }

  # error catching (pdftex not available)

  my.pdftex.flag <- rte.check.system.command('pdflatex')

  if (my.pdftex.flag=='Not Ok'){
    stop('cant find pdflatex.exe! Check your latex installation and also if the command is available at userpath')
  }

  cat('Done')

  # END error checking

  df.questions <- list.in$df.questions
  df.answers <- list.in$df.answers
  my.preamble <- list.in$my.preamble
  my.last.part <- list.in$my.last.part
  my.begin.mchoice.line <- list.in$my.begin.mchoice.line

  df.out <- data.frame()

  for (i.test in seq(1,n.test)){

    cat('\nrte: Building Test #',i.test,'...', sep = '')

    # Set version number in pdf

    my.temp_preamble <- sub(x = my.preamble,
                            pattern = '\\newcommand{\\myversion}{}',
                            replacement = sprintf('\\newcommand{\\myversion}{%s}',i.test), fixed = T )

    f.temp.tex <-  paste0(latex.dir.out,'/',f.out, i.test,'.tex')

    # Index to randomize questions

    if (do.randomize.questions){
      my.rdn.idx.question <- sample(nrow(df.questions))[1:n.question]
    } else {
      my.rdn.idx.question <- 1:n.question
    }

    my.tex.file <- paste0(my.temp_preamble,'\n',my.begin.mchoice.line)
    my.correct.answer <- character()
    n.version <-c()
    n.possible.versions <-c()

    for (i.q in seq(1,n.question)){

      q.now <- my.rdn.idx.question[i.q]

      q.text <- as.character(df.questions$main.text[q.now])

      temp_answers <- df.answers[df.answers$n.question==q.now, ]
      n.answers <- nrow(temp_answers)



      # Detect the existence of switch statemens in questions text

      my.matches <- stringr::str_extract_all(q.text,pattern = '@(.*?)@')[[1]]
      n.cases.qtext <-1
      case.now <- 1

      if (length(my.matches)>0){  # if found, fix it for case.now

        n.cases.qtext <- nrow(stringr::str_locate_all(my.matches[1], pattern = '\\{(.*?)\\}')[[1]])

        # select a random case

        case.now <- sample(n.cases.qtext)[1]

        n.matches <- length(my.matches)

        # fix q.text for n cases

        for (i.match in seq(1,n.matches)){

          temp.split <- stringr::str_match_all(my.matches[i.match], pattern = '\\{(.*?)\\}')[[1]]

          q.text <- sub(x = q.text,
                        pattern = my.matches[i.match],
                        replacement = temp.split[case.now,2],
                        fixed = T )

        }
      }


      # fix answers for different cases (switch @{}|{}@)

      all_answers <- paste0(temp_answers$text.answer,collapse = '\n')

      my.matches <- stringr::str_extract_all(all_answers,pattern = '@(.*?)@')[[1]]

      n.cases.answers <- length(my.matches)
      n.matches <- length(my.matches)


      for (i.match in my.matches){

        temp.split <- stringr::str_match_all(i.match, pattern = '\\{(.*?)\\}')[[1]]

        all_answers <- sub(x = all_answers,
                           pattern = i.match,
                           replacement = temp.split[case.now,2],
                           fixed = T )


      }

      temp_answers$text.answer <- stringr::str_split(all_answers, pattern = '\\n')[[1]]

      # fix correct answers for different cases

      idx.correct <-which(stringr::str_detect(temp_answers$text.answer, stringr::fixed(paste0('[',case.now,']'))))

      n.cases.correct.answers <- sum(stringr::str_detect(temp_answers$text.answer, '\\[.*?\\]'))

      for (i.cases in seq(n.cases.correct.answers)){
        idx <-which(stringr::str_detect(temp_answers$text.answer, stringr::fixed(paste0('[',i.cases,']'))))

        temp_answers$text.answer[idx] <- stringr::str_replace_all(string = temp_answers$text.answer[idx],
                                                         pattern = stringr::fixed(paste0('[',i.cases,']')),
                                                         replacement = '')

      }

      temp_answers$text.answer[idx.correct] <- sub(pattern = '\\choice{',
                                                   x = temp_answers$text.answer[idx.correct],
                                                   replacement = '\\choice[!]{',
                                                   fixed= T)


      my.tex.file <- paste0(my.tex.file, '\n', q.text)

      # check for different versions

      n.answers <- nrow(temp_answers)

      if (do.randomize.answers){
        my.rdn.idx.answers <- sample(n.answers)
      } else {my.rdn.idx.answers <- 1:n.answers}

      for (i.ans  in seq(1,n.answers)){
        a.now <- my.rdn.idx.answers[i.ans]
        a.text <- as.character(temp_answers$text.answer[a.now])

        my.tex.file <- paste0(my.tex.file, '\n', a.text)

      }

      idx<-which(stringr::str_detect(temp_answers$text.answer[my.rdn.idx.answers],stringr::fixed('[!]')))

      my.correct.answer <- c(my.correct.answer, letters[idx])
      n.version <-c(n.version,case.now)
      n.possible.versions <- c(n.possible.versions, max(c(n.cases.qtext,n.cases.answers)))

      my.tex.file <- paste0(my.tex.file, '\n', '\\end{question} \n')

    }

    my.tex.file <- paste0(my.tex.file, '\n', '\\end{multiplechoice}','\n',my.last.part)

    stringi::stri_write_lines(my.tex.file,fname = f.temp.tex, encoding = 'UTF-8')

    rte.compile.latex(f.temp.tex, pdf.dir.out)


    df.out <- rbind(df.out, data.frame(n.test = rep(i.test,n.question),
                                       n.question = seq(1,n.question),
                                       rnd.idx.questions = my.rdn.idx.question,
                                       n.version = n.version,
                                       n.possible.versions = n.possible.versions,
                                       correct.answer= my.correct.answer ))

    cat('Done')
  }


  # clean up files

  if (do.clean.up){
    my.temp.files <- dir(pdf.dir.out, pattern = '*.*', full.names = T)

    my.temp.files <- my.temp.files[which(!stringr::str_detect(my.temp.files,pattern ='.pdf' ))]

    if (length(my.temp.files)!=0) file.remove(my.temp.files)
  }

  cat('\nrte: FINISHED - Check folder', pdf.dir.out, 'for all pdf files with the tests')

  answer.sheet <- data.table::dcast(data = data.table::data.table(df.out),
                        formula = n.test  ~ n.question,
                        fun.aggregate = function(x) return(x), value.var = 'correct.answer', fill = NA )


  list.out <-list(answer.sheet = answer.sheet, df = df.out)

  return(list.out)

}
