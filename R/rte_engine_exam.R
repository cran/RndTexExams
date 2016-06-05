#' Function that breaks latex file of exam class exam into a dataframe (internal use)
#'
#' @param my.text Text of latex file written as an exam template (UTF-8)
#'
#' @return A dataframe with several information (see rte.analyze.tex.file)
#'
#' @examples
#' f.in <- system.file("extdata", "MyRandomTest_exam.tex", package = "RndTexExams")
#' my.text <- stringi::stri_read_lines(f.in)
#' Encoding(my.text) <- 'UTF-8'
#'
#' out <- engine.analyze.class.exam(my.text)
#'
#' @export
engine.analyze.class.exam <- function(my.text){

  # get parameters

  l.def <- rte.get.classes.def(exam.class = 'exam')

  # find idx of text of questions (exam class does not diferentiate mchoice from openchoice)

  line.beg.questions <- which(stringr::str_detect(string = my.text,
                                                  stringr::fixed(l.def$str.pattern.start.env)))
  line.end.questions <- which(stringr::str_detect(string = my.text,
                                                  stringr::fixed(l.def$str.pattern.end.env)))

  questions.text <- my.text[line.beg.questions:line.end.questions]

  # find beggining and ending of all multiple choice questions

  idx.beg.questions <- which(stringr::str_detect(string = questions.text, stringr::fixed(l.def$str.pattern.start.question)))
  idx.end.questions  <- c(idx.beg.questions[2:length(idx.beg.questions)]-1, length(questions.text))

  # find text part before and after multiple choice questions

  my.preamble <- paste0(my.text[1:(line.beg.questions[1]-1)], collapse = ' \n')
  my.last.part <- paste0(my.text[(line.end.questions[length(line.end.questions)]+1):length(my.text)], collapse = '\n')

  # Find text with options for \begin{questions}

  idx.mchoice.comm <- which(stringr::str_detect(string = my.text, stringr::fixed(l.def$str.pattern.start.env)))
  my.begin.mchoice.line <- my.text[idx.mchoice.comm]

  # build dataframe with questions to be randomized

  df.questions <- data.frame(n.question = seq(1,length(idx.beg.questions)),
                             idx.begin = idx.beg.questions,
                             idx.end = idx.end.questions)

  # function to get text from m.questions

  locate.questions <- function(x,mchoice.text){
    q.temp <- mchoice.text[x['idx.begin']:(x['idx.end'])]
    q.temp <- paste0(q.temp,collapse = '\n')
    return(q.temp)
  }

  df.questions$q.text <- apply(df.questions,MARGIN = 1 ,
                               FUN = locate.questions,
                               mchoice.text=questions.text)

  # function to locate the main text (header) of questions

  locate.header <- function(x, l.def){
    q.temp <- x['q.text']
    idx1 <- stringr::str_locate(q.temp, stringr::fixed(l.def$str.pattern.choice))[1]
    idx2 <- stringr::str_locate(q.temp, stringr::fixed(l.def$str.pattern.correct))[1]
    idx3 <- stringr::str_locate(q.temp, stringr::fixed('[1]'))[1]

    # error control for no definition of correct answer

    if ( (is.na(idx2))&(!is.na(idx1))&(is.na(idx3))){
      stop(paste('ERROR: Correct choice mark not found for question\n\n:',
                 q.temp,
                 '\n\n You must edit the latex file, assigning the correct choice with \\CorrectChoice (see exam manual for details) '))
    }

    # error control oneparchoices

    is.oneparchoices <- stringr::str_detect(q.temp, stringr::fixed(l.def$str.pattern.identifier.oneparchoice))[1]


    if (is.oneparchoices){
      stop(paste('ERROR: Found \\begin{oneparchoices} in question:',
                 q.temp,
                 '\nThe command \\begin{oneparchoices} is not supported by rndtexexams.',
                 'You must edit the latex file, using \\begin{choices} and \\end{choices} instead of oneparchoices.'))
    }


    idx <- NA
    if (!is.na(idx1)|!is.na(idx2)){
      idx<- min(c(idx1,idx2), na.rm = T)
    }

    if (!is.na(idx)){
      q.temp <- stringr::str_sub(q.temp,1,idx-2)
    }


    return(q.temp)
  }

  df.questions$main.text <- apply(df.questions,MARGIN = 1 ,
                                  FUN = locate.header,
                                  l.def = l.def)


  # figure out which questions are mchoice and which are not

  is.mchoice <- function(x, l.def){
    q.temp <- x['q.text']
    idx <- stringr::str_locate(q.temp, stringr::fixed(l.def$str.pattern.identifier.mchoice))[1]

    if ( !is.na(idx) ){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  df.questions$is.mchoice <- apply(df.questions,
                                   MARGIN = 1,
                                   FUN = is.mchoice,
                                   l.def = l.def)



  df.answers <- data.frame() # Dataframe with all answers to all questions
  for (i.q in df.questions$n.question){

    q.temp <- df.questions$q.text[i.q]
    q.temp <- stringr::str_split(q.temp, pattern = '\\n')[[1]] # break lines

    idx.choice <- which(stringr::str_detect(string = q.temp, pattern = stringr::fixed(l.def$str.pattern.choice)))
    idx.correctchoice <-which(stringr::str_detect(string = q.temp, pattern = stringr::fixed(l.def$str.pattern.correct)))

    idx.choices <- c(idx.choice, idx.correctchoice)
    idx.choices <- sort(idx.choices)

    if (length(idx.choices)==0){

      idx.choices <- NA

    }

    my.main.text <- paste(df.questions$main.text[i.q], collapse = '\\n')

    my.answers <- as.character(q.temp[idx.choices])
    my.correct.answers <- stringr::str_detect(string = my.answers, l.def$str.pattern.correct)

    # Build data.frame for output

    df.answers <- rbind(df.answers, data.frame(n.question = rep(i.q, length(my.answers)),
                                               main.text = rep(my.main.text,length(my.answers)),
                                               text.answer = my.answers,
                                               correct.answer = my.correct.answers,
                                               stringsAsFactors=F ))

  }

  # fix for questions that are not mchoice (only for exam class)

  df.questions.not.mchoice <- df.questions[!df.questions$is.mchoice, ]
  df.questions <- df.questions[df.questions$is.mchoice, ]

  df.answers <- df.answers[!is.na(df.answers$text.answer), ]

  # return a list with questions, answers, preamble and last part of tex file

  out <- list(df.questions=df.questions, # df with all questions
              df.questions.not.mchoice = df.questions.not.mchoice,
              df.answers = df.answers,   # df with all answers
              my.begin.mchoice.line = my.begin.mchoice.line, # text with begging of mchoice enviroment
              my.preamble = my.preamble, # preamble of tex file
              my.last.part = my.last.part) # last part of tex file

  return(out)

}
