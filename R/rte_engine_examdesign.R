#' Function that breaks latex file of exam class examdesign into a dataframe (internal use)
#'
#' @param my.text Text of latex file written in class examdesign (UTF-8)
#'
#' @return A dataframe with several information (see rte.analyze.tex.file)
#'
#' @examples
#' f.in <- system.file("extdata", "MyRandomTest_examdesign.tex", package = "RndTexExams")
#' my.text <- stringi::stri_read_lines(f.in)
#' Encoding(my.text) <- 'UTF-8'
#'
#' out <- engine.analyze.class.examdesign(my.text)
#'
#' @export
engine.analyze.class.examdesign <- function(my.text){

  # specific error checking for examdesign

  detect.nokey <- stringr::str_detect(my.text,
                                      stringr::fixed('\\NoKey'))

  if (!any(detect.nokey)){
    warning('wARNING:  Could not find command "\\NoKey". The exams will be printed with all answer keys!')
  }

  detect.norearrange <-  stringr::str_detect(my.text,
                                             stringr::fixed('\\NoRearrange '))
  if (!any(detect.norearrange)){
    stop('ERROR:  Could not find command "\\NoRearrange". Please add it to the preamble of tex file')
  }

  # get parameters

  l.def <- rte.get.classes.def(exam.class = 'examdesign')

  # find idx of text of multiple choice questions

  line.beg.mchoice <- which(stringr::str_detect(string = my.text,
                                                stringr::fixed(l.def$str.pattern.beg.mchoice)))
  line.end.mchoice   <- which(stringr::str_detect(string = my.text,
                                                  stringr::fixed(l.def$str.pattern.end.mchoice)))

  mchoice.text <- my.text[line.beg.mchoice:line.end.mchoice]

  # find beggining and ending of all multiple choice questions

  idx.beg.mchoices <- which(stringr::str_detect(string = mchoice.text, stringr::fixed(l.def$str.pattern.beg.question)))
  idx.end.mchoices <- which(stringr::str_detect(string = mchoice.text, stringr::fixed(l.def$str.pattern.end.question)))

  # find text part before and after multiple choice questions

  my.preamble <- paste0(my.text[1:(line.beg.mchoice[1]-1)], collapse = ' \n')
  my.last.part <- paste0(my.text[(line.end.mchoice[length(line.end.mchoice)]+1):length(my.text)], collapse = '\n')

  # Find text with options for \begin{multiplechoice}

  idx.mchoice.comm <- which(stringr::str_detect(string = my.text, stringr::fixed(l.def$str.pattern.beg.mchoice)))
  my.begin.mchoice.line <- my.text[idx.mchoice.comm]

  # build dataframe with questions to be randomized

  df.questions <- data.frame(n.question = seq(1,length(idx.beg.mchoices)),
                             idx.begin = idx.beg.mchoices,
                             idx.end = idx.end.mchoices)

  # function to get text from m.questions

  locate.questions <- function(x,mchoice.text){
    q.temp <- mchoice.text[x['idx.begin']:(x['idx.end'])]
    q.temp <- paste0(q.temp,collapse = '\n')
    return(q.temp)
  }

  df.questions$q.text <- apply(df.questions,MARGIN = 1 , FUN = locate.questions, mchoice.text=mchoice.text)

  # function to locate main text (header) of all m.questions

  locate.header <- function(x, l.def){
    q.temp <- x['q.text']
    idx1 <- stringr::str_locate(q.temp, stringr::fixed(l.def$str.pattern.choice))[1]
    idx2 <- stringr::str_locate(q.temp, stringr::fixed(l.def$str.pattern.correct))[1]

    idx3 <- stringr::str_locate(q.temp, stringr::fixed('[1]'))[1]

    if ( (is.na(idx2))&(!is.na(idx1))&(is.na(idx3))){
      stop(paste('ERROR: Correct choice mark not found for question\n\n:',
                 q.temp,
                 '\n\n You must edit the latex file, assigning the correct choice with \\choice[!]{} or [ver]',
                '(see examdesign manual or examples of latex files for rndtexexam for details) '))
    }

    idx<- min(c(idx1,idx2), na.rm = T)

    if (!is.na(idx)){
      q.temp <- stringr::str_sub(q.temp,1,idx-2)
    }


    return(q.temp)
  }

  df.questions$main.text <- apply(df.questions,MARGIN = 1 , FUN = locate.header, l.def)


  df.answers <- data.frame() # Dataframe with all answers to all questions
  for (i.q in df.questions$n.question){

    q.temp <- df.questions$q.text[i.q]
    q.temp <- stringr::str_split(q.temp, pattern = '\\n')[[1]] # break lines

    idx.choice <- which(stringr::str_detect(string = q.temp, pattern = stringr::fixed(l.def$str.pattern.choice)))

    my.main.text <- as.character(paste0('\n',q.temp[1:(idx.choice[1]-1)], collapse = '\n'))

    my.answers <- as.character(q.temp[idx.choice])

    # Build data.frame for output

    df.answers <- rbind(df.answers, data.frame(n.question = rep(i.q, length(my.answers)),
                                               main.text = rep(my.main.text,length(my.answers)),
                                               text.answer = my.answers,
                                               stringsAsFactors=F ))

  }

  # return a list with questions, answers, preamble and last part of tex file

  out <- list(df.questions=df.questions, # df with all questions
              df.answers = df.answers,   # df with all answers
              my.begin.mchoice.line = my.begin.mchoice.line, # text with begging of mchoice enviroment
              my.preamble = my.preamble, # preamble of tex file
              my.last.part = my.last.part) # last part of tex file

  return(out)

}
