#' Function to check the distribution of LaTeX
#'
#' @return The flavor of latex instlation (e.g. miktex, texlive)
#' @examples
#' rte.check.latex.flavor()
#'
#' @export
rte.check.latex.flavor <-function(){

  # check latex flavor

  str.out <- system(command = 'pdflatex --version', intern = T)[1]

  flavor.latex <- 'unknown'
  if (stringr::str_detect(str.out, stringr::fixed('MiKTeX'))) flavor.latex <- 'miktex'
  if (stringr::str_detect(str.out, stringr::fixed('TeX Live'))) flavor.latex <- 'texlive'
  if (stringr::str_detect(str.out, stringr::fixed('MacTeX'))) flavor.latex <- 'mactex'

  return(flavor.latex)

}

#' Function to check if system has pdflatex.exe available
#'
#' @return TRUE if the pdflatex is available, FALSE if not
#' @examples
#' rte.check.pdflatex()
#'
#' @export
rte.check.pdflatex <-function(){

  flag <- FALSE

  try({system('pdflatex -version', intern = T)
    flag <- TRUE}, silent = T)


  return(flag)
}

#' Function to check operating system of user
#'
#' @return A string with the name of the operating system (e.g. Windows)
#' @examples
#' rte.check.my.os()
#'
#' @export
rte.check.my.os <-function(){

  my.sys <- Sys.info()

  return(my.sys['sysname']) # check OS
}

#' Function to compile a LaTeX file
#'
#' This function will first check for the flavor of latex, type of OS and then use the proper
#' command for pdflatex compilation
#'
#' @param f.in The location and name of latex file
#' @param pdf.dir.out The name of the folder for the output pdf
#' @param do.clean.up Clean (delete) auxiliary latex files? (TRUE or NOT)
#' @param latex.compile.fct Option for function compiling pdf ('texi2pdf' or 'custom')
#' @return A flag, TRUE if the latex compilation was a sucess and FALSE if not
#' @examples
#' f.in <- system.file("extdata", "MyRandomTest_examdesign.tex", package = "RndTexExams")
#' pdf.dir.out <- 'PdfOut'
#'
#' rte.compile.latex(f.in = f.in,
#'                  pdf.dir.out =  pdf.dir.out,latex.compile.fct = 'custom')
#'
#' @export
rte.compile.latex <- function(f.in,
                              pdf.dir.out = 'PdfOut',
                              do.clean.up = T,
                              latex.compile.fct = 'texi2pdf'){

  my.os <- rte.check.my.os()

  # for SunOS, force compilation with custom file
  # This is implemented due to CHECK error in CRAN for Solaris (texi2dvi doesnt work in this OS..)
  if (my.os=='SunOS'){
    latex.compile.fct <- 'custom'
  }

  if (!rte.check.pdflatex()){
    stop('Cant find pdflatex.exe. Make sure you have a flavor of LaTeX installed in your OS.')
  }

  if (!dir.exists(pdf.dir.out)){
    warning(paste('Folder', pdf.dir.out, 'does not exists. Creating it...'))
    dir.create(pdf.dir.out)
  }

  if (latex.compile.fct=='custom'){
    # compile latex using system command (it is faster than texi2pdf and it runs on solaris)

    my.latex.flavor <- rte.check.latex.flavor()

    if (my.latex.flavor =='miktex'){
       # for miktex, use -quiet
       my.c<- sprintf('pdflatex -quiet -interaction=batchmode -output-directory="%s" "%s"',pdf.dir.out, f.in)
       }

     if (any(my.latex.flavor ==c('texlive','mactex','unknown'))){
       # for texlive, mactex or unknown, use only batchmode
       my.c <- sprintf('pdflatex -interaction=batchmode -output-directory="%s" "%s"', pdf.dir.out, f.in)
     }

    if (my.os == 'Windows'){
       # use show.output.on.console for Windows
       system(command = my.c, show.output.on.console = F)
     } else {
       system(command = my.c)
     }

    # remove path and extension for checking pdf

    file.name.noext <- basename(tools::file_path_sans_ext(f.in))

  }


  if (latex.compile.fct=='texi2pdf'){

    # compile using tools::texi2pdf (new code in version 1.2)
    tools::texi2pdf(file = f.in, clean = do.clean.up, quiet = TRUE)

    # remove path and extension for checking pdf
    file.name.noext <- basename(tools::file_path_sans_ext(f.in))

    # copy pdf files to folder
    f.out.pdf <- paste0(file.name.noext,'.pdf')
    file.copy(from = f.out.pdf, to = pdf.dir.out )

    # delete pdf files manually
    file.remove(f.out.pdf)

  }

  # Check if pdf file exists
  if (!file.exists(paste0(pdf.dir.out, '/', file.name.noext, '.pdf'))){
    stop(paste('Cant find compiled pdf file. You should check for sintax errors in the tex file and whether your',
               'OS has a flavor of latex installed.'))
  }


}

#' Function to get number of cases (textual switches) based on question text
#'
#' @param str.in The full text of the question
#'
#' @return n.cases Number of cases (textual switches in string)
#'
#' @examples
#'
#' my.question <- 'My questions is ... @{ver1}|{ver2}@' ## two cases
#'
#' n.cases <- rte.get.n.cases(my.question)
#'
#' @export
rte.get.n.cases <- function(str.in){
  my.matches <- stringr::str_extract_all(str.in ,pattern = '@(.*?)@')[[1]]

  out.split <- lapply(my.matches, FUN = stringr::str_split, pattern=stringr::fixed('|'))
  n.cases <- unlist(lapply(out.split,FUN = function(x) length(x[[1]]) ))

  n.cases <- n.cases[1]
  if (is.null(n.cases)){
    return(1)
  } else {
    return(n.cases)
  }
}

#' Function that returns parameters of latex classes (internal use)
#'
#' This function outputs the main elements of each latex class such as question/choices identifiers and environmental switches.
#'
#' @param exam.class The class of the exam (exam, examdesign)
#'
#' @return A list with parameters of the class
#'
#' @export
#'
#' @examples
#' l.out <- rte.get.classes.def(exam.class = 'exam')
#' print(l.out)
rte.get.classes.def <- function(exam.class){

  l.out <- list()
  if (exam.class =='examdesign'){
    l.out$str.pattern.correct <- '\\choice[!]'
    l.out$str.pattern.choice <- '\\choice'

    l.out$str.pattern.beg.mchoice <- '\\begin{multiplechoice}'
    l.out$str.pattern.end.mchoice <- '\\end{multiplechoice}'

    l.out$str.pattern.beg.question <- '\\begin{question}'
    l.out$str.pattern.end.question <- '\\end{question}'

    l.out$str.pattern.identifier.mchoice <- '\\choice{'



  }

  if (exam.class =='exam'){
    l.out$str.pattern.correct <- '\\CorrectChoice'
    l.out$str.pattern.choice <- '\\choice'

    l.out$str.pattern.end.mchoice <- '\\end{questions}'
    l.out$str.pattern.end.question <- ' \\end{choices}'
    l.out$str.pattern.start.question <- '\\question'

    l.out$str.pattern.start.env <- '\\begin{questions}'
    l.out$str.pattern.end.env <- '\\end{questions}'

    l.out$str.pattern.identifier.mchoice <- '\\begin{choices}'
    l.out$str.pattern.identifier.oneparchoice <- '\\begin{oneparchoices}'

  }

  return(l.out)
}
