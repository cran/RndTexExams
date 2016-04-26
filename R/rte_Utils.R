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
#' @return A flag, TRUE if the latex compilation was a sucess and FALSE if not
#' @examples
#' f.in <- system.file("extdata", "MyRandomTest.tex", package = "RndTexExams")
#' pdf.dir.out <- 'PdfOut'
#'
#' rte.compile.latex(f.in = f.in,
#'                  pdf.dir.out =  pdf.dir.out)
#'
#' @export
rte.compile.latex <- function(f.in,
                              pdf.dir.out = 'PdfOut',
                              do.clean.up = T){

  #require(tools)

  my.os <- rte.check.my.os()

  if (!rte.check.pdflatex()){
    stop('Cant find pdflatex.exe. Make sure you have a flavor of LaTeX installed in your OS.')
  }

  # compile latex using system command (OLD code, version 1.1.1)
  # KEEP IT FOR FUTURE REFERENCE

  # my.latex.flavor <- rte.check.latex.flavor()
  #
  # if (my.latex.flavor =='miktex'){
  #   # for miktex, use -quiet
  #   my.c<- sprintf('pdflatex -quiet -interaction=batchmode -output-directory="%s" "%s"',pdf.dir.out, f.in)
  # }
  #
  # if (any(my.latex.flavor ==c('texlive','mactex','unknown'))){
  #   # for texlive, mactex or unknown, use only batchmode
  #   my.c <- sprintf('pdflatex -interaction=batchmode -output-directory="%s" "%s"', pdf.dir.out, f.in)
  # }
  #
  # if (my.os == 'Windows'){
  #   # use show.output.on.console for Windows
  #   system(command = my.c, show.output.on.console = F)
  # } else {
  #   system(command = my.c)
  # }

  # compile using tools::texi2pdf (new code in version 1.2)

  tools::texi2pdf(file = f.in, clean = do.clean.up, quiet = T)

  # remove path and extension for checking pdf

  file.name.noext <- basename(tools::file_path_sans_ext(f.in))

  # check if folder exists

  if (!dir.exists(pdf.dir.out)){
    warning(paste('Folder', pdf.dir.out, 'does not exists. Creating it...'))
    dir.create(pdf.dir.out)
  }

  # copy pdf files to folder

  f.out.pdf <- paste0(file.name.noext,'.pdf')

  file.copy(from = f.out.pdf, to = pdf.dir.out )

  # delete pdf files

  file.remove(f.out.pdf)

  # Check if pdf file exists
  if (!file.exists(paste0(pdf.dir.out, '/', file.name.noext, '.pdf'))){
    stop(paste('Cant find compiled pdf file with  pdflatex. You should check for sintax errors in the tex file and whether your',
               'OS has a flavor of latex installed.'))
  }


}
