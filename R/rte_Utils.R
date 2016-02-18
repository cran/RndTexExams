#' Function to check if system command is available
#'
#' @param mycommand A system command (e.g. pdflatex inputfile.tex)
#' @return A flag, TRUE if the command is available  or FALSE if not)
#' @examples
#' rte.check.system.command('pdflatex')
#'
#' @export
rte.check.system.command <-function(mycommand){

  # check windows or linux

  my.sys <- rte.check.my.os()

  flag <- 0

  if (my.sys=='Windows'){
    flag <- system(paste('where',mycommand),
                   show.output.on.console = F,
                   intern = F,
                   ignore.stderr = T,
                   ignore.stdout = T)
  }

  if (my.sys=='Linux'){
    flag <-  system(paste('which', mycommand), ignore.stdout = T )
  }

  if (flag==0) return('OK')
  if (flag==1) return('Not Ok')
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

  return(my.sys['sysname']) # check if its windows or linux
}

#' Function to compile a LaTeX file
#'
#' This function will first check for the user's OS and then use the proper
#' command for pdflatex compilation
#'
#' @param f.in The location and name of latex file
#' @param pdf.dir.out The name of the folder for the output pdf
#' @return A flag, TRUE if the latex compilation was a sucess and FALSE if not
#' @examples
#' f.in <- system.file("extdata", "MyRandomTest.tex", package = "RndTexExams")
#' pdf.dir.out <- 'PdfOut'
#'
#' \dontrun{rte.compile.latex <- function(f.in = f.in,
#'                              pdf.dir.out =  pdf.dir.out) }
#'
#' @export
rte.compile.latex <- function(f.in,
                              pdf.dir.out = 'PdfOut'){

  #require(tools)

  my.os <- rte.check.my.os()

  my.flag <- 0 #default value (other OS)

  if (my.os=='Windows'){
    my.c<- sprintf('pdflatex -quiet -interaction=nonstopmode "%s" -output-directory=%s\\',f.in, pdf.dir.out)
    system(command = my.c, show.output.on.console = F)
  }

  if (my.os =='Linux'){

    my.c <- sprintf('pdflatex -output-directory=%s -interaction=nonstopmode %s >/dev/null', pdf.dir.out, f.in)
    system(command = my.c)

  }

  # remove path and extension for checking pdf

  file.name.noext <- basename(tools::file_path_sans_ext(f.in))

  if (file.exists(paste0(pdf.dir.out, '/', file.name.noext, '.pdf'))){
    my.flag<-1
  }

  if (my.flag==0){
    #browser()
    stop(paste('Cant compile file', f.in,' with  pdflatex. You should check for sintax errors in the tex file and whether your',
                'OS has miktex or texlive installed. This package was been tested on Windows 10 and linux mint.'))
    }


}
