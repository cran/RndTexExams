#' Function to randomize a question in a dataframe (internal use)
#'
#' @param q.text Main text of question (character)
#' @param q.answers The answers of the questions as an atomic vector (each alternative as an item)
#' @param case.now The random case to build the string (only used for textual switches)
#' @param my.rdn.idx.answers The random index of the questions
#'
#' @return A list with the text of the full questions, among other things
#'
#' @export
#'
#' @examples
#' q.text <- '\\question Whats my name \\n'
#' q.answers <- c('\\choice Mario', '\\choice Roberto','\\choice Marcelo')
#'
#' case.now <- 1
#' my.rdn.idx.answers <- sample(seq(length(q.answers)))
#'
#' l.out <- rte.The.Randomizer(q.text,q.answers, case.now, my.rdn.idx.answers)
rte.The.Randomizer <- function(q.text,q.answers, case.now, my.rdn.idx.answers){

  q.answers.rnd <- q.answers[my.rdn.idx.answers]

  full.question <- paste(q.text, paste(q.answers.rnd, collapse = '\n'), collapse = '\n')

  my.matches <- stringr::str_extract_all(full.question ,pattern = '@(.*?)@')[[1]]

  out.split <- lapply(my.matches, FUN = stringr::str_split, pattern=stringr::fixed('|'))
  n.cases <- unlist(lapply(out.split,FUN = function(x) length(x[[1]]) ))

  test.size <- all(n.cases  == n.cases[1]) # check if all switches have the same number of cases

  if (!test.size){
    stop(paste('ERROR: In question:\n\n',
               q.text, '\n', paste(q.answers, collapse = '\n'),
               '\n\n',
               'There are a different number of textual switches in each case (please fix it).',
               'Below you can check the textual switches that dont match:\n\n',
               paste(my.matches, collapse = '\n')))

  }

  n.matches <- length(my.matches)

  if (n.matches==0){
    return(list(full.question = full.question,
                case.now = case.now,
                q.text = q.text,
                q.answers.rnd = q.answers.rnd,
                q.answers = q.answers))
  }





  for (i.match in seq(n.matches)){

    match.now <- my.matches[i.match]

    temp.split <- stringr::str_match_all(match.now, pattern = '\\{(.*?)\\}')[[1]]

    full.question <- sub(x = full.question,
                  pattern = match.now,
                  replacement = temp.split[case.now,2],
                  fixed = T )



  }

  return(list(full.question = full.question,
              case.now = case.now,
              q.text = q.text,
              q.answers.rnd = q.answers.rnd,
              q.answers = q.answers))

}
