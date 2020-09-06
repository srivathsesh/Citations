

#' Gets citation for a given authorname
#'
#' @param authorname string partial or full name of author
#' @param df dataframe of citations from Aravind's Rmd file
#'
#' @return tibble of paper and citation_paper
#' @export
#'
#' @examples getCitationPaper(authorname = "mcgee")
getCitationPaper <- function(authorname,df=citation_df){
  library(dplyr)
  library(magrittr)
  library(textcat)

  df[(str_detect(string = tolower(df$authors), pattern = authorname)), ] %>% 
    select(paper,citing_paper) %>% 
    as_tibble() %>% 
    mutate(language = textcat(citing_paper)) %>% 
    filter(!is.na(paper),language == 'english') %>% 
      select(-language)
  
  }

