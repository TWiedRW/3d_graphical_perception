library(tidyverse)
library(words)

#### This is necessary to remove naughty words from the word list before generating codes
# library(words) # Word game dictionary, used to generate response code
data(words, package = "words")
badwords <- readLines("words-to-avoid.txt") %>%
  str_remove_all("[\\d]{1,}") %>%
  str_split("[\\s-]") %>% 
  unlist() %>% 
  unique() %>%
  tibble(word = .) %>%
  dplyr::filter(nchar(word) > 0)

contains_badwords <- filter(badwords, nchar(word) > 3) %>%
  `[[`("word") %>%
  paste(collapse = "|")

words <- words %>%
  anti_join(badwords) %>%
  dplyr::filter(!str_detect(word, contains_badwords))

generate_completion_code <- function(nwords = 4, sep = '-', exclusion_file = "codes.txt") {
  # Generate new code
  newcode <- words %>%
    sample_n(size = nwords) %>%
    select(word) %>%
    unlist() %>%
    as.character() %>%
    paste(., sep = sep, collapse = sep)
  
  oldcodes <- readLines(exclusion_file)
  if (newcode %in% oldcodes) {
    message("Code collision, generating new code")
    newcode <- generate_completion_code(nwords = nwords, sep = sep, exclusion_file = exclusion_file)
  }
  
  writeLines(c(oldcodes, newcode), exclusion_file)
  return(newcode)
}
