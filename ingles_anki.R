library(magrittr)

"https://www.ted.com/talks/rita_pierson_every_kid_needs_a_champion/transcript" %>%
  xml2::read_html() %>%
  xml2::xml_find_all("//div[contains(@class, 'Grid--with-gutter')]") %>%
  purrr::map_chr(xml2::xml_text) %>%
  stringr::str_squish() %>%
  dplyr::tibble(speech = .)

url <- "https://www.ted.com/talks/rita_pierson_every_kid_needs_a_champion/transcript"


teste <- url %>% 
  xml2::read_html()

teste <- url %>%
  xml2::read_html() %>% 
  xml2::xml_find_all("//div[contains(@class, 'Grid--with-gutter')]") %>%
  purrr::map_chr(xml2::xml_text) %>%
  stringr::str_squish() %>%
  dplyr::tibble(speech = .)
