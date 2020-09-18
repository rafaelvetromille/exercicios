library(tidyverse)
library(stringr)
library(readtext)

arquivos <- list.files(pattern = "nota[0-9]{1,2}\\.pdf$")

out = purrr::map_dfr(.x = arquivos, .f = readtext) %>%
  select(text) %>%
  pull() %>%
  str_squish() %>%
  str_to_lower()

df <- tibble(

  # Identificador da Nota
  id = arquivos,

  # Número da Nota
  notas = out %>%
    str_extract("data pregão: .+?(?= folha)") %>%
    str_extract("(?<=nº nota: )[0-9]+"),

  # Data do Pregão
  data_pregao = out %>%
    str_extract("data pregão: .+?(?= folha)") %>%
    str_extract("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"),

  # Classificação entre compra ou venda
  cv = out %>%
    str_extract("(?<=d/c ).+(?= resumo dos negócios)") %>%
    str_split("1-") %>%
    purrr::map(.f = ~ keep(.x, .p = str_length(.x) > 1)) %>%
    purrr::map(.f = str_squish) %>%
    purrr::map(str_extract, "(?<=bovespa )(.*)(?= vis)"),

) %>%
  unnest(cv)

titulo = out %>%
  str_extract("(?<=d/c ).+(?= resumo dos negócios)") %>%
  str_split("1-") %>%
  purrr::map(.f = ~ keep(.x, .p = str_length(.x) > 1)) %>%
  purrr::map(.f = str_squish) %>%
  purrr::map(str_extract, "(?<=vis ).+?(?= [0-9]+ [0-9]+,)") %>%
  set_names(letters[1:length(arquivos)]) %>%
  purrr::map_dfr(~ as_tibble(., .name_repair = "minimal")) %>%
  magrittr::set_colnames('acao')

valores = out %>%
  str_extract("(?<=d/c ).+(?= resumo dos negócios)") %>%
  str_split("1-") %>%
  purrr::map(.f = ~ keep(.x, .p = str_length(.x) > 1)) %>%
  purrr::map(.f = str_squish) %>%
  purrr::map(str_extract, "(?<=subtotal : )(.*)(,[0-9]{2})") %>%
  purrr::map_dfr(~ as_tibble(., .name_repair = "minimal")) %>%
  separate(value, c("qtd","preco", "valor"), sep = " ") %>%
  mutate(across(.cols = where(is.character),
                .fns = ~ parse_number(.x, locale = locale(decimal_mark = ","))))


final <- bind_cols(df, titulo, valores) %>%
  mutate(across(where(is.character), str_to_upper)) %>%
  rename_all(toupper)


