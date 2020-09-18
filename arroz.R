## 1. PACOTES
library(readr)
library(tidyverse)
library(lubridate)
library(scales)
library(httr)

## 2. LEITURA DOS DADOS
temp <- tempfile()
download.file(
  url = "http://www.mdic.gov.br/balanca/bd/comexstat-bd/ncm/EXP_COMPLETA.zip",
  destfile = temp)
datazip <- unzip(temp, files = 'EXP_COMPLETA.csv')
base <- read_csv2(datazip)

## 3. CÓDIGOS NCM - Nomenclatura Comum do Mercosul
ncm <- read_csv2("http://www.mdic.gov.br/balanca/bd/tabelas/NCM.csv",
                 locale = locale(encoding = "Latin1")) %>%
  janitor::clean_names() %>%
  dplyr::select(co_ncm, no_ncm_por) %>%
  magrittr::set_colnames(c("codigo", "nome")) %>%
  filter(str_detect(codigo, pattern = "^1006")) %>%
  pull(codigo)

## 4. BASE DE DADOS (TIDY)
arroz <- base %>%
  janitor::clean_names() %>%
  dplyr::filter(co_ncm %in% ncm) %>%
  dplyr::mutate(co_mes = as.numeric(co_mes),
                co_unid = as.character(co_unid),
                date = lubridate::make_date(year = co_ano, month = co_mes),
                ton = kg_liquido/1000,
                vl_fob = vl_fob/1000) %>%
  dplyr::relocate(date) %>%
  dplyr::select(!co_ano & !co_mes & !kg_liquido) %>%
  dplyr::arrange(desc(date)) %>%
  dplyr::group_by(ano = year(date), mes = month(date)) %>%
  dplyr::summarise(across(where(is.numeric), .fns = ~ sum(.x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(date = make_date(year = ano, month = mes)) %>%
  dplyr::select(!ano & !mes) %>%
  dplyr::relocate(date) %>%
  dplyr::arrange(desc(date))

## 3. IMPORTAR DADOS DA TAXA DE CÂMBIO
dolar <- rbcb::get_series(code = list(dolar = '3698'))

## 4. JUNTAR TIBBLES
dados <- arroz %>%
  left_join(dolar, by = "date")

## 6. CONTRUINDO GRÁFICOS

## FUNÇÃO AUXILIAR (UNITS)
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        'too big!'
                                 ))))
  return(labels)
}

## GRÁFICO DE LINHA COM EIXO SECUNDÁRIO
limits <- dados %>%
  filter(date >= '2010-01-01') %>%
  slice_max(date, n = 5) %>%
  pull(date)

filter(dados, date >= '2010-01-01') %>%
ggplot(aes(x = date, y = ton)) +
  geom_line(aes(x = date, y = ton), size = 1, color = "red") +
  geom_line(aes(x = date, y = dolar * 25000), size = 1, color = "blue") +
  scale_y_continuous(
    name = 'Toneladas de Arroz Exportada\n',
    labels = addUnits,
    sec.axis = sec_axis(trans = ~ . / 25000,
                        name = 'Taxa de Câmbio Média (Venda)\n')
  ) +
  theme_bw() +
  scale_x_date(
    name = "",
    breaks = limits,
    labels = date_format("%b. %Y"),
    expand = c(0.02,0.02)) +
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(title = 'Exportações de Arroz vs. Taxa de Câmbio',
       caption = 'Fonte: Elaboração própria com dados do BACEN e MDIC.')

## GRÁFICO DE COLUNA COM EIXO SECUNDÁRIO
ggplot(data = dados) +
  geom_col(aes(x = data, y = ton)) +
  geom_line(aes(x = data, y = dolar * 30000), size = 1, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(
    name = "",
    breaks = dados$data,
    labels = date_format("%b/%Y"),
    limits = as.Date(c("2019-01-01", "2020-09-01"))
  ) +
  theme_bw() +
  scale_y_continuous(
    name = 'Toneladas de Arroz Exportadas\n',
    labels = addUnits,
    sec.axis = sec_axis(
      trans = ~ . / 30000,
      name = 'Taxa de Câmbio Média (Venda)\n',
      breaks = seq(from = 0, to = 8, by = 1.5)
    )
  )



