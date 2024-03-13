#'------------------------------------------------------------------------------
#' Script para executar os scripts da EAPI
#' Autor : Arthur Maciel, Henique Rosseti, Leonardo Claudino.
#' Data : 05/23
#'Atualização:
#'------------------------------------------------------------------------------

# Limpando ambiente ------------------------------------------------------------

rm(list = objects())

# Importando as bibliotecas necessárias
library(GetTDData)
library(ggplot2)
library(dplyr)

# Definindo os ativos e o intervalo de tempo desejado
assets <- 'LTN'
first_year <- 2005
last_year <- 2023

# Coletando dados dos ativos especificados
df_td <- td_get(assets,
                first_year,
                last_year)

# Verificando os códigos únicos dos ativos
unique(df_td[,4])

# Filtrando os dados para um ativo específico
my_asset_code <- "LTN 010117" 
LTN <- df_td %>%
  filter(asset_code  ==  my_asset_code)

# Criando um gráfico de linha para os preços em função das datas
p <- ggplot(data = LTN, 
            aes(x = as.Date(ref_date), 
                y = price_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Datas', y = 'Preços')
print(p)

# Criando um gráfico de linha para os rendimentos em função das datas
p <- ggplot(data = LTN, 
            aes(x = as.Date(ref_date), 
                y = yield_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Datas', y = 'Yield')
print(p)

# Filtrando os dados para um ativo específico com uma data de vencimento
LTN <-  df_td %>% filter(matur_date == as.Date("2025-01-01") )

# Criando um gráfico de linha para os preços em função das datas
p <- ggplot(data = LTN, 
            aes(x = as.Date(ref_date), 
                y = price_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Dates')

print(p)

# Mudando o ativo para "NTN-B"
assets <- 'NTN-B'   # Identificador dos ativos 
first_year <- 2005
last_year <- 2023

# Coletando dados dos ativos especificados
df_td <- td_get(assets,
                first_year,
                last_year)

# Verificando os códigos únicos dos ativos
unique(df_td[,4])

# Filtrando os dados para um ativo específico
my_asset_code <- "NTN-B 150824"
NTNB <- df_td %>%
  filter(asset_code  ==  my_asset_code)

# Criando um gráfico de linha para os preços em função das datas
p <- ggplot(data = NTNB, 
            aes(x = as.Date(ref_date), 
                y = price_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Datas', y = 'Preços')
print(p)

# Criando um gráfico de linha para os rendimentos em função das datas
p <- ggplot(data = NTNB, 
            aes(x = as.Date(ref_date), 
                y = yield_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Datas', y = 'Yield')
print(p)

# Verificando os códigos únicos dos ativos
unique(df_td[,5])

# Filtrando os dados para um ativo específico com uma data de vencimento
NTNB <-  df_td %>% filter(matur_date == as.Date("2024-08-15") )

# Criando um gráfico de linha para os preços em função das datas
p <- ggplot(data = NTNB, 
            aes(x = as.Date(ref_date), 
                y = price_bid, 
                color = asset_code)) + 
  geom_line(linewidth = 1) + scale_x_date() + labs(title = '', x = 'Dates')

print(p)

# Obtendo a curva de rendimento
df.yield <- get.yield.curve()  

# Exibindo informações sobre a curva de rendimento
str(df.yield)

# Criando um gráfico de linha para a curva de rendimento atual do Brasil
p <- ggplot(df.yield, aes(x=ref.date, y = value) ) +
  geom_line(size=1) + geom_point() + facet_grid(~type, scales = 'free') + 
  labs(title = paste0('Curva de Rendimento Atual do Brasil'),
       subtitle = paste0('Date: ', df.yield$current.date[1]))    

print(p)
