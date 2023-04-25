
# Configuración y parametría de la API de Twitter

## Limpiar consola y reiniciar sesión
rm(list = ls())
.rs.restartR()
## Quitar notación científica
options(scipen = 999)
## Setear ruta de trabajo y de guardado de archivos
path = ".../UTDT/Tesis/"
files_path = ".../UTDT/Tesis/Datos/"
setwd(path)
## Cargar paquetes
suppressPackageStartupMessages({
  library(rtweet)
  library(wordcloud)
  library(RColorBrewer)
  library(plyr)
  library(ggplot2)
  library(tm)
  library(RSentiment)
  library(twitteR)
  library(SnowballC)
  library(wordcloud)
  library(twitteR)
  library(dplyr)
  library(lubridate)
  library(SentimentAnalysis)
  library(quanteda)
  library(gridExtra)
  library(streamR)
  library(ROAuth)
  library(base64enc)
  library(tidyverse)
  library(tidytext)
  library(rjson)
  library(threadr)
})
## Acceder a la API de Twitter
## Cargar parámetros de configuración
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache = T)
## Consumer API keys
## Access token & access token secret
api_key <- "......"
api_secret <- "......"
access_token <- "......"
access_token_secret <- "......"
twitter_app <- "Reputational Risk in Banks"
## Acceder a Twitter mediante los datos del token
create_token(
  app = twitter_app,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret
)
# Ejecutar la autenticación de TwitteR
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Recolección de Tweets

## Palabras claves relacionadas al Banco Santander Río
palabras <- c("#bancosantanderrio",
              "#bancosantander",
              "bancosantander",
              "bancosantanderrio",
              "@bancosantander",
              "@Santander_Ar")
## Búsqueda de palabras claves
tweets <- search_tweets2(
  q = palabras,
  n = 10000,
  retryonratelimit = TRUE
)
## Guardar Tweets como archivo json
dia = 5 ## variar el día de recolección
write_json_lines(df = tweets,
                 file = paste0(files_path,"dia",dia,"_total.json"))

# Análisis de series financieras 

## Cargar paquetes para el análisis de series financieras
suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(hrbrthemes)
  library(gridExtra)
  library(reshape2)
  library(fBasics)
  library(dlookr)
  library(xlsx)
  library(lessR)
  library(psych)
  library(funModeling)
})
## Tickers de las Entidades Financieras cotizantes en BYMA
tickers = c("BHIP.BA",
            "BMA.BA",
            "BPAT.BA",
            "BRIO.BA",
            "BBAR.BA",
            "GGAL.BA",
            "VALO.BA",
            "SUPV.BA")
desde = "2018-01-01"
hasta = Sys.Date()
getSymbols(tickers,
           auto.assign = TRUE,
           src = "yahoo",
           from = desde,
           to = hasta,
           periodicity = "daily")
## Se definen los data frame de cada activo
valores <- as.data.frame(VALO.BA)
hipotecario <- as.data.frame(BHIP.BA)
macro <- as.data.frame(BMA.BA)
patagonia <- as.data.frame(BPAT.BA)
supervielle <- as.data.frame(SUPV.BA)
galicia <- as.data.frame(GGAL.BA)
rio <- as.data.frame(BRIO.BA)
frances <- as.data.frame(BBAR.BA)
## Gráficos de series temporales con ggplot2
g1 <- ggplot(valores) + geom_line(mapping = aes(index(valores),
                                                Ad(VALO.BA)))
g1 <- g1 + labs(title = "Grupo Financiero Valores",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g1 <- g1 + theme_bw()
g2 <- ggplot(hipotecario) + geom_line(mapping = aes(index(hipotecario),
                                                    Ad(BHIP.BA)))
g2 <- g2 + labs(title = "Banco Hipotecario",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g2 <- g2 + theme_bw()
g3 <- ggplot(macro) + geom_line(mapping =
                                  aes(index(macro),
                                      Ad(BMA.BA)))
g3 <- g3 + labs(title = "Banco Macro",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g3 <- g3 + theme_bw()
g4 <- ggplot(patagonia) + geom_line(mapping = aes(index(patagonia),
                                                  Ad(BPAT.BA)))
g4 <- g4 + labs(title = "Banco Patagonia",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g4 <- g4 + theme_bw()
g5 <- ggplot(supervielle) + geom_line(mapping = aes(index(supervielle),
                                                    Ad(SUPV.BA)))
g5 <- g5 + labs(title = "Banco Supervielle",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g5 <- g5 + theme_bw()
g6 <- ggplot(galicia) + geom_line(mapping = aes(index(galicia),
                                                Ad(GGAL.BA)))
g6 <- g6 + labs(title = "Grupo Financiero Galicia",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g6 <- g6 + theme_bw()
g7 <- ggplot(rio) + geom_line(mapping =aes(index(rio),
                                           Ad(BRIO.BA)))
g7 <- g7 + labs(title = "Banco Santander Río",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g7 <- g7 + theme_bw()
g8 <- ggplot(frances) + geom_line(mapping = aes(index(frances),
                                                Ad(BBAR.BA)))
g8 <- g8 + labs(title = "Banco BBVA Francés",
                subtitle = "Desde Enero 2018 a 2021") +
  xlab("Fecha") + ylab("")
g8 <- g8 + theme_bw()
## Exportar gráficos en grillas
jpeg("Bancos 1.jpg")
grid.arrange(g1,g2,g3,g4)
dev.off()
jpeg("Bancos 2.jpg")
grid.arrange(g5,g6,g7,g8)
dev.off()
## Gráfico de Retornos Acumulados
list <- lapply(tickers, function(x) Ad(get(x)))
precio.cierre <- do.call(merge,list)
retornos <- data.frame(apply(precio.cierre, 2,
                             function(x)
                               Delt(x, type = "log")),
                       fecha = index(precio.cierre)) %> %
  rename(valo = VALO.BA.Adjusted,
         ggal = GGAL.BA.Adjusted,
         bbar = BBAR.BA.Adjusted,
         brio = BRIO.BA.Adjusted,
         macro = BMA.BA.Adjusted,
         hipo = BHIP.BA.Adjusted,
         pata = BPAT.BA.Adjusted,
         supv = SUPV.BA.Adjusted) %> %
  na.omit()
acumulados <- data.frame(apply(retornos[1:8], 2, function(x) cumsum(x)),
                         fecha = index(precio.cierre[-1]))
reshape <- melt(acumulados, id.vars = "fecha")
h <- ggplot(reshape) +
  geom_line(mapping = aes(fecha,value,
                          color = variable))
h <- h + labs(title = "Retornos Acumulados",
              subtitle = "Bancos Cotizantes en BYMA")

h <- h + theme_bw() +
  xlab("Fecha") + ylab("Retornos Acumulados")
h <- h + scale_color_manual("Tickers", values = c("red","blue","green",
                                                  "orange","yellow","black",
                                                  "brown","violet"))
h <- h + theme(legend.position = "bottom")
h
## Exportar gráfico de retornos acumulados
jpeg("Retornos.jpg")
h
dev.off()
## Estadística Descriptiva
summary <- basicStats(retornos[1:8])[c("Mean", "Stdev", "Median", "Minimum",
                                       "Variance","Maximum",
                                       "nobs","Skewness","Kurtosis"),]
summary
names(retornos)
## Exportar la tabla descriptiva a una carpeta local
write.xlsx(summary,
           file = "Descriptiva.xlsx",
           sheetName = "Descriptiva Retornos")
describe(retornos)
### Configuración del data frame como objeto tibble
retornos <- as_tibble(retornos[1:8])
plot_num(retornos,
         bins = 30,
         path_out = getwd())
## Exportar gráfico de outliers retornos
jpeg("Outliers Valores.jpg")
plot_outlier(retornos,valo)
dev.off()
jpeg("Outliers Hipotecario.jpg")
plot_outlier(retornos,hipo)
dev.off()
jpeg("Outliers Macro.jpg")
plot_outlier(retornos,macro)
dev.off()

jpeg("Outliers Francés.jpg")
plot_outlier(retornos,bbar)
dev.off()
jpeg("Outliers Galicia.jpg")
plot_outlier(retornos,ggal)
dev.off()
jpeg("Outliers Santander.jpg")
plot_outlier(retornos,brio)
dev.off()
jpeg("Outliers Patagonia.jpg")
plot_outlier(retornos,pata)
dev.off()
## Exportar diagnóstico de outliers
diagnostico <- diagnose_outlier(retornos)
write.xlsx(x = diagnostico,
           file = "Outliers.xlsx",
           sheetName = "Outliers Retornos")
## Exportar gráfico de correlación de retornos
jpeg("Correlación Retornos.jpg")
chart.Correlation(retornos, histogram = TRUE, pch = 15)
dev.off()
correlate(retornos)
jpeg("Correlación Retornos2.jpg")
plot_correlate(retornos)
dev.off()
retornos %> %
  eda_web_report(target = "hipo")
## Exportar gráfico de retornos acumulados
### Exportación de gráficos
jpeg("Normalidad Hipotecario.jpg")
dlookr::plot_normality(retornos, hipo)
dev.off()
jpeg("Normalidad Valores.jpg")
dlookr::plot_normality(retornos, valo)
dev.off()
jpeg("Normalidad Macro.jpg")

dlookr::plot_normality(retornos, macro)
dev.off()
jpeg("Normalidad Patagonia.jpg")
dlookr::plot_normality(retornos, pata)
dev.off()
jpeg("Normalidad Santander Río.jpg")
dlookr::plot_normality(retornos, brio)
dev.off()
jpeg("Normalidad Galicia.jpg")
dlookr::plot_normality(retornos, ggal)
dev.off()
jpeg("Normalidad Francés.jpg")
dlookr::plot_normality(retornos, bbar)
dev.off()
## Reporte Automático
library(DataExplorer)
create_report(data = retornos) ## no funciona
## Missing
library(visdat)
jpeg("Tipo de Dato.jpg")
vis_dat(retornos,sort_type = FALSE)
dev.off()
jpeg("Missing.jpg")
vis_miss(retornos)
dev.off()
## Test de Normalidad
jarqueberaTest(retornos$hipo)
jarqueberaTest(retornos$macro)
Carga de datos para realizar análisis
## Merge de los archivos json recolectados
total_files = 30
datos = data.frame()
for(i in 1:total_files){
  datos1 = jsonlite::stream_in(file(paste0(
    files_path,"dia",i,"_total.json")))
  datos = gtools::smartbind(datos,datos1)
}
## Remover base auxiliar
rm("datos1",total_files,i)
## Consultar cantidad de registros
dim(datos)
## Quedarse con registros únicos
datos = unique(datos)
nrow(datos)
dim(datos)
## Creación de variables vinculadas a las fechas
datos$date <- substr(datos$created_at,1,10)
datos$fecha <- date(datos$created)
datos$date <- day(datos$created)
datos$hour <- hour(datos$created)
## Gráfico menciones por hora de "Santander Río"
search_term = "Santander Río"
by <- 'hour'
grafico <- ts_plot(datos, by = by, trim = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = paste0('Tweets mentioning "',search_term,'" by ',by),
       x = 'Date', y = 'Count',
       caption = 'Source: Twitter API')
## Exportar gráfico como jpg
jpeg("Menciones.jpg")
grafico
dev.off()
# Gráfico Densidad por hora y fecha
x11()
g1 = ggplot(datos, aes(x = date)) +
  geom_density()
g2 = ggplot(datos, aes(x = hour)) +
  geom_density()
grid.arrange(g1,g2)
## Guardar último gráfico
ggsave('tweet_volume.png',last_plot())
## Configuración de dataset para análisis de sentimiento

# Separar el dataset en subconjuntos
tabla = data.frame(cantidad = table(datos$fecha))
names(tabla) <- c("Fecha","Cantidad")
tabla
ggplot(tabla, aes(x = Fecha, y = Cantidad)) +
  geom_point() +
  ggtitle("Cantidad Diaria de Tweets") +
  theme_minimal()
## Consultar tipo de objeto
data.class(datos)
## Partición del dataset en 5 subconjuntos
datos1 <- dplyr::filter(datos, fecha <= "2022-01-08")
datos2 <- dplyr::filter(datos, fecha > "2022-01-08" &
                          fecha <= "2022-01-15")
datos3 <- dplyr::filter(datos, fecha > "2022-01-15" &
                          fecha <= "2022-01-21")
datos4 <- dplyr::filter(datos, fecha > "2022-01-21" &
                          fecha <= "2022-01-28")
datos5 <- dplyr::filter(datos, fecha > "2022-01-28")
## Comprobación de que la partición fue correcta
nrow(datos1) + nrow(datos2) + nrow(datos3) +
  nrow(datos4) + nrow(datos5) == nrow(datos)
Análisis de Sentimiento
sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, value)
# sentiment_dataset[1:10,]
sentiment <- datos[,3:5] %> %
  unnest_tokens(output = 'word', input = 'text')
sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
# Merge - Unión
sentiment <- merge(sentiment,
                   sentiment_dataset,
                   by = 'word')
# Limpieza

entiment$word <- NULL
sentiment$screen_name <- NULL
# get nearest hour of time for plot
sentiment$hour <- format(round(sentiment$created_at,
                               units = "hours"),
                         format = " %H: %M")
str(sentiment)
pivot <- sentiment %> %
  group_by(hour) %> %
  summarise(sentiment = mean(value))
# Gráfico
ggplot(pivot[-1,], aes(x = hour, y = sentiment)) +
  geom_line(group = 1) +
  geom_point() + theme_minimal() +
  labs(title = paste0('Average sentiment of tweetings mentioning "',
                      search_term,'"'),
       subtitle = paste0(pivot$hour[2],' - ',pivot$hour[nrow(pivot)],' on ',
                         format(sentiment$created_at[1], ' %d %B %Y')),
       x = 'Date',
       y = 'Sentiment',
       caption = 'Source: Twitter API')
## Procesamiento para análisis de sentimiento
i = 1
for(i in 1:5){
  datos <- datos1
  ## reemplazar objetos datos1 por cada dataset
  datos$text <- gsub("@[[:alpha:]]*","", datos$text)
  text_corpus <- Corpus(VectorSource(datos$text))
  text_corpus <- tm_map(text_corpus, tolower)
  text_corpus <- tm_map(text_corpus, removeWords,
                        c("santander", "rt", "re", "amp"))
  text_corpus <- tm_map(text_corpus, removeWords,
                        stopwords("spanish"))
  text_corpus <- tm_map(text_corpus, removePunctuation)
  text_df <- data.frame(text_clean = get("content", text_corpus),
                        stringsAsFactors = FALSE)
  datos <- cbind.data.frame(datos, text_df)
  ## Análisis de Sentimiento
  sentiment <- analyzeSentiment(datos$text_clean)
  names(sentiment)
  sentiment <- dplyr::select(sentiment,
                             SentimentGI,
                             SentimentHE,
                             SentimentLM,
                             SentimentQDAP,
                             WordCount)
  sentiment <- dplyr::mutate(sentiment,
                             mean_sentiment = rowMeans(sentiment[,-5]))
  sentiment <- dplyr::select(sentiment,
                             WordCount,
                             mean_sentiment)
  datos <- cbind.data.frame(datos, sentiment)
  datos_negative <- dplyr::filter(datos, mean_sentiment < 0)
  nrow(datos_negative)
  datos_negative %> %
    group_by(fecha) %> %
    summarise(mean = mean(mean_sentiment),
              sum = sum(mean_sentiment),
              median = median(mean_sentiment),
              sd = sd(mean_sentiment))
  ## reemplazar el objeto tabla1 por el nombre adecuado
  tabla1 <- datos_negative %> %
    group_by(fecha) %> %
    summarise(mean = mean(mean_sentiment),
              sum = sum(mean_sentiment),
              median = median(mean_sentiment),
              sd = sd(mean_sentiment))
}
## Se unifica cada una de las tablas con el
## resultado del análisis del sentimiento
table <- rbind(tabla1,tabla2,tabla3,tabla4,tabla5)
## Gráfico de la evolución
ggplot(table, aes(fecha,mean)) +
  geom_line() +
  geom_point() + theme_minimal() +
  ggtitle("Sentimiento Diario",
          subtitle = "Menciones Negativas") +
  xlab("Fecha") + ylab("Promedio")
jpeg("Sentimiento Diario.jpg")

ggplot(table, aes(fecha,mean)) +
  geom_line() +
  geom_point() + theme_minimal() +
  ggtitle("Sentimiento Diario",
          subtitle = "Menciones Negativas") +
  xlab("Fecha") + ylab("Promedio")
dev.off()
jpeg("Sentimiento Diario2.jpg")
ggplot(table, aes(fecha,sum)) +
  geom_line() +
  geom_point() + theme_minimal() +
  ggtitle("Sentimiento Diario",
          subtitle = "Menciones Negativas") +
  xlab("Fecha") + ylab("Total")
dev.off()
jpeg("Sentimiento Diario3.jpg")
ggplot(table, aes(fecha,median)) +
  geom_line() +
  geom_point() + theme_minimal() +
  ggtitle("Sentimiento Diario",
          subtitle = "Menciones Negativas") +
  xlab("Fecha") + ylab("Mediana")
dev.off()
jpeg("Sentimiento Diario4.jpg")
ggplot(table, aes(fecha,sd)) +
  geom_line() +
  geom_point() + theme_minimal() +
  ggtitle("Sentimiento Diario",
          subtitle = "Menciones Negativas") +
  xlab("Fecha") + ylab("Desvío Estándar")
dev.off()
Construcción de modelo econométrico
## Descarga de precios de activos financieros
tickers = c("BRIO.BA","^MERV")
desde = "2021-12-28"
hasta = Sys.Date()
quantmod::getSymbols(tickers,
                     auto.assign = TRUE,
                     src = "yahoo",
                     from = desde,o = hasta,
                     periodicity = "daily")
rio <- as.data.frame(BRIO.BA)
merv <- as.data.frame(MERV)
head(merv)
library(quantmod)
tickers = c("BRIO.BA","MERV")
list <- lapply(tickers, function(x) Ad(get(x)))
precio.cierre <- do.call(merge,list)
retornos <- data.frame(apply(precio.cierre, 2,
                             function(x) Delt(x, type = "log")),
                       fecha = index(precio.cierre)) %> %
  rename(brio = BRIO.BA.Adjusted,
         market = MERV.Adjusted) %> %
  na.omit()
stock_return = retornos$brio
market_return = retornos$market
head(BRIO.BA)
## Días no hábiles de cotización
dates = c("2021-12-29","2021-12-30","2022-01-03",
          "2022-01-04","2022-01-05","2022-01-06","2022-01-07",
          "2022-01-10","2022-01-11","2022-01-12","2022-01-13",
          "2022-01-14","2022-01-17","2022-01-18","2022-01-19",
          "2022-01-20","2022-01-21","2022-01-24","2022-01-25",
          "2022-01-26","2022-01-27","2022-01-28","2022-01-31",
          "2022-02-01","2022-02-02","2022-02-03","2022-02-04",
          "2022-02-07")
dates = as.Date(dates)
# data.class(dates)
## Tabla filtrada con las fechas de cotización de la acción
table_filter = dplyr::filter(table, fecha %in % dates)
sentiment = table_filter$mean
## Análisis de correlación de Pearson
cor(stock_return,market_return)
cor(stock_return,sentiment)
## Prueba de Hipótesis
## Test de Correlación de Pearson (Significatividad)
cor.test(stock_return,market_return)

cor.test(stock_return,sentiment)
cor.test(market_return,sentiment)
## Creación de variables dummies
## Días con más menciones en Twitter
dummies = c(rep(0,21),1,0,1,1,0,0,1)
## Creación de data frame con las variables utilizadas
df <- data.frame(sentiment = sentiment,
                 return = stock_return,
                 market = market_return,
                 dummies = dummies)
# df2 = dplyr::filter(df, dummies == 0)
## Gráfico de Nube de Puntos
jpeg("Nube de Puntos.jpg")
ggplot2::ggplot(df, aes(x = sentiment, y = return)) +
  geom_point(color='blue') +
  theme_minimal() + xlab("Sentiment") + ylab("Return") +
  ggtitle("Gráfico de Dispersión",subtitle = "Sentiment - Return")
dev.off()
## Gráfico de Modelo Lineal
jpeg("Modelo Estimado.jpg")
ggplot2::ggplot(df, aes(x = sentiment, y = return)) +
  geom_point(color='blue') +
  theme_minimal() + xlab("Sentiment") + ylab("Return") +
  ggtitle("Modelo Lineal",subtitle = "Sentiment - Return") +
  geom_smooth(method = "lm", se = FALSE, color = "red")
dev.off()
## Modelos entrenados
modelo_1 <- lm(stock_return ~ market_return)
summary(modelo_1)
modelo_2 <- lm(stock_return ~ sentiment + market_return)
summary(modelo_2)
modelo_3 <- lm(stock_return ~ sentiment)
summary(modelo_3)
modelo_4 <- glm(stock_return ~ sentiment + dummies)
summary(modelo_4)
modelo_5 <- lm(stock_return ~ sentiment + dummies)
summary(modelo_5)

## Para tabla en Latex
stargazer::stargazer(modelo_1,modelo_2,modelo_3)
  