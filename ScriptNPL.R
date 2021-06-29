
#Paquetes---------------------------------------------------------------------------

install.packages("textdata")
install.packages("naivebayes")
install.packages("randomForest")
install.packages("caret")
install.packages("prob")
install.packages("naniar")

#/Paquetes---------------------------------------------------------------------------

#Librerias---------------------------------------------------------------------------
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(readr)
library(textdata)
library(naivebayes)
library(caret)
library(prob)
library(randomForest)
library(naniar)

#/Librerias---------------------------------------------------------------------------

#Datos--------------------------------------------------------------------------------


AmazonReviewsDS <- read_csv("AmazonReviewsDS.csv")
View(AmazonReviewsDS)
summary(AmazonReviewsDS)

#Listado de palabras
palabras <- AmazonReviewsDS %>%
  select(c("id","name","reviews.rating", "reviews.text")) %>%
  unnest_tokens(word, reviews.text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))


#Analisis sentimental AFINN

afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
afinn <- data.frame(afinn)
reviews.afinn <- palabras %>%
  inner_join(afinn, by = "word")


#Palabras mas comunes
word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(reviews.rating), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#Grafico con las palabras mas comunes segun su rating
ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue")+ guides(size = FALSE, color=FALSE)+ coord_cartesian(xlim=c(3.5,5)) 

#Nube de palabras mas frecuentes
wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set2"))

#Nube de palabras positivas
good <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(reviews.rating), score = max(value), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#Nube de palabras negativas----------------------No es util
bad <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(reviews.rating), score = max(value), count_word = n()) %>%
  filter(count_word>1000) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#Reviews por producto
review_summary <- reviews.afinn %>%
  group_by(id) %>%
  summarise(mean_rating = mean(reviews.rating), sentiment = mean(value))
datatable(head(review_summary))

#Grafica del sentimiento del producto
y_mid = 0
x_mid = 3.5

review_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Valoracion Positiva/Sentimiento positivo",
                              mean_rating <= x_mid & sentiment > y_mid  ~ "Valoracion Negativa/Sentimiento positivo",
                              mean_rating <= x_mid & sentiment <= y_mid ~ "Valoracion Negativa/Sentimiento Negativo",
                              TRUE                                      ~ "Valoracion Positiva/Sentimiento Negativo")) %>% 
  ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.5) + 
  geom_vline(xintercept=x_mid, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("lightgreen", "pink", "pink","lightgreen")) +
  ggtitle("Valoracion Producto Amazon vs Valoracion Sentimental del comentario") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Valoracion Positiva/Sentimiento positivo") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Valoracion Negativa/Sentimiento positivo") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Valoracion Negativa/Sentimiento Negativo") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Valoracion Positiva/Sentimiento Negativo") +
  geom_point()


#Bayes

AmazonReviewsDS2 <- AmazonReviewsDS[,c("id","name","reviews.rating", "reviews.text")]
amazon_df <- mutate(AmazonReviewsDS2, reviews.rating = as.factor(reviews.rating))

amazon_df %>%
  unnest_tokens(input = "reviews.text", output = "palabra") %>%
  count(name, palabra) %>%
  spread(key = palabra, value = n)

crear_matriz <- function(tabla) {
  tabla %>%
    unnest_tokens(input = "reviews.text", output = "palabra") %>%
    count(name, id, palabra) %>%
    spread(key = palabra, value = n) %>%
    select(-id)
}


ejemplo_matriz <-
  amazon_df %>%
  mutate(name = ifelse(name == "AmazonBasics AAA Performance Alkaline Batteries (36 Count)", name, "Otro"),
         name = as.factor(name)) %>%
  crear_matriz

#matriz <- crear_matriz(amazon_df)

smp_size <- floor(0.7 * nrow(amazon_df))
set.seed(2000)
train_index <- sample(seq_len(nrow(amazon_df)), size = smp_size)

train <- mdisp_df_known[train_index, ]
test <- mdisp_df_known[-train_index, ]

ejemplo_modelo <- naive_bayes(formula = overall ~ .,  data = train)

model1_prediction <- predict(ejemplo_modelo, test)
head(model1_prediction, 25)
confusionMatrix(model1_prediction, test[["overall"]])

model2 <- randomForest(formula = overall ~ ., data = train)
model2_prediction <- predict(model2, test)
model2_prediction
confusionMatrix(model2_prediction, test[["overall"]])
#/Datos--------------------------------------------------------------------------------

