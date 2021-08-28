##################################
# Librerías

library(tidyverse)
library(tidytext)
library(stringr)
library(gutenbergr)
library(scales)
library(textdata)
library(wordcloud)
library(reshape2)
library(dplyr)

##################################
# Biblia

# Biblia raw

biblia <- gutenberg_download(c(30))

# Biblia tokenizada

biblia <- biblia %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) 

# Limpiar

biblia <- subset(biblia, select = c(-chapter,-gutenberg_id)) # Remover id y capitulo

biblia <- biblia %>% 
  mutate(word = str_extract(word, "[a-z']+")) # Remover números

biblia <- subset(biblia, word != "NA") # Remover los NA

# Ver palabras que más se utilizan

commonWords <- biblia %>%
  count(word, sort = TRUE)


# Sentimientos de la biblia con bing

biblia_sentiment <- biblia %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, 
        sentiment) %>%
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Arco narrativo de la biblia según el sentimiento

biblia_sentiment %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill="lightblue",color="black"),show.legend = FALSE) +
  labs(y = "Sentimiento", x = "Indice") +
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Las 10 palabras (positivas y negativas) 

bilia_word_top <- biblia %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bilia_word_top %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Nube de palabras con división de sentimiento

biblia %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

##################################
# Mormon

#  raw

mormon <- gutenberg_download(c(17))

# tokenizar

mormon <- mormon %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) 

# Limpiar

mormon <- subset(mormon, select = c(-chapter,-gutenberg_id)) # Remover id y capitulo

mormon <- mormon %>%
  mutate(word = str_extract(word, "[a-z']+")) # Remover números

mormon <- subset(mormon, word != "NA") # Remover los NA

# Ver palabras que más se utilizan

mormon %>%
  count(word, sort = TRUE)

# Sentimientos del libro mormón con bing

mormon_sentiment <- mormon %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, 
        sentiment) %>%
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Arco narrativo del libro mormón según el sentimiento

mormon_sentiment %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill="lightblue",color="black"),show.legend = FALSE) +
  labs(y = "Sentimiento", x = "Indice") +
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Las 10 palabras (positivas y negativas) 

mormon_word_top <- mormon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

mormon_word_top %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Nube de palabras con división de sentimiento

mormon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

##################################
# Dhanmmapada

#  raw

dhanmmapada <- gutenberg_download(c(35185))

# tokenizar

dhanmmapada <- dhanmmapada %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) 

# Limpiar

dhanmmapada <- subset(dhanmmapada, select = c(-chapter,-gutenberg_id)) # Remover id y capitulo

dhanmmapada <- dhanmmapada %>%
  mutate(word = str_extract(word, "[a-z']+")) # Remover números

dhanmmapada <- subset(dhanmmapada, word != "NA") # Remover los NA


# Ver palabras que más se utilizan

dhanmmapada %>%
  count(word, sort = TRUE)

# Sentimientos del Dhanmmapada con bing

dhanmmapada_sentiment <- dhanmmapada %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, 
        sentiment) %>%
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Arco narrativo del Dhanmmapada según el sentimiento

dhanmmapada_sentiment %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill="lightblue",color="black"),show.legend = FALSE) +
  labs(y = "Sentimiento", x = "Indice") +
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Las 10 palabras (positivas y negativas) 

dhanmmapada_word_top <- dhanmmapada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

dhanmmapada_word_top %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Nube de palabras con división de sentimiento

dhanmmapada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

##################################
# Quran

#  raw

quran <- gutenberg_download(c(7440))

# tokenizar

quran <- quran %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) 

# Limpiar

quran <- subset(quran, select = c(-chapter,-gutenberg_id)) # Remover id y capitulo

quran <- quran %>%
  mutate(word = str_extract(word, "[a-z']+")) # Remover números

quran <- subset(quran, word != "NA") # Remover los NA


# Ver palabras que más se utilizan

quran %>%
  count(word, sort = TRUE)

# Sentimientos del quran con bing

quran_sentiment <- quran %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, 
        sentiment) %>%
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Arco narrativo del quran según el sentimiento

quran_sentiment %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill="lightblue",color="black"),show.legend = FALSE) +
  labs(y = "Sentimiento", x = "Indice") +
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Las 10 palabras (positivas y negativas) 

quran_word_top <- quran %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

quran_word_top %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Nube de palabras con división de sentimiento

quran %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

##################################


#Calcular la frecuencia de cada palabra

frequency <- bind_rows(mutate(biblia, libro = "The Bible"),
                       mutate(dhanmmapada, libro = "Dhammapada"), 
                       mutate(mormon, libro = "The Book of Mormon"),
                       mutate(quran, libro = "The Quran")) %>% 
  count(libro, word) %>%
  group_by(libro) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = libro, values_from = proportion)
