library(rtweet)
library(tidyverse)
library(tidytext)

#Buscar os dados sobre os tuítes que citam datafolha. Dados coletados em 25/06/2022

tw_datafolha<- rtweet::search_tweets(q="datafolha", n=18000, include_rts = FALSE)

tw_datafolha%>%
  readr::write_csv("tw_datafolha.csv")


tw_datafolha %>%
  mutate(data_publicacao = lubridate::as_date(created_at)) %>%
  ggplot() +
  geom_bar(aes(x=data_publicacao)) +
  theme_light() +
  labs(
    x=NULL,
    y="Número de tweets"
  )

#Criei essa função para buscar as hashtags distintas. Procure-me na DM do twitter se quiser ter acesso
hashtags_datafolha<- analise_hashtags(tw_datafolha)

hashtags_datafolha %>%
  dplyr::slice_max(order_by = quantidade, n=20) %>%
  mutate(hashtag= reorder(hashtag, quantidade)) %>%
  ggplot() +
  geom_col(aes(y=hashtag, x=quantidade))+
  theme_light() +
  labs(
    x="Número de tweets"
  )



tidy_twitter <- 
  tw_datafolha %>%
  unnest_tokens(word, text, token = "tweets", strip_punct = TRUE, strip_url = TRUE ) %>%
  #filter(str_detect(word, "@", negate = TRUE)) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

#Stop words

library(stopwords) 

library(tibble)

stopword <- as_tibble(stopwords::stopwords("pt")) 
stopword <- rename(stopword, word=value)
stopwords_full <- c(stopword)
tb <- anti_join(tidy_twitter, stopword, by = 'word')

newstopwords <- tibble(word = c("rt","https", "t.co", "é", "t","n","q","vc","h", "pra","via","ñ","vai"))

stop_words_users<- 
  tidy_twitter %>%
  filter(str_detect(word, "@")) %>%
  select(word)

stop_words_users_sem_arroba<- 
  tidy_twitter %>%
  filter(str_detect(word, "@")) %>%
  mutate(word=str_sub(word,2,200)) %>%
  select(word)


stopwords_full <- bind_rows (stopword, newstopwords, stop_words_users, stop_words_users_sem_arroba)


###########Term frequency (tf)

tb <- anti_join(tb, stopwords_full, by = 'word')

word_count <- count(tb, word, sort = TRUE)



tb %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  labs(title="Most frequent words after removing stop words", 
       subtitle="Top 20 words by month",
       x= NULL, 
       y= "Word Count")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer()   


#bigrams

tuites_bigrams <-   tw_datafolha %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)



twittes_bigrams_count <- tuites_bigrams %>% 
  count(bigram, sort = TRUE)

library(tidyr)

# seperate words
bigrams_separated <- tuites_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words and NA
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords_full$word) %>%
  filter(!word2 %in% stopwords_full$word) %>% 
  filter(!is.na(word1))


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)



##trigrams
tuites_trigrams <-   tw_datafolha %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)



tuites_trigrams_count <- tuites_trigrams %>% 
  count(trigram, sort = TRUE)

library(tidyr)

# seperate words
trigrams_separated <- tuites_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

# filter stop words and NA
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stopwords_full$word) %>%
  filter(!word2 %in% stopwords_full$word) %>% 
  filter(!word3 %in% stopwords_full$word) %>% 
  filter(!is.na(word1))


# new trigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)


##########################Network analysis


#### Para todos os meses
library(igraph)

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(123)


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()



# filter for only relatively common combinations
trigram_graph <- trigram_counts %>%
  filter(n > 25) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(13)



  ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name),size =2.5, vjust = 1, hjust = 1)+
  theme_void()


#769x583
ggsave("grafo_trigam.jpg", plot = grafo_trigram, width=180, height = 140, units = "mm")

#Análise com correlação

analise_twitter_secoes <- tibble(texto= tw_datafolha$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stopwords_full)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 50) %>%
  widyr::pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .60) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "mds") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#Palavras mais frequentes para #lulanoprimeiroturno
lula_primeiro_turno<-filter_hashtags(tw_datafolha, "lulanoprimeiroturno")

tidy_lula_primeiro_turno <- 
  tw_datafolha %>%
  filter_hashtags("lulanoprimeiroturno") %>%
  unnest_tokens(word, text, token = "tweets", strip_punct = TRUE, strip_url = TRUE ) %>%
  #filter(str_detect(word, "@", negate = TRUE)) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tb_lula_primeiro_truno <- anti_join(tidy_lula_primeiro_turno, 
                                    stopwords_full, by = 'word')

word_count_lula_primeiro_turno <- count(tb_lula_primeiro_truno, word, sort = TRUE)



tb_lula_primeiro_truno %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  labs(title="Most frequent words after removing stop words", 
       subtitle="Top 20 words by month",
       x= NULL, 
       y= "Word Count")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer()   




#Palavras mais frequentes para #bolsonaronoprimeiroturno

tidy_bolsonaro_primeiro_turno <- 
  tw_datafolha %>%
  filter_hashtags("bolsonaronoprimeiroturno") %>%
  unnest_tokens(word, text, token = "tweets", strip_punct = TRUE, strip_url = TRUE ) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tb_bolsonaro_primeiro_truno <- anti_join(tidy_bolsonaro_primeiro_turno, 
                                    stopwords_full, by = 'word')

tb_bolsonaro_primeiro_truno %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(y=word, x=n)) +
  geom_col() +
  theme_light() +
  theme() 


#Análise conjunta #lulanoprimeiroturno #bolsonaronoprimeiroturno

tidy_primeiro_turno <- 
  tw_datafolha %>%
  filter_hashtags("lulanoprimeiroturno") %>%
  mutate(grupo = "lula") %>%
  dplyr::bind_rows(
    tw_datafolha %>%
      filter_hashtags("bolsonaronoprimeiroturno") %>%
      mutate(grupo = "bolsonaro")
  ) %>% 
  unnest_tokens(word, text, token = "tweets", strip_punct = TRUE, strip_url = TRUE ) %>%
  #filter(str_detect(word, "@", negate = TRUE)) %>%
  group_by(grupo, word) %>%
  filter(n() > 10) %>%
  ungroup()

tb_primeiro_truno <- anti_join(tidy_primeiro_turno, 
                                    stopwords_full, by = 'word')

word_count_lula_primeiro_turno <- count(tb_lula_primeiro_truno, word, sort = TRUE)


library(viridis)

tb_primeiro_truno %>%
  count(grupo, word, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(y=word, x=n, fill=grupo)) +
  geom_col(show.legend = TRUE) +
  theme_light() +
  theme() +
  scale_fill_viridis(discrete = TRUE)   

tb_primeiro_truno %>%
  group_by(grupo) %>%
  summarise(
    `soma retweets` = sum(retweet_count),
    `soma favoritos` = sum(favorite_count)
  ) %>%
  tidyr::pivot_longer(cols = c(`soma retweets`, `soma favoritos`), names_to = "repercussao", values_to = "valor") %>%
  ggplot() +
  geom_col(aes(y=repercussao, x=valor))+
  theme_light() +
  theme() +
  facet_grid(grupo~.) +
  scale_x_continuous(labels = function(x){format(x, big.mark = ".",, scientific = FALSE)})+
  labs(x=NULL,
       y=NULL)
  
  scale_fill_viridis(discrete = TRUE)   
