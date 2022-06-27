#Função que gera um dataframe com as hashtags associadas aos twittes onde aparecem os candidatos
analise_hashtags<- function(.data, n=NULL){
  
  library(purrr)
  
  hashtags<- .data$hashtags
  
  
  keywords<-
    map_dfr(1:length(hashtags), function(inc){
      
      #print(inc)
      if ( !is.na(hashtags[[inc]]) ){
        #print(hashtags[[inc]])
        map_dfr(1:NROW(hashtags[[inc]]), function(inc_int){
          #print(hashtags[[inc]][inc_int])
          tibble(hashtag= hashtags[[inc]][inc_int])
          
        })
      }
    })
  
  
  analise_keywords<-
    keywords %>%
    mutate(hashtag= str_to_lower(hashtag)) %>%
    group_by(hashtag) %>%
    summarise(
      quantidade = n()
    )
  
  if (!is.null(n)){
    
    analise_keywords<-
      analise_keywords %>%
      slice_max(quantidade, n=n)
    
    
  }
  
  .data<-analise_keywords
  
  .data 
  
}


filter_hashtags<- function(.data, filtro){
  
  library(purrr)
  library(abjutils)
  filtro <- abjutils::rm_accent(stringr::str_to_lower(filtro))
  
  
  hashtags<- .data$hashtags
  
  
  
  map_dfr(1:length(hashtags), function(inc){
    
    if ( !is.na(hashtags[[inc]]) ){
      map_dfr(1:NROW(hashtags[[inc]]), function(inc_int){
        #print(hashtags[[inc]][inc_int])
        
        if (abjutils::rm_accent(stringr::str_to_lower(hashtags[[inc]][inc_int])) ==filtro){

          .data[inc,]
        }
        
        
      })
    }
  })
  
  
  
}



#associação de mensagens positivas, negativas e neutras aos candidatos a uma amostra das mensagens

associa_tipo_mensagem <- function(.data, n, tags_positivas, tags_negativas, usuarios_noticias=NULL, tipo=1, nome_arquivo=NULL, delim=",", seed=1972){
  
  
  .data<-
    .data %>%
    mutate(tipo_mensagem = case_when(
      tolower(hashtags) %in% tags_positivas ~ "positivo",
      tolower(hashtags) %in% tags_negativas ~ "negativo",
      screen_name %in% usuarios_noticias ~ "noticia"
    )) %>%
    select(status_id, screen_name, text,tipo_mensagem, hashtags)
  
  set.seed(seed)
  
  if (tipo == 1){
    
    .data<-
      .data %>%
      slice_sample(n=n)
    
    #seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
    
    
  } else{
    
    #seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
    .data<-
      .data %>%
      filter(!is.na(tipo_mensagem)) %>%
      slice_sample(n=n)
  }
  
  
  if (!is.null(nome_arquivo)){
    .data%>%
      readr::write_delim(nome_arquivo, delim = delim,  )
  }
  
  .data
  
}








#################Processamento de tweets

processa_tweets<- function(.data){
  .data<- 
    .data %>%
    select(status_id, screen_name, tipo_mensagem, text) %>%
    unnest_tweets(word,text, strip_url = TRUE)%>%
    dplyr::filter(!str_detect(word,"#"),
                  !str_detect(word,"@"),
                  !str_detect(word, "[^[:graph:][:space:]]"))
  
  .data$word<- abjutils::rm_accent(.data$word)
  
  palavras_unicas_usuario<- 
    .data %>%
    group_by(screen_name, word) %>%
    summarise(
      quantidade = n()
    ) %>%
    filter(quantidade == 1) %>%
    select(screen_name, word) %>%
    ungroup()
  
  .data<-   
    .data %>%
    inner_join(palavras_unicas_usuario)
  
  library(stopwords) 
  
  library(tibble)
  
  stopword <- as_tibble(stopwords::stopwords("pt")) 
  stopword <- rename(stopword, word=value)
  .data <- anti_join(.data, stopword, by = 'word')
  
  .data
  
}


###########Term frequency (tf)

frequencia_termos<-function(.data){
  
  
  word_count <- count(.data, word, sort = TRUE)
  
  tipo_count <-  .data %>% 
    count(tipo_mensagem, word, sort = TRUE)
  
  .data<- tipo_count 
  .data
  
  
}



########## Term frequency and inverse document frequency (tf-idf)

tabela_tf_idf<-  function(.data){
  
  library(forcats)
  
  tabela <- .data %>%
    count(tipo_mensagem, word, sort = TRUE) %>%
    bind_tf_idf(word, tipo_mensagem, n) %>%
    mutate(word = fct_reorder(word, tf_idf)) %>%
    mutate(tipo_mensagem = factor(tipo_mensagem, 
                                  levels = c("negativo",
                                             "positivo",
                                             "noticias")))
  
  graph<-
    tabela %>% 
    group_by(tipo_mensagem) %>% 
    top_n(15, tf_idf) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = tipo_mensagem)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~tipo_mensagem, ncol = 3, scales = "free") +
    coord_flip() +
    theme_classic(base_size = 12) +
    labs(fill= "Tipo Mensagem", 
         x= NULL, 
         y= NULL) +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          axis.title = element_blank(),
          axis.text.x = element_blank()) +
    scale_fill_viridis(discrete = TRUE)  
  
  list(tabela= tabela,graph= graph )
  
  
  
  
}





### Preparação das base de ML
prepara_base_ml<-  function(.data){
  
  
  library(caret)
  library(tidyr)
  
  pml_train_trabalho<-
    .data %>%
    group_by(status_id, tipo_mensagem, word) %>%
    summarise(
      quantidade = n()
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = word, values_from = quantidade)
  
  
  pml_train_trabalho[is.na(pml_train_trabalho)]<-0  
  
  library(janitor)
  
  pml_train_model <- pml_train_trabalho[,-1]
  
  status_id_model<-  pml_train_trabalho$status_id
  
  colnames(pml_train_model) <- make.names(colnames(pml_train_model))
  #pml_train_model<- janitor::clean_names(pml_train_model)
  
  pml_train_model<- pml_train_model[,substr(names(pml_train_model),1,2)!="X."]
  
  .data= list(status_id_model= status_id_model , pml_train_model = pml_train_model)
  
}


########### decision tree para todos os elementos

cria_arvore_decisao<- function(.data, seed= 1972){
  if (class(.data)=="list"){
    .data <- .data$pml_train_model
  }
  
  control_dt <- trainControl(method="cv")
  
  set.seed(seed)
  
  
  dt_model <- train(tipo_mensagem~., data=.data, method="rpart",  trControl=control_dt)
  
  dt_model
  
}


########### gráfico da decision tree para todos os elementos
grafico_arvore_decisao<-function(.data){
  
  library(rattle)
  fancyRpartPlot(.data$finalModel)
  
}





