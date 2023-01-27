#Wine KNN classification testing
library(tidyverse)
library(formatR)
library(moderndive)
library(caret)
library(class)
library(fastDummies)

regex_ify<-function(x){
  x<-x[!is.na(x)]
  step1<-paste0("[ |^]",x,"[| |$]")
  step2<-paste0(step1,collapse="|")
  return(step2)
}

wine = read_rds("resources/pinot.rds")

all_description_words<-data.frame(stringr::str_split(wine$description," ",simplify=T),province=wine$province,id=wine$id) %>% 
  pivot_longer(c(-province,-id),values_to="word") %>% 
  mutate(word=tolower(word),
         word=gsub("[[:punct:]]","",word)
         ) %>% 
  filter(word!="")

word_summary_province<-all_description_words %>% 
  group_by(province) %>% 
  summarise(
    total_words_in_prov=n(),
    unique_words_in_prov=n_distinct(word),
    total_wines_in_prov=n_distinct(id),
  )

some_metrics<-all_description_words %>% 
  group_by(word,province) %>% 
  summarise(
    count=n(),
    wines=n_distinct(id)
  ) %>% 
  left_join(
    word_summary_province
  ) %>% 
  mutate(
    metric_1=count/total_words_in_prov, #frequency that word appears relative to all words in province
    metric_2=wines/total_wines_in_prov #frequency that wine with that word appear relative to all wines in province
  ) %>% 
  group_by(word) %>% 
  mutate(
    metric_3=metric_2/sum(metric_2), #relative frequency that word appears in province relative to other provinces
    metric_4=wines/sum(wines) #if you used this word to guess this wine's province how often would you be right
  )

ca_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="California"&some_metrics$wines>=10]
or_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Oregon"&some_metrics$wines>=10]
ny_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="New_York"&some_metrics$wines>=5]
ma_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Marlborough"&some_metrics$wines>=5]
cv_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Casablanca_Valley"&some_metrics$wines>=2]
bu_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Burgundy"&some_metrics$wines>=2]

wino<-wine %>% 
  ungroup() %>% 
  mutate(
    price=log(price),
    f_1=grepl(regex_ify(ca_words),description,ignore.case = T),
    f_2=grepl(regex_ify(or_words),description,ignore.case = T),
    f_3=grepl(regex_ify(ny_words),description,ignore.case = T),
    f_4=grepl(regex_ify(ma_words),description,ignore.case = T),
    f_5=grepl(regex_ify(cv_words),description,ignore.case = T),
    f_6=grepl(regex_ify(bu_words),description,ignore.case = T)
    ) %>% 
  select(province,price,f_1:f_6)

set.seed(504)
wine_index <- createDataPartition(wino$province, p = 0.8, list = FALSE)
train <- wino[ wine_index, ]
test <- wino[-wine_index, ]

fit <- knn(
  train = select(train,-province), 
  test = select(test,-province), 
  k=5, 
  cl = train$province, 
  prob = T)

conf<-confusionMatrix(fit,factor(test$province))
kappa<-conf$overall[2]
