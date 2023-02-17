#Wine KNN classification testing
library(tidyverse)
library(formatR)
library(moderndive)
library(caret)
library(class)
library(fastDummies)
library(tidytext)
library(ggalluvial)

data(stop_words)

regex_ify<-function(x){
  x<-x[!is.na(x)]
  step1<-paste0("[ |^]",x,"[| |$]")
  step2<-paste0(step1,collapse="|")
  return(step2)
}

wine = read_rds("~/Desktop/MSDS/505-ML/resources/pinot.rds")

# wine = read_rds("C:\\Users\\karyn\\OneDrive\\Documents\\MSDS\\505 - Machine Learning\\ML Homework\\pinot.rds")

all_description_words<-data.frame(stringr::str_split(wine$description," ",simplify=T),province=wine$province,id=wine$id) %>% 
  pivot_longer(c(-province,-id),values_to="word") %>% 
  mutate(word=tolower(word),
         word=gsub("[[:punct:]]","",word)
         ) %>% 
  filter(word!="")

all_description_words = all_description_words %>%
  anti_join(stop_words)

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

ca_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="California"&some_metrics$wines>=1][1:2500]
or_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Oregon"&some_metrics$wines>=1][1:2500]
ny_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="New_York"&some_metrics$wines>=1]
ma_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Marlborough"&some_metrics$wines>=1]
cv_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Casablanca_Valley"&some_metrics$wines>=1]
bu_words<-some_metrics$word[some_metrics$metric_4>=1&some_metrics$province=="Burgundy"&some_metrics$wines>=1]
ca_words2<-some_metrics$word[some_metrics$metric_4>=.8&some_metrics$metric_4<1&some_metrics$province=="California"&some_metrics$wines>=1]
or_words2<-some_metrics$word[some_metrics$metric_4>=.7&some_metrics$metric_4<1&some_metrics$province=="Oregon"&some_metrics$wines>=1]
ny_words2<-some_metrics$word[some_metrics$metric_4>=.3&some_metrics$metric_4<1&some_metrics$province=="New_York"&some_metrics$wines>=1]
ma_words2<-some_metrics$word[some_metrics$metric_4>=.3&some_metrics$metric_4<1&some_metrics$province=="Marlborough"&some_metrics$wines>=1]
cv_words2<-some_metrics$word[some_metrics$metric_4>=.3&some_metrics$metric_4<1&some_metrics$province=="Casablanca_Valley"&some_metrics$wines>=1]
bu_words2<-some_metrics$word[some_metrics$metric_4>=.7&some_metrics$metric_4<1&some_metrics$province=="Burgundy"&some_metrics$wines>=1]

# some_metrics$word[some_metrics$province=="California"]
# some_metrics %>% 
#   filter(province=="California"&metric_4==1&wines>10) %>% 
#   View()

wino<-wine %>% 
  ungroup() %>% 
  mutate(
    lprice = log(price),
    points = points,
    f_1=grepl(regex_ify(ca_words),description,ignore.case = T),
    f_2=grepl(regex_ify(or_words),description,ignore.case = T),
    f_3=grepl(regex_ify(ny_words),description,ignore.case = T),
    f_4=grepl(regex_ify(ma_words),description,ignore.case = T),
    f_5=grepl(regex_ify(cv_words),description,ignore.case = T),
    f_6=grepl(regex_ify(bu_words),description,ignore.case = T),
    f_1b=grepl(regex_ify(ca_words2),description,ignore.case = T),
    f_2b=grepl(regex_ify(or_words2),description,ignore.case = T),
    f_3b=grepl(regex_ify(ny_words2),description,ignore.case = T),
    f_4b=grepl(regex_ify(ma_words2),description,ignore.case = T),
    f_5b=grepl(regex_ify(cv_words2),description,ignore.case = T),
    f_6b=grepl(regex_ify(bu_words2),description,ignore.case = T)
    ) %>% 
  select(province, lprice, points, year, f_1:f_6b)

wino = wino %>% 
  preProcess(method = c("BoxCox","center","scale")) %>% 
  predict(wino)


set.seed(504)
wine_index <- createDataPartition(wino$province, p = 0.8, list = FALSE)
train <- wino[ wine_index, ]
test <- wino[-wine_index, ]

# fit <- knn(
#   train = select(train,-province), 
#   test = select(test,-province), 
#   k=5, 
#   cl = train$province, 
#   prob = T)

# fit <- train(province ~ .,
#              data = train, 
#              method = "naive_bayes",
#              tuneGrid = expand.grid(usekernel = c(T,F), laplace = T, adjust = T),
#              metric = "Kappa",
#              trControl = trainControl(method = "cv"))

control <- trainControl(method = "repeatedcv", number = 5)

#### KNN ####
# fit <- train(province ~ .,
#              data = train,
#              method = "knn",
#              tuneLength = 15,
#              trControl = control)

# conf<-confusionMatrix(fit$finalModel,factor(test$province))
# kappa<-conf$overall[2]

#### RF ####
fit <- train(province ~ .,
             data = train,
             method = "rf",
             tuneLength = 5,
             trControl = control)

fit

pred <- predict(fit, newdata=test)
conf<-confusionMatrix(factor(pred),factor(test$province))

kappa<-conf$overall[2]

alluvial_data<-conf$table %>%
  data.frame() %>% 
  mutate(
    Prediction=gsub("_"," ",Prediction,fixed = T),
    Reference=gsub("_"," ",Reference,fixed = T)
  )

alluvial_summary<-alluvial_data %>% 
  group_by(Reference) %>% 
  summarise(n=sum(Freq)) %>% 
  arrange(-n)

alluvial_data$Reference<-factor(alluvial_data$Reference,levels = alluvial_summary$Reference)
alluvial_data$Prediction<-factor(alluvial_data$Prediction,levels = alluvial_summary$Reference)

alluvial_data<-alluvial_data %>% 
  group_by(Prediction) %>%
  mutate(to_n=sum(Freq)) %>% 
  group_by(Reference) %>% 
  mutate(from_n=sum(Freq)) %>% 
  arrange(Reference,Prediction) %>% 
  mutate(
    n2=paste0("(",round(Freq/from_n*100),"%)"),
    n3=ifelse(Reference==Prediction,paste(Freq,n2),NA)
    ) %>% 
  as.data.frame()

is_alluvia_form(alluvial_data)

ggplot(alluvial_data,
       aes(y = Freq)) +
  geom_alluvium(width = 1/12,aes(fill=Reference, axis2 = Prediction, axis1 = Reference)) +
  geom_stratum(data=alluvial_data %>% ungroup(),width = 1/12, color = "black",aes(fill=Prediction,axis1=Reference,axis2=Prediction)) +
  geom_stratum(data=alluvial_data %>% ungroup() %>% select(-Prediction),width = 1/12, color = "black",aes(fill=Reference,axis2=Reference))+
  geom_text(stat = "stratum", aes(axis1="",axis2=Prediction,label = after_stat(stratum)),nudge_x = .05,hjust=0)+
  geom_text(stat = "stratum", aes(axis1=Reference,label = after_stat(stratum)),nudge_x = -.05,hjust=1)+
  geom_text(stat = "stratum", aes(axis1="",axis2=Prediction,label = (to_n)))+
  geom_text(stat = "stratum", aes(axis1=Reference,label = (from_n)))+
  # geom_text(stat = "alluvium", aes(axis1=Prediction,label = (Freq)),nudge_x = .075,size=2)+
  geom_text(stat = "alluvium", aes(axis1=Prediction,label = (n3)),nudge_x = 0.07,size=2)+
  labs(y=NULL)+
  scale_fill_brewer(type="qual",palette =2)+
  theme_classic()+
  theme(
    text = element_text(size=16),
    panel.grid.major = element_line(color="gray84",size = .25),
    # axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.25), "cm"),
    legend.position = "bottom",
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.y = element_blank()
  )+
  guides(fill=F)+
  scale_x_continuous(breaks = c(1,2),labels = c("Actual","Predicted"),expand = c(.15,.15))+
  scale_y_continuous(expand = c(0,0),breaks = c(0,sum(alluvial_data$Freq)))

