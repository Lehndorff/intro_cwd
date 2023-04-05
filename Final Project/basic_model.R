library(tidyverse)
library(caret)
library(pROC)
library(MLmetrics)
library(randomForest)

tourney_data <- read_csv("intro_cwd/Final Project/tourney_data.csv") %>% 
  mutate(rank=log2(rank)) %>% # converts 1,2,4,8,16, etc to 0,1,2,3,4
  select(-Season,-class,-`Coach(es)`,-`NCAA Tournament`) %>% #either not helpful, or duplicative
  filter(!is.na(Pts_for)) %>% #removes 1 case
  mutate(across("AP Pre":"AP Final",function(x){ifelse(is.na(x),"Not 25",paste("Top",ceiling(x/5)*5))})) %>% #data cleaning
  mutate(across(Conf_W:Conf_Pct,function(x){ifelse(is.na(x),0,x)})) #%>% #fill missing info

skimr::skim(tourney_data)

tourney_data2<-tourney_data %>%
  fastDummies::dummy_cols(remove_selected_columns = T) %>% #simple dummy cols
  arrange(tourn_year) %>% mutate(lag_rank=lag(rank)) %>% mutate(lag_rank=ifelse(is.na(lag_rank),6,lag_rank)) %>% #how did the team do last year
  mutate(rank=as.numeric(rank<=4))# <=4 = Did team make the sweet 16 (one of the final 2^4 teams)
table(tourney_data2$rank)

test2<-tourney_data2 %>% 
  filter(tourn_year==2023)

tourney_data2<-tourney_data2 %>% 
  filter(tourn_year!=2023)
# ctrl <- trainControl(method = "cv", number = 5)
set.seed(504) 

# tourney_index <- createDataPartition(tourney_data2$rank, p = 0.80, list = FALSE)
# train <- tourney_data2[ tourney_index, ]
# test <- tourney_data2[-tourney_index, ]

train<-tourney_data2 %>% 
  group_by(rank) %>% 
  sample_frac(.8)
test<-tourney_data2 %>% 
  anti_join(train)

control <- trainControl(method = "boot", number = 1)
fit_rf <- train(rank ~ .,
             data = train,
             method = "rf",
             trControl = control)

plot(varImp(fit_rf),top=10)

test3=test
test=test2
pred <- predict(fit_rf, newdata=test)

data.frame(test=test$rank,pred=pred) %>% 
  ggplot()+
  geom_density(aes(x=pred,fill=factor(test),group=test),alpha=.5)+
  theme(legend.position = "bottom")+
  labs(x="Prediction",fill="Actual")

#A function for determining the best threshold for cutoff (needed for regression not categorization)
best_thresh<-function(x){
  pred2<-as.numeric(pred>x)
  output<-as.numeric(confusionMatrix(factor(pred2),factor(test$rank))$overall[2])
  return(output)
}

cutoff<-data.frame(thresh=seq(0,1,.01)) %>% 
  mutate(best=sapply(thresh,best_thresh)) %>% 
  filter(best==max(best)) %>% 
  pull(thresh)
pred2<-as.numeric(pred>cutoff)

confusionMatrix(factor(pred2),factor(test$rank))

test$pred=pred2

zzz<-tourney_data %>% 
  filter(tourn_year==2023) %>% 
  mutate(rank=as.numeric(rank<=4)) %>% 
  # mutate(pred=pred) %>% 
  mutate(pred=round(pred,3)) %>% 
  arrange(pred==rank)
table(zzz$pred==zzz$rank,zzz$rank)
