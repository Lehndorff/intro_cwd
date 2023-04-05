library(rvest)
library(purrr)
library(tidyverse)

csv_skip<-function(x){
  x %>% 
    write.csv("file.csv",row.names = F)
  x=read_csv("file.csv",skip = 1)
  return(x)
}

# Define the URL of the website
url <- "https://www.sports-reference.com/cbb/schools/"

# Read the HTML code of the website
webpage <- read_html(url)

# Extract the table on the webpage
table <- html_nodes(webpage, "table")

# Extract the links from the table
links <- html_nodes(table, "a") %>% html_attr("href")
links<-links[grepl("/men/",links)]

# Create a list to store the data from each page
data_list <- list()

# Loop through each link and extract the data from the corresponding page
for (link in links) {
  Sys.sleep(runif(1)+2)
  
  # Construct the URL of the page
  print(link)
  page_url <- paste0("https://www.sports-reference.com", link)
  
  # Use read_html() function to extract the HTML code of the page
  page_html <- read_html(page_url)
  
  # html_text(html_nodes(page_html,"span")[9])
  
  # Use html_table() function to extract the data from the page
  data <- page_html %>% html_table() %>% csv_skip() %>% mutate(team=html_text(html_nodes(page_html,"span")[9])) %>% 
    mutate(across(everything(),as.character))
  
  # Store the data in the list
  data_list[[link]] <- data
}

# Combine the data from all pages into a single data frame
all_data <- bind_rows(data_list)
all_data<-all_data %>% 
  rename(Ovr_W=W...4,Ovr_L=L...5,Ovr_Pct=`W-L%...6`,
         Conf_W=W...7,Conf_L=L...8,Conf_Pct=`W-L%...9`,
         Pts_for=`PS/G`,Pts_against=`PA/G`) %>% 
  mutate(across(Ovr_W:`AP Final`,as.numeric)) %>% 
  filter(!is.na(Ovr_W)) %>% 
  mutate(tourn_year=as.numeric(str_extract(Season,"^[0-9]{4}"))+1)

all_data %>% 
  filter(!is.na(`NCAA Tournament`)) %>% 
  group_by(tourn_year,`NCAA Tournament`) %>% 
  summarise(n=n()) %>% 
  View()

table(all_data$`NCAA Tournament`[all_data$tourn_year>=1985&all_data$tourn_year<2023])



selected_data<-all_data %>% 
  filter(!is.na(`NCAA Tournament`)&between(tourn_year,1985,2023)) %>% 
  filter(`NCAA Tournament`!="Playing NCAA Tournament First Round")

clean_data<-selected_data %>% 
  mutate(class=`NCAA Tournament`) %>% 
  mutate(class=ifelse(`NCAA Tournament`=="Lost NCAA Tournament First Four"|`NCAA Tournament`=="Lost NCAA Tournament Opening Round","Lost NCAA Tournament First Round",class)) %>%
  mutate(class=ifelse(`NCAA Tournament`=="Lost NCAA Tournament First Round"&between(tourn_year,2011,2015),"Lost NCAA Tournament Second Round",class)) %>% 
  # group_by(`NCAA Tournament`,tourn_year) %>% 
  # summarise(n=n()) %>% 
  mutate(rank=case_when(
    grepl("Won",`NCAA Tournament`)~1,
    grepl("National Final",`NCAA Tournament`)~2,
    grepl("National Semifinal",`NCAA Tournament`)~4,
    grepl("Regional Final",`NCAA Tournament`)~8,
    grepl(" Regional Semifinal",`NCAA Tournament`)~16,
    grepl("Third Round",`NCAA Tournament`)&between(tourn_year,2011,2015)~32,
    grepl("Second Round",`NCAA Tournament`)&!between(tourn_year,2011,2015)~32,
    TRUE~64
  )) %>% 
  select(-Rk)

table(log2(clean_data$rank))
write.csv(clean_data,file="intro_cwd/Final Project/tourney_data.csv",row.names = F)
      