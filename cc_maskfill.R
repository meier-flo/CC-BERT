require(tidyverse)
require(tidytext)
require(SnowballC)
source("hf_maskfill.R")
require(scales)
#palette_okabe_sentiment <- c('Negative' = '#D55E00','Neutral'='grey','Positive'='#009E73') 

palette_keywords_1<-c('#56B4E9','#D55E00','#009E73')
palette_keywords_2<-c('#56B4E9','#CC79A7','#E69F00')
infomedia_final_augumented <- read_csv("infomedia_final_augumented.csv")

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
non_relevant_articles <-c('e39f8337','e84656f1','e77037a2','e81d2fc2','e893ab64','e6bf352f')

set.seed(1234)


# Transforming the dataset 
# masking the keywords and 
# sample 5% for each keyword and time period 

infomedia_final_augumented%>%
  filter(WordCount>100)%>%
  filter(!ArticleId%in%non_relevant_articles)%>%
  mutate(all_text = paste(Heading,SubHeading,BodyText,sep=' '))%>%
  select(ArticleId,all_text)%>%
  mutate(all_text = str_remove_all(all_text,'Klik her for at åbne originalartiklen på (Jp.dk|Kristeligt-Dagblad.dk|Politiken.dk|Ekstrabladet.dk|Berlingske.dk|Weekendavisen.dk)'),
         all_text = str_remove_all(all_text,'Klik her'),
         all_text = str_remove_all(all_text,'/ritzau/'))%>%
  mutate(all_text = cleanFun(all_text),
         klima_count = str_count(all_text,'klimaforandring*'),
         drivhus_count = str_count(all_text,'drivhuseffekt*'),
         opvarmning_count = str_count(all_text,'opvarmning'))%>%
  group_by(ArticleId)%>%
  mutate(total = sum(klima_count,drivhus_count,opvarmning_count))%>%
  filter(total>2)%>%
  unnest_tokens(input=all_text,output=all_text,
                token = "sentences")%>%
  mutate(all_text = str_replace_all(all_text,'(global\\Wopvarmning)','global-opvarmning'),
         all_text = str_replace_all(all_text,'(globale\\Wopvarmning)','global-opvarmning'))%>%
  mutate(all_text = cleanFun(all_text),
         klima_count = str_count(all_text,'klimaforandring*'),
         drivhus_count = str_count(all_text,'drivhuseffek*'),
         opvarmning_count = str_count(all_text,'global-opvarmning'),
         total=klima_count+drivhus_count+opvarmning_count)%>%
  filter((klima_count == 1 | drivhus_count == 1 | opvarmning_count == 1) &total<2)%>%
  mutate(klimaforandring = ifelse(str_detect(all_text,'klimaforandring*'),1,0),
         drivhuseffekt = ifelse(str_detect(all_text,'drivhuseffek*'),1,0),
         global_opvarmning = ifelse(str_detect(all_text,'global-opvarmning'),1,0))%>%
  mutate(keyword = case_when(klimaforandring==1~'climate_change',
                             drivhuseffekt==1~'greenhouse_effect',
                             global_opvarmning==1~'global_warming'))%>%
  left_join(infomedia_final_augumented%>%select(ArticleId,pub_year),
            by = c('ArticleId'))%>%
  mutate(period = case_when(pub_year<2000~'1990-1999',
                            pub_year>1999&pub_year<2005~'2000-2004',
                            pub_year>2004&pub_year<2010~'2005-2009',
                            pub_year>2009&pub_year<2015~'2010-2014',
                            pub_year>2014~'2015-2021'))%>%group_by(keyword,period)%>%count()

    mutate(all_text = str_replace_all(all_text,
                                    pattern = 'klimaforandring\\w*',
                                    replacement = '[MASK]'),
         all_text = str_replace_all(all_text,
                                    pattern = 'drivhuseffek\\w*',
                                    replacement = '[MASK]'),
         all_text = str_replace_all(all_text,
                                    pattern = 'global-opvarmning',
                                    replacement = '[MASK]'))%>%
  mutate(word_count = str_count(all_text, '\\w+'))%>%
  filter(word_count < 500)%>%
  group_by(keyword,period)%>%
             sample_frac(size = .05)%>%
    select(ArticleId,all_text,keyword,period)->df_for_api
             
          


# use the function call_hf_inference maskfill to connect to the API 
# and get predictions 
hf_result<-map_df(df_for_api$all_text,~call_hf_inference_maskfill(.))

# repeat the original doc each row five times 
df_for_api[rep(seq_len(nrow(df_for_api)), each = 5), ]->maskfill_df

#bind the original back to the HF API result
bind_cols(maskfill_df,hf_result)->final_maskfill_df


# --------------------------
# Let's do some analysis 
require(tidylo)
require(SnowballC)
require(scales)

#AFINN sentiment  lexicon for Danish
afinn_dansk <- read_delim("afinn_dansk.txt", 
                          delim = "\t", escape_double = FALSE, 
                          col_names = FALSE, trim_ws = TRUE)%>%
                          select(word = X1,value = X2)%>%
                          mutate(word = wordStem(word,
                          language = 'da'))%>%
                          distinct()
  

#Code for Figure 2
# Create a plot for the distribution of the confidence score
final_maskfill_df%>%group_by(ArticleId)%>%slice_max(score)%>%
  mutate(keyword=case_when(keyword == 'climate_change'~'Climate change',
                           keyword == 'global_warming'~'Global warming',
                           keyword == 'greenhouse_effect'~'Greenhouse effect'))%>%
  ggplot(aes(x=score,color=keyword))+
    scale_color_manual(values=palette_keywords_1)+
                geom_density(linewidth=1.2)+
                    theme_minimal(base_size = 18)+
      labs(y='Density',x='Confidence Score',color='')+
              theme(legend.position = 'bottom',
                axis.text = element_text(colour = "black"))


# What is the average confidence score 
final_maskfill_df%>%
      group_by(ArticleId)%>%slice_max(score)%>%
        group_by(keyword)%>%summarise(avg = mean(score))


# What is the type token ration for each keyword 
# The higher the ratio the more unique words are predicted 
final_maskfill_df%>%
            group_by(keyword)%>%
                summarise(type=n_distinct(token_str),
                        token = n(),
                        ttr=type/token)%>%view()


# Code for Figure 3
# Create the plot for the top 15 predicted 
final_maskfill_df%>%
  mutate(token_str = wordStem(token_str,language = 'da'))%>%
  group_by(keyword,token_str)%>%count()%>%group_by(keyword)%>%
  mutate(percent = round(n/sum(n),3))%>%
 # bind_log_odds(keyword, token_str, n)%>%
 #left_join(afinn_dansk,by=c('token_str'='word'))%>%
 #mutate(value = case_when(is.na(value)~'Neutral',
 #                          value>0~'Positive',
 #                          value<0~'Negative'))%>%
mutate(keyword=case_when(keyword == 'climate_change'~'Climate change',
                        keyword == 'global_warming'~'Global warming',
                        keyword == 'greenhouse_effect'~'Greenhouse effect'))%>%
  group_by(keyword)%>%
  slice_max(percent, n = 15) %>% ungroup %>% distinct()%>%
  ggplot(aes(x=percent,y=reorder_within(token_str, 
                    percent,keyword),fill=keyword,color=keyword))+
  geom_col(alpha=.5)+facet_wrap(vars(keyword),scales = 'free_y')+
  geom_text(aes(label = percent*100), hjust = -0.2,size=4,color='black')+
  scale_fill_manual(values=palette_keywords_1)+
  scale_color_manual(values=palette_keywords_1)+
            theme_minimal(base_size = 18)+scale_y_reordered()+
                labs(x='Percent',y='',fill='Sentiment',color='Sentiment')+
  scale_x_continuous(labels = label_percent())+
  theme(axis.text = element_text(colour = "black"),
        legend.position = 'none')



# Code for Figure 1
df_for_api%>%
    group_by(keyword,period)%>%count()%>%
  mutate(keyword=case_when(keyword == 'climate_change'~'Climate change',
                           keyword == 'global_warming'~'Global warming',
                           keyword == 'greenhouse_effect'~'Greenhouse effect'))%>%
      ggplot(aes(x=period,y=n,
        fill=keyword,color=keyword,group=keyword))+
          geom_col(position='dodge2',alpha=0.6)+
         geom_text(aes(label = n),
           colour = "black", size = 3.5,
           vjust = -0.8, position = position_dodge(.9))+
            theme_minimal(base_size = 18)+ 
             scale_fill_manual(values=palette_keywords_1)+
            scale_color_manual(values=palette_keywords_1)+
            theme(axis.text = element_text(colour = "black"),
                        legend.position = 'bottom')+labs(y='Count',
                                    x='Period',color='',fill='')
              

df_for_api%>%group_by(keyword)%>%count()


#### Do some sentiment analysis - plot not part of the paper but described
final_maskfill_df%>%
  mutate(token_str = wordStem(token_str,language = 'da'))%>%
  group_by(keyword,token_str)%>%count()%>%group_by(keyword)%>%
  mutate(percent = round(n/sum(n),3))%>%
  left_join(afinn_dansk,by=c('token_str'='word'))%>%
  mutate(value = case_when(is.na(value)~'Neutral',
                            value>0~'Positive',
                            value<0~'Negative'))%>%
  mutate(keyword=case_when(keyword == 'climate_change'~'Climate change',
                           keyword == 'global_warming'~'Global warming',
                           keyword == 'greenhouse_effect'~'Greenhouse effect'))%>%
  group_by(keyword)%>%
  slice_max(percent, n = 15) %>% ungroup %>% distinct()%>%
  ggplot(aes(x=percent,y=reorder_within(token_str, 
                                        percent,keyword),fill=value,color=value))+
  geom_col(alpha=.5)+facet_wrap(vars(keyword),scales = 'free_y')+
  geom_text(aes(label = percent*100), hjust = -0.2,size=4,color='black')+
  scale_fill_manual(values=palette_keywords_1)+
  scale_color_manual(values=palette_keywords_1)+
  theme_minimal(base_size = 18)+scale_y_reordered()+
  labs(x='Percent',y='',fill='Sentiment',color='Sentiment')+
  scale_x_continuous(labels = label_percent())+
  theme(axis.text = element_text(colour = "black"),
        legend.position = 'none')

