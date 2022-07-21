# Economic Tweets & Market Sentiment: US Inflation & Recession Risks
# Linh-Chi Pham - Summer 2022

#==============================================================================#
# Data Wrangling
library(rtweet)
library(tidyverse)
library(tidytext)
library(dplyr)

# Data Visualization
library(ggplot2)
library(ggthemes)
library(highcharter)
library(plotly)
library(shiny)
library(cowplot)
library(ggpubr) 

# Quantitative Finance
library(quantmod)
library(qrmdata)
library(yfR)

#==============================================================================#

# SECTION I: Mine Tweets + Download Data ----------------------------------
# Create token 
twitter_token <- create_token(
  app = "Summer_Research_2022",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret, 
  set_renv = TRUE)

# Tweets of Major News Articles
news_articles_2 <- get_timelines(c("WSJ", "TheEconomist", "nytimes", "FinancialTimes", "CNBC", "business",
                                 "ABC", "CBSNews", "CNN", "FoxNews", "MSNBC", "NBC News", "USATODAY",
                                 "washingtonpost", "politico", "VICENews", "HuffPost", "CNETNews", "nprpolitics",
                                 "Newsweek", "NewYorker", "TIME", "USRealityCheck", "YahooFinance", "MarketWatch",
                                 "Forbes", "latimes", "Reuters"), 
                        language = 'en',
                        n = 200000,
                        since = '2021-11-01', 
                        until = '2022-07-21',
                        retryonratelimit = TRUE)

nrow(news_articles_2)
summary(news_articles_2$created_at)
articles_df_2 <- as.data.frame(news_articles_2)
saveRDS(articles_df_2, "news_articles_2.rds")

# Tweets about 'inflation' & 'recession' -- general public (retryonratelimit = TRUE)
inflation_tweets <- search_tweets("inflation", n = 15000, type = "recent", include_rts = FALSE, lang = "en")
recession_tweets <- search_tweets("recession", n = 26000, type = "recent", include_rts = FALSE, lang = "en")
inflation_df <- as.data.frame(inflation_tweets)
recession_df <- as.data.frame(recession_tweets)
saveRDS(inflation_df, "inflation_tweets.rds")
saveRDS(recession_df, "recession_tweets.rds")

#------------------------------------------------------------------------------#
# US Treasury Yields Data
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
t5yr <- getSymbols(Symbols = "DGS5", src = "FRED", auto.assign = FALSE)
t2yr <- getSymbols(Symbols = "DGS2", src = "FRED", auto.assign = FALSE)
treasury_yields <- merge(t2yr, t5yr, t10yr, all = FALSE)
View(treasury_yields)
View(t2yr)

# Convert rownames to column (Date)
treasury_yields <- as.data.frame(treasury_yields)
treasury_yields <- tibble::rownames_to_column(treasury_yields, "Date") 
treasury_yields$Date <- as.Date(treasury_yields$Date, format = "%Y-%m-%d")
str(treasury_yields)  
summary(treasury_yields$Date)

#saveRDS(treasury_yields, "treasury_yields.rds")

# Stock Index
sp500_full <- getSymbols(Symbols = "SPY", auto.assign = FALSE)

View(sp500_full)

# Source tutorial: https://www.r-bloggers.com/2022/03/new-r-package-yfr/

my_ticker <- 'SPY'
#first_date <- '2022-05-17'
#last_date <- "2022-07-18"
sp500 <- yf_get(tickers = my_ticker, 
                first_date = '2007-01-03',
                last_date = '2022-07-20')
View(sp500)
class(sp500)
str(sp500)
sp500_ts <- xts(sp500[,-c(1,2)], order.by = as.POSIXct(sp500$ref_date))
#sp500_ts <- sp500_ts[-1,]
View(sp500_ts)
#==============================================================================#

# SECTION II: DATA PRE-PROCESSING -----------------------------------------

# a. Using tidytext library 
recession <- readRDS("recession_tweets.rds")
recession_clean <- recession %>% 
  mutate(text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"), ## weird encoding
         text = str_replace_all(text, "<a(.*?)>", " "),             ## links 
         text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),      ## html yuck
         text = str_replace_all(text, "&#[:digit:]+;", " "),        ## html yuck
         text = str_remove_all(text, "<[^>]*>"),                    ## mmmmm, more html yuck
         postID = row_number())
View(recession_clean)

# Custom stop words
custom_stops <- bind_rows(tibble(word = c("recession","inflation", "us", "#recession", "just", "#inflation", "economy", "economic", "https", "t.co"),
                                 lexicon = c("CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM")),
                          stop_words)

# Transform tweets into "one token per row" format: unnest_tokens()
tidy_recession <- recession_clean %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(custom_stops) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word)

#------------------------------------------------------------------------------#
# News Article Data
news2 <- readRDS("news_articles_2.rds")
news2_clean <- news2 %>% 
  mutate(text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"), ## weird encoding
         text = str_replace_all(text, "<a(.*?)>", " "),             ## links 
         text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),      ## html yuck
         text = str_replace_all(text, "&#[:digit:]+;", " "),        ## html yuck
         text = str_remove_all(text, "<[^>]*>"),                    ## more html yuck
         postID = row_number())
#View(news_clean)

# Custom stop words
#data("stop_words")
custom_stops <- bind_rows(tibble(word = c("heres","@cnbcmakeit", "writes", "people"),
                                 lexicon = c("CUSTOM", "CUSTOM", "CUSTOM", "CUSTOM")),
                          stop_words)

tidy_news2 <- news2_clean %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]+")) %>%
  add_count(word)
#View(tidy_news)

# Rename news articles
unique(tidy_news2$screen_name)
tidy_news2$screen_name <- as.factor(tidy_news2$screen_name)
tidy_news2$screen_name <- recode_factor(tidy_news2$screen_name,
                                             "business" = "Bloomberg",
                                             "WSJ" = "Wall Street Journal",
                                             "FinancialTimes" = "Financial Times",
                                             "TheEconomist" = "The Economist",
                                             "nytimes" = "New York Times")

# TREEMAP (highcharter): Topic Rank (news)
rank_news_words <- tidy_news2 %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 1800)
View(rank_news_words)

rank_news_words %>% 
  hchart('treemap', hcaes(x = 'word', value = 'n', color = 'n')) %>% 
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>% 
  hc_title(text = "Most Frequent Words in News Articles Tweets") %>% 
  hc_subtitle(text = "March - July 2022") %>% 
  hc_credits(enabled = TRUE, text = "@lchi.pham") %>% 
  hc_caption(text = "Source Tweets: WSJ, Financial Times, New York Times, Bloomberg, CNBC, The Economist")
  
#==============================================================================#

# SECTION III: TWEET EXPLORATION  ----------------------------------------------

# a. Most common words
rank_recession_words <- tidy_recession %>% 
  count(word, sort = TRUE) 
#View(rank_recession_words)
top_15_most_common <- rank_recession_words %>% 
  mutate(word2 = fct_reorder(word, n)) %>%
  top_n(15) %>% 
  ggplot(aes(word2, n)) +
  geom_col() +
  coord_flip() +
  theme_bw()
top_15_most_common  

rec_n_99_pct <- quantile(rank_recession_words$n, probs = 0.99)

#plotLines = list(list(value = 100, color = "pink", width = 2)),

#Function to create theme for each graph

# twitter background: https://media.giphy.com/media/10zI52A8mrfwNG/giphy.gif

# social media background: https://media.giphy.com/media/eBb2W1OYVHou9l6W7N/giphy.gif

# wsj: https://images.wsj.net/im-571374?width=10&height=5

# twitter bird: https://thumbs.gfycat.com/ZestyMedicalAtlanticblackgoby-max-1mb.gif

make_me_pretty <- function(list) {
  hc_theme_merge(
    hc_theme_tufte(),
    hc_theme(
      colors = list,
      chart = list(backgroundColor = "white",
                   divBackgroundImage = "https://media.istockphoto.com/photos/unrecognizable-silhouettes-of-people-walking-on-a-street-picture-id919886016?k=20&m=919886016&s=612x612&w=0&h=Z0P8FRrnqmZpdVgbU_aCHbxvaSz_roxjrKh65LOXhBU="),
      title = list(style = list(color = "black", fontFamily = "Exchange", fontWeight = 'bold')),
      subtitle = list(style = list(color = "black", fontFamily = "Exchange")),
      legend = list(itemStyle = list(fontFamily = "Exchange", color = "black"),
                    itemHoverStyle = list(color = "gray"))
    )
  )
}

#Theme of industry graph
custom_theme <- make_me_pretty(list = c('#fb9a99', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#d9d9d9', '#d6604d', '#ccebc5', '#ffed6f','#8dd3c7'))


# plotLines = list(list(value = 100, color = "brown", width = 2))
rank_recession_words %>% 
  mutate(word2 = fct_reorder(word, n)) %>%
  top_n(15) %>% 
  hchart(type = "bar", hcaes(x = word2, y = n), color = "skyblue", borderColor = "black") %>% 
  hc_yAxis(title = list(text = "Word Count",
                        style = list(color = "black", fontFamily = "Exchange", fontWeight = 'bold')),
           labels = list(style = list(color = "black", fontFamily = "Exchange"))) %>% 
  hc_xAxis(title = list(text = ""),
           labels = list(style = list(color = "black", fontFamily = "Exchange", fontSize = '12px'))) %>% 
  hc_title(text = "Most Frequent Words in 'Recession' Tweets") %>% 
  hc_subtitle(text = "July 2022") %>% 
  hc_add_theme(custom_theme)
  
# b. WORDCLOUD
library(wordcloud)
library(wordcloud2)
library(reshape2)

wc_sentiment <- tidy_recession %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("steelblue", "brown"), max.words = 200, family = "serif")

wcloud <- wordcloud2(rank_recession_words,
           color = "random-dark",
           backgroundColor = "oldlace")

#Analyzing Word and Document Frequency: tf-idf
#TERM FREQUENCY

#bind_tf_idf function
#The idea of tf-idf is to find the important words for the content of each document by:
#+ decreasing the weight for commonly used words
#+ increasing the weight for words that are not used very much in a collection or corpus of documents,

#Combine count of each word with total word counts (all tweets)
recession_words <- tidy_recession %>% 
  count(verified, word, sort = TRUE) %>% 
  ungroup()
recession_total_words <- recession_words %>% 
  group_by(verified) %>% 
  summarize(total = sum(n))
recession_words_combined <- left_join(recession_words, recession_total_words)

ggplot(recession_words_combined, aes(n/total, fill = verified)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0002) +
  facet_wrap(~verified, ncol = 2, scales = "free_y")

recession_tf_idf <- recession_words_combined %>% 
  bind_tf_idf(word, verified, n)
head(recession_tf_idf)
# idf and tf-idf are zero for the extremely common words: inflation, market, amp, people, economy

#Terms with high tf-idf (less common words)
recession_tf_idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

#NOT WORKING
#Here we see keywords which are in fact important in Recession tweets
recession_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word2 = factor(word, levels = rev(unique(word)))) %>% 
  group_by(verified) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(word2, tf_idf, fill = verified)) +
  geom_col(show.legend = FALSE) %>% 
  labs(y = "tf_idf") +
  facet_wrap(~verified, nrow = 2, scales = "free") +
  theme_bw() +
  coord_flip()

# ZIP'S LAW: 
# Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.

freq_by_rank <- recession_words_combined %>% 
  group_by(verified) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

head(freq_by_rank)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = verified)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# the relationship between rank and frequency does have negative slope.

# Broken Power's Law
# rank subset where negative relationship is constant
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#(Intercept)  log10(rank)  
#-1.5846      -0.6756  

# Plot fitted power's law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = verified)) + 
  geom_abline(intercept = -1.5846 , slope = -0.6756, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# The deviations we see here at high rank are not uncommon for many kinds of language; 
# a corpus of language often contains fewer rare words than predicted by a single power law. 
# The deviations at low rank are more unusual. 
# These tweets use a lower percentage of the most common words than many collections of language.

# Relationships between words: N-grams and Correlations
# token = "ngrams" argument, which tokenizes by pairs of adjacent words rather than by individual ones
recession_bigrams <- recession_tweets %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
View(
  recession_bigrams %>% 
    count(bigram, sort = TRUE)
)
View(recession_bigrams)

# Separate bigrams into individual words
bigrams_separated <- recession_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

#Remove stop words from bigrams
bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% custom_stops$word) %>% 
  filter(!word2 %in% custom_stops$word)

View(bigrams_filtered %>% 
       select(word1, word2))

View(
  bigrams_filtered %>% 
    filter(word2 == "market") %>% 
    count(word1, sort = TRUE)
)

#new bigram count
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
View(bigram_counts)

#Regroup bigrams
bigram_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

View(bigram_united %>% 
       select(bigram))

#tf-idf
bigram_tf_idf <- bigram_united %>% 
  count(verified, bigram) %>% 
  bind_tf_idf(bigram, verified, n) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(10, tf_idf) %>% 
  ggplot(aes(reorder(bigram, tf_idf), tf_idf, fill = verified)) +
  geom_col(show.legend = TRUE) +
  #facet_wrap(~verified) +
  coord_flip()
bigram_tf_idf

#------------------------------------------------------------------------------#
#Words followed by negation word
#which words contributed the most in the “wrong” direction?
#==> multiply score by number of times appeared (n)

#bigrams for sentiment analysis
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c("word2" = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  ungroup()
head(not_words)
not_words %>% 
  mutate(contribution = n*value) %>% 
  arrange(desc(abs(contribution))) %>%
  head(20) %>% 
  mutate(word2 = fct_reorder(word2, contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Words preceded by \"not\"", y = "Contribution = Sentiment score * Number of Occurences") +
  coord_flip() +
  theme_bw()

# NOT "good” was overwhelmingly the largest cause of misidentification, making the text seem much more positive than it is.

#negation terms similar to "not"
negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c("word2" = "word")) %>% 
  count(word1, word2, value, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(contribution = n*value) %>% 
  arrange(desc(abs(contribution))) %>%
  head(20) %>% 
  mutate(word2 = fct_reorder(word2, contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Words preceded by \"not\"", y = "Sentiment score * number of occurences") +
  facet_wrap(~word1, scales = "free_x") +
  coord_flip() +
  theme_bw()
#Similar Result as above

#==============================================================================#
#Visualizing networks of bigrams with ggraph
library(igraph)
library(ggraph)
bigram_graph <- bigram_counts %>% 
  filter(n > 30) %>% 
  graph_from_data_frame()

set.seed(269)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# pairs or triplets form common short phrases
# main common center/ topic that are most linked to other topics: 
# prices, commodity, gas, dollar, market

set.seed(269)
a <- grid::arrow(type = "closed", length = unit(.06, "inches"))
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.06, 'inches')) +
  geom_node_point(color = "tomato3", size = 1.9, alpha = 0.7) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#==============================================================================#

# SECTION IV.  TREASURY YIELDS & STOCK PERF vs. WORD FREQ ---------------------

#------------------------------------------------------------------------------#
us_tyields <- readRDS("treasury_yields.rds")
# Filter yield dates 
tyields_subset <- us_tyields %>% 
  filter(Date >= "2022-05-01")
summary(tyields_subset$Date)

#------------------------------------------------------------------------------#
# 1. Correlation between "Recession" word frequency and US Treasury Yields
# Filter out recession words 
recession_news <- tidy_news %>% 
  select(created_at, screen_name, word) %>% 
  filter(word == "recession")
summary(recession_news$created_at)

# Split date-time column into Date and time variables
recession_news$Date <- as.Date(recession_news$created_at) # already got this one from the answers above
recession_news$Time <- format(as.POSIXct(recession_news$created_at), format = "%H:%M:%S")
str(recession_news)

# Join treasury yields data and inflation data
tyield_recession <- recession_news %>% 
  count(screen_name, Date, sort = TRUE) %>% 
  right_join(tyields_subset, by = "Date") %>% 
  rename("recession_count" = "n") %>% 
  filter(!is.na(screen_name))
View(tyield_recession)

inf_10yr <- ggplot(tyield_recession, aes(x = recession_count, y = DGS10)) +
  geom_point(aes(fill = screen_name), color = "black", 
             position = "jitter", size = 2.4, pch = 21, show.legend = TRUE) +
  #scale_x_log10() +
  geom_smooth(method = "lm", col = "brown", se = FALSE, size = 0.8) +
  stat_regline_equation(label.x = 11, label.y = 3.05) +
  labs(subtitle = "10-year Treasury Yields",
       x = "'Recession' Count", y = "10Y Yields", fill = "Source") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        text = element_text(family = "serif", size = 11),
        legend.title = element_blank())

inf_2yr <- ggplot(tyield_recession, aes(x = recession_count, y = DGS2)) +
  geom_point(aes(fill = screen_name), color = "black", 
             position = "jitter", size = 2.4, pch = 21, show.legend = FALSE) +
  #scale_x_log10() +
  geom_smooth(method = "lm", col = "brown", se = FALSE, size = 0.8) +
  stat_regline_equation(label.x = 9, label.y = 2.96) +
  labs(subtitle = "2-year Treasury Yields",
       x = "'Recession' Count", y = "2Y Yields", fill = "Source") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        text = element_text(family = "serif", size = 11))

plot_yield_inf <- ggarrange(inf_2yr, inf_10yr, ncol=2, nrow=1, common.legend = TRUE, legend = "top")
annotate_figure(plot_yield_inf, top = text_grob("US Treasury Yields vs 'Recession' Frequency in Tweets (May - July 2022)", 
                                                color = "black", face = "bold", size = 12.9, family = "serif"))


#------------------------------------------------------------------------------#
# 2. Correlation between "Inflation" word frequency and US Treasury Yields
# Filter out inflation words 
inflation_news <- tidy_news2 %>% 
  select(created_at, screen_name, word) %>% 
  filter(word == "inflation")
str(inflation_news)

# Split date-time column into Date and time variables
inflation_news$Date <- as.Date(inflation_news$created_at) # already got this one from the answers above
inflation_news$Time <- format(as.POSIXct(inflation_news$created_at), format = "%H:%M:%S")
#View(inflation_news)

# Join treasury yields data and inflation data
tyield_inflation <- inflation_news %>% 
  count(screen_name, Date, sort = TRUE) %>% 
  right_join(tyields_subset, by = "Date") %>% 
  rename("inflation_count" = "n") %>% 
  drop_na() %>% 
  mutate(yield_spread = DGS10 - DGS2)
View(tyield_inflation)
str(tyield_inflation)
#saveRDS(tyield_inflation, "tyields_inflation_freq.rds")

inf_10yr <- ggplot(tyield_inflation, aes(x = inflation_count, y = DGS10)) +
  geom_point(aes(fill = screen_name), color = "black", 
             position = "jitter", size = 2.4, pch = 21, show.legend = TRUE) +
  #scale_x_log10() +
  geom_smooth(method = "lm", col = "brown", se = FALSE, size = 0.8) +
  stat_regline_equation(label.x = 30, label.y = 3.05) +
  labs(subtitle = "10-year Treasury Yields",
       x = "'Inflation' Count", y = "10Y Yields", fill = "Source") +
  #scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        text = element_text(family = "serif", size = 11),
        legend.title = element_blank())

inf_2yr <- ggplot(tyield_inflation, aes(x = inflation_count, y = DGS2)) +
  geom_point(aes(fill = screen_name), color = "black", 
             position = "jitter", size = 2.4, pch = 21, show.legend = FALSE) +
  #scale_x_log10() +
  geom_smooth(method = "lm", col = "brown", se = FALSE, size = 0.8) +
  stat_regline_equation(label.x = 30, label.y = 3.25) +
  labs(subtitle = "2-year Treasury Yields",
       x = "'Inflation' Count", y = "2Y Yields", fill = "Source") +
  #scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11), 
        text = element_text(family = "serif", size = 11))

plot_yield_inf <- ggarrange(inf_2yr, inf_10yr, ncol=2, nrow=1, common.legend = TRUE, legend = NULL)
plots_yield_inf <- annotate_figure(plot_yield_inf, top = text_grob("US Treasury Yields vs 'Inflation' Frequency in Tweets (May - July 2022)", 
                                                                   color = "black", face = "bold", size = 12.9, family = "serif"))
plots_yield_inf

# Highchart version
dgs10_inf_model <- augment(lm(DGS10 ~ inflation_count, data = tyield_inflation))
dgs2_inf_model <- augment(lm(DGS2 ~ inflation_count, data = tyield_inflation))

inf_10yr_hc <- hchart(tyield_inflation, type = "scatter",
                      hcaes(x = inflation_count, y = DGS10, group = screen_name),
                      showInLegend = FALSE) %>%
  hc_add_series(dgs10_inf_model, "line", hcaes(x = inflation_count, y = .fitted), showInLegend = FALSE) %>%
  hc_tooltip(pointFormat = "10-year Treasury Yield: {point.y} <br> 'Inflation' Count: {point.x}") %>% 
  hc_title(text = "US Treasury Yields vs 'Inflation' Frequency in Tweets (May - July 2022)") %>% 
  hc_subtitle(text = "Source: Twitter") %>% 
  hc_yAxis(title = list(text = "10-year Treasury Yields"),
           labels = list(format = "{value}%")) %>%
  hc_xAxis(title = list(text = "Inflation Word Count")) %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_credits(enabled = TRUE, text = "https://lchipham.netlify.app") 

inf_2yr_hc <- hchart(tyield_inflation, type = "scatter",
                     hcaes(x = inflation_count, y = DGS2, group = screen_name),
                     showInLegend = FALSE) %>%
  hc_add_series(dgs2_inf_model, "line", hcaes(x = inflation_count, y = .fitted), showInLegend = FALSE) %>%
  hc_tooltip(pointFormat = "2-year Treasury Yield: {point.y} <br> 'Inflation' Count: {point.x}") %>% 
  hc_title(text = "US Treasury Yields vs 'Inflation' Frequency in Tweets (May - July 2022)") %>% 
  hc_subtitle(text = "Source: Twitter") %>% 
  hc_yAxis(title = list(text = "2-year Treasury Yields"),
           labels = list(format = "{value}%")) %>%
  hc_xAxis(title = list(text = "Inflation Word Count")) %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_credits(enabled = TRUE, text = "https://lchipham.netlify.app")

hw_grid(inf_2yr_hc, inf_10yr_hc)

# Correlation matrix
View(tyield_inflation)
tyield_inflation_clean <- na.omit(tyield_inflation)
hchart(cor(tyield_inflation[,-c(1,2)]))

# Inflation Word Count By Date
View(tyield_inflation)
hchart(tyield_inflation, type = "column",
       hcaes(x = Date, y = inflation_count, group = screen_name),
       showInLegend = TRUE) %>% 
  hc_title(text = "Inflation Word Frequency May-July 2022") %>% 
  hc_subtitle(text = "Source: Twitter") %>%
  hc_add_theme(custom_theme) %>% 
  hc_credits(enabled = TRUE, text = "@2022 lchi.pham")

# Inflation Count by News Article
count_per_media <- tyield_inflation %>% 
  group_by(screen_name) %>% 
  summarize(inf_count_per_news = sum(inflation_count)) %>%
  arrange(desc(inf_count_per_news)) %>% 
  hchart(type = 'bar', hcaes(x = screen_name, y = inf_count_per_news, group = screen_name), showInLegend = FALSE) %>% 
  hc_xAxis(categories = count_per_media$screen_name) %>% 
  hc_title(text = "'Inflation' Count by News Outlet") %>% 
  hc_subtitle(text = "Source: Twitter") %>% 
  hc_add_theme(custom_theme) %>% 
  hc_credits(enabled = TRUE, text = "https://lchipham.netlify.app")
count_per_media

#------------------------------------------------------------------------------#
# 3. Correlation between "Inflation" word frequency and Yield Spread
names(tyield_inflation)

# Highchart version
spread_inf_lm <- augment(lm(yield_spread ~ inflation_count, data = tyield_inflation))

hchart(tyield_inflation, type = "scatter",
                      hcaes(x = inflation_count, y = yield_spread, group = screen_name),
                      showInLegend = TRUE) %>%
  hc_add_series(spread_inf_lm, "line", hcaes(x = inflation_count, y = .fitted), showInLegend = FALSE) %>%
  hc_tooltip(pointFormat = "Yield Spread: {point.y} <br> 'Inflation' Count: {point.x}") %>% 
  hc_title(text = "US Treasury Yield Spread vs 'Inflation' Frequency in Tweets") %>% 
  hc_subtitle(text = "May - July 2022") %>% 
  hc_yAxis(title = list(text = "Yield Spread"),
           labels = list(format = "{value}%")) %>%
  hc_xAxis(title = list(text = "Inflation Word Count")) %>%
  hc_add_theme(custom_theme) %>% 
  hc_credits(enabled = TRUE, text = "https://lchipham.netlify.app") 

#------------------------------------------------------------------------------#
# 3. Correlation between "Inflation" word frequency and Yield Spread

# Join S&P500 historical data into tyield_inflation
yield_index_inf <- sp500 %>% 
  select(ref_date, ret_closing_prices, price_adjusted) %>% 
  right_join(tyield_inflation, by = c("ref_date" = "Date"))
View(yield_index_inf)

spy_inf_lm <- augment(lm(price_adjusted ~ inflation_count, data = yield_index_inf))

hchart(yield_index_inf, type = "scatter",
       hcaes(x = inflation_count, y = price_adjusted, group = screen_name),
       showInLegend = TRUE) %>%
  hc_add_series(spy_inf_lm, "line", hcaes(x = inflation_count, y = .fitted), showInLegend = FALSE) %>%
  hc_tooltip(pointFormat = "S&P 500: {point.y} <br> 'Inflation' Count: {point.x}") %>% 
  hc_title(text = "S&P 500 Adjusted Price vs 'Inflation' Frequency in Tweets") %>% 
  hc_subtitle(text = "May - July 2022") %>% 
  hc_yAxis(title = list(text = "S&P 500 Adjusted Price"),
           labels = list(format = "${value}")) %>%
  hc_xAxis(title = list(text = "Inflation Word Count")) %>%
  hc_add_theme(custom_theme) %>% 
  hc_credits(enabled = TRUE, text = "https://lchipham.netlify.app") 

# Time series
highchart(type = "stock") %>% 
  hc_add_series(t10yr, type = "line", color = "pink", name = "10-year Treasury Yield") %>% 
  hc_add_series(t2yr, type = "line", color = "lavender", name = "2-year Treasury Yield") %>% 
  hc_title(text = "S&P 500 vs Treasury Yields") %>% 
  hc_subtitle(text = "Source: Yahoo Finance") %>%
  hc_add_theme(hc_theme_tufte())

highchart(type = "stock") %>% 
  hc_add_series(sp500_ts[,8], type = "line", color = "pink", name = "Close Price Return") %>% 
  hc_add_series(sp500_ts[,7], type = "line", color = "skyblue", name = "Adjusted Price Return") %>% 
  hc_title(text = "S&P 500 Historical Data") %>% 
  hc_subtitle(text = "Source: Yahoo Finance") %>%
  hc_add_theme(hc_theme_tufte())

highchart(type = "stock") %>% 
  hc_add_series(sp500_ts[,4], type = "area", color = "skyblue", name = "Close Price") %>% 
  hc_add_series(sp500_ts[,6], type = "area", color = "pink", name = "Adjusted Price") %>% 
  hc_title(text = "S&P 500 Historical Data") %>% 
  hc_subtitle(text = "Source: Yahoo Finance") %>%
  hc_add_theme(hc_theme_tufte())

#==============================================================================#

# SECTION V: SENTIMENT ANALYSIS ------------------------------------------
get_sentiments("bing")  
get_sentiments("nrc")
get_sentiments("afinn")

# a. Words that contribute to positive and negative sentiment (bing sentiments)
word_pos_neg <- tidy_recession %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n)) 
word_sent_plot <- ggplot(word_pos_neg, aes(word2, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL, y = "Contribution to sentiment") +
  coord_flip()
word_sent_plot  

# b. Words that belong to different sentiment categories (nrc sentiments)
word_nrc <- tidy_recession %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  group_by(sentiment) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n)) 

# 20 words per sentiment category
word_nrc_20 <- word_nrc %>% 
  group_by(sentiment) %>% 
  top_n(20)

nrc_plot_faceted <- ggplot(word_nrc_20, aes(word2, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL, y = "Contribution to sentiment") +
  coord_flip()
word_per_sentiment <- word_nrc %>% 
  count(sentiment, sort = TRUE)

nrc_plot_sent <- ggplot(word_per_sentiment, aes(x = reorder(sentiment, n), 
                                                y = n, 
                                                fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "Word Count") +
  coord_flip()
nrc_plot_sent  

# c. Words that belong to positive/negative sentiment rating (afinn sentiments)
tidy_recession %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(category = ifelse(value < 0, "Negative", "Positive")) %>%
  count(category, value, word) %>% 
  top_n(40) %>% 
  mutate(word2 = fct_reorder(word, value)) %>% 
  ggplot(aes(word2, value, fill = category)) +
  labs(x = NULL) +
  geom_col() +
  coord_flip()

#==============================================================================#

# SECTION VI: TOPIC MODELING -----------------------------------------------
library(textmineR)
library(topicmodels)
library(tm)
library(quanteda)
library(stm)
library(furrr)
plan(multiprocess)
names(tidy_recession)

# Create Document Term Matrix (DTM)
recession_bigram_dtm <- bigram_united %>% 
  count(status_id, bigram, sort = TRUE) %>% 
  cast_dtm(status_id, bigram, n)
View(recession_bigram_dtm)

# CAST DFM 
recession_dfm <- tidy_recession %>% 
  count(status_id, word, sort = TRUE) %>% 
  mutate(line = row_number()) %>% 
  cast_dfm(status_id, word, n) 
rownames(recession_dfm)


# set a seed so that the output of the model is predictable
# k = n, to create a n-topic LDA model.
recession_bi_lda <- LDA(recession_bigram_dtm, k = 6, control = list(seed = 269))
recession_bi_lda

# stm package: train topic model with k topics
topic_model <- stm(recession_dfm, K = 2, 
                   verbose = FALSE, init.type = "Spectral")

# Word-topic probabilities

# Tidy model objects
#  per-topic-per-word probabilities, called β (“beta”)
recession_bi_topics <- tidy(recession_bi_lda, matrix = "beta")
recession_bi_topics

# Interpretation: the term “aa_bought” has a 1.81e-83 probability of being generated from topic 1
# slice_max(): find the 10 terms that are most common within each topic
recession_bi_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 9) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

# User-topic probabilities
# user-document-per-topic probabilities, called γ (“gamma”)
rec_documents <- tidy(recession_lda, matrix = "gamma")
rec_documents 

ggplot(inf_documents, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))





