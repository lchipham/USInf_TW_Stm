## Economic Tweets & Market Sentiment: US Inflation & Recession Risks

### I. ABTRACT
Through exploration of Tweets from the general public and prominent news articles (WSJ, Financial Times, NY Times. etc), this project aims to gain 
insights on market sentiments during the current high inflationary period in the US. "Sentiments" are shaped and defined from discovery of term/word frequency, most mentioned keywords/hashtags, word networks, sentiment analysis and topic modeling. Correlations between US treasury yields data and usage frequency of keywords like "inflation" and "recession" are also found and validated.

### II. CONTENT
This can also be treated as documentation of main R script.
#### 1. Mine Tweets & Data Download
US Treasury Yields data is pulled directyly from FRED (Federal Reserve Electronic Database) and updated whenever a new datapoint is made available. 
Tweets are mined directly from Twitter using Twitter API access. Everytime the script is revised/edited, the author (me) makes an effort redownload datasets to reflect the most recent Tweets and current 'sentiments'. 
- inflation_tweets and recession_tweets datasets: only the most recent 6-9 days worth of Tweets are available due to Twitter API policy restrictions.
- news_articles: timelines from May-July 2022 of the following news articles Twitter accounts: Wall Street Journal (@WSJ), New York Times (@nytimes), Financial Times (@FinancialTimes), Bloomberg (@business), CNBC (@CNBC) and The Economist (@TheEconomist). More news articles can be added if appropriate.

#### 2. Data Preprocessing
Tweets are cleaned, irrelevant/stop words as well as unwanted encodings are removed. Date/time variables are revised regularly and monitored closely.

#### 3. Tweet Exploration
- Barchart: Top n most common words
- Wordcloud: Most common words, freq reflected by size
- tf-idf: find the important words for the content of each document by decreasing the weight for commonly used words; increasing the weight for words that are not used very much in a collection or corpus of documents
- ZIP'S LAW: the frequency that a word appears is inversely proportional to its rank
- Relationships between words: N-grams (bigram/trigram) and Correlations
- Words followed by negation word: which words contributed the most in the “wrong” direction? ("not" good, "no" hope, etc)
- Word networks: Visualizing networks of bigrams with ggraph

#### 4. Treasury Yields vs Word Frequency
Correlations between US Treasury Yields and keyword frequency of appearing in Tweets (of news articles). 
Treasury Yields data are joined with inflation & recession tweets count data.
- Treemap: Most commonly discussed (top-of-mind) topics on news articles
- Dual scatterplots: 2yr & 10r yields vs Word Freq (ggplot2 + highcharter versions)
- Correlation matrix: yields vs word freq, yields vs yields
- Line chart (Time Series): US Treasury Yields Historical Data (slidebar available)
- Column chart: Inflation/Recession Word Frequency By Time
- Coord-flipped bar chart: Inflation Count by News Article

#### 5. Sentiment Analysis
The following sentiment libraries are used: afinn, NRC, bing.
- bing sentiments: Words that contribute to positive and negative sentiment 
- nrc sentiments: Words that belong to different sentiment categories
- afinn sentiments: Words that belong to positive/negative sentiment rating

#### 6. Topic Modeling
Various topic models are built and tested with different k (number of topics). Models are built from bigram Tweet data instead of individual-word data as it makes understanding and navigating topics significantly easier.
- Create Document Term Matrix (DTM)
- Train topic model with k topics
- 2Word-topic probabilities
- Document(user)-topic probabilities

### III. OUTPUT 
#### 1. RShiny App

#### 2. RMarkdown: Research Report




