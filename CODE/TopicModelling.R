library(tidyverse)
library(tidytext)
library(stringi)       # string manipulation
library(textclean)     # string manipulation
library(tm)            # corpus
library(udpipe)        # POS-tagging
library(textstem)      # lemmatization function
library(topicmodels)   # LDA function
library(wordcloud)
library(igraph)        # bigram graph
library(ggraph)        # bigram graph
library(data.table)    # core splitting
library(future.apply)  # core splitting
library(zoo)
library(randomcoloR)


# set up paths
outpath <- paste0(getwd(), '/OUTPUT/')
imppath <- paste0(getwd(), '/DATA/')


################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################
####  PART I: Dataset  #########################################################
################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################


################################################################################
## 1) Load and clean the scraped articles  #####################################
################################################################################
# load articles
articles <- read_csv(paste0(imppath, "/NYtimesArticles_sample_World_0.06.csv"),
                     locale = locale(encoding="UTF-8")) %>%
  drop_na(text)  %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, text, section_name, word_count, pub_date, web_url) # reorder

# delete preabmles
preambles <- c(".*?Occasionally the digitization process introduces transcription errors or other problems; we are continuing to work to improve these archived versions.",
               ".*?Buy ReprintsView on timesmachineTimesMachine is an exclusive benefit for home delivery and digital subscribers.")
articles <- articles %>%
  mutate(text = gsub(pattern = preambles[1], replacement = "", text, perl = T),
         text = gsub(pattern = preambles[2], replacement = "", text, perl = T))

sub <- articles[100:200,] %>%
  rbind(articles[7200:7700,])


##  News article frequency analysis
meta <- read_csv(paste0(imppath, "/NYtimesMeta.csv"))
meta_prev <- read_csv(paste0(imppath, "/NYtimesMeta1979-1989.csv")) %>%
  filter(pub_date > as.Date("1980-12-31"))
meta <- rbind(meta, meta_prev)

# All meta data
meta_freq <- meta %>%
  select(pub_date, word_count) %>%
  filter(word_count > 0) %>%
  mutate(count = 1,
         year_quarter = paste(format(pub_date, "%Y"), quarters(pub_date))) %>%
  group_by(year_quarter) %>%
  summarise(all = sum(count))

# Meta data of section "World"
meta_world <- meta %>%
  filter(section_name == "World", word_count > 0) %>%
  select(pub_date, word_count) %>%
  filter(word_count > 0) %>%
  mutate(count = 1,
         year_quarter = paste(format(pub_date, "%Y"), quarters(pub_date))) %>%
  group_by(year_quarter) %>%
  summarise(world = sum(count))

# Articles in sample
articles_frequency <- articles %>%
  mutate(count = 1,
         year_quarter = paste(format(pub_date, "%Y"), quarters(pub_date))) %>%
  group_by(year_quarter) %>%
  summarise(sample = sum(count))

# Compare frequencies
article_frequencies <- meta_freq %>%
  left_join(meta_world, by="year_quarter") %>%
  left_join(articles_frequency, by="year_quarter") %>%
  pivot_longer(!year_quarter,
               names_to = "type",
               values_to = "values") %>%
  mutate(year_quarter = as.Date(as.yearqtr(year_quarter, format = "%Y Q%q")))

article_frequencies %>%
  ggplot(aes(x=year_quarter, y=values)) +
    geom_line(aes(color=type), size=1) + xlab("") + ylab("") +
    theme_minimal() +
    scale_x_date(date_breaks = "2 years" , date_labels = "%Y")

article_frequencies %>%
  filter(type != "all") %>%
  ggplot(aes(x=year_quarter, y=values)) +
    geom_line(aes(color=type), size=1) + xlab("") + ylab("") +
    theme_minimal() +
    scale_x_date(date_breaks = "2 years" , date_labels = "%Y")

article_frequencies %>%
  filter(type == "sample") %>%
  ggplot(aes(x=year_quarter, y=values)) +
    geom_line(aes(color=type), size=1) + xlab("") + ylab("") +
    theme_minimal() +
    scale_x_date(date_breaks = "2 years" , date_labels = "%Y")



################################################################################
## 2) Identify and label most frequent bigrams  ################################
################################################################################

# Function to obtain 200 most frequent bigrams
bigramer <- function (dataframe, stops){
  bigram <- dataframe %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2) %>% # tokenize texts by bigrams
    separate(bigram, c("word1", "word2"), sep = " ") %>% # seperate bigrams
    filter(!word1 %in% stops) %>% # filter for stopwords
    filter(!word2 %in% stops) %>%
    count(word1, word2, sort = TRUE) %>% # order by occurence of bigram
    slice_head(n=200) %>%
    mutate(pat = paste(word1, word2),
           bigram = paste(word1, word2, sep="_"),
           bigram_smooth = paste0(word1, word2))

  return(bigram)
}


# Obtain bigrams from article data (note: takes 5 min with 14000 articles)
stops <- data.frame(word = c("year", "years", "said","percent", "days", "months",
                             "weeks", "mr", "mrs", "font", "css", "https", "740px",
                             "schema.org", "videoid", "1px", "nyt.com", "www.nytimes.com",
                              "truncated", "decoration:none", "http", "videoobject",
                              "index.html", "offsite", "width", "a.m", "bottom",
                             "web", "family:nyt", "1pd7go", "700", "sans", "serif"),
                    lexicon = rep("snowball", 33)) %>%
  rbind(stop_words) %>%
  filter(word != "new")

bigram_start <- Sys.time()
bigrams <- bigramer(articles, stops$word)
bigram_end <- Sys.time()
bigram_runtime <- bigram_start - bigram_end

# Graph connections of bigrams
arrows <- grid::arrow(type = "closed", length = unit(.15, "inches"))
bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrows) +
    geom_node_point(color = "lightblue", size = 4) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()

# Exchange bigrams in articles by smooth (preprocessing-robust) bigrams
articles_bigram <- articles %>%
  mutate(text = mgsub(tolower(text), pattern = as.vector(bigrams$pat),
                      replacement = as.vector(bigrams$bigram_smooth)))



################################################################################
## 3) Keep only nouns and names  ###############################################
################################################################################

ud_model <- udpipe_load_model(udpipe_download_model(language = "english")$file_model)

# load this file if you do not want to rerun (takes ca. 2h)

load(paste0(imppath, 'pos_df2.RData'))
# pos_start <- Sys.time()
# articles_tagged <- as.data.frame(udpipe_annotate(ud_model, x = articles_bigram$text))
# pos_end <- Sys.time()

# pos_runtime <- pos_start - pos_end


articles_composition <- data.frame(table(articles_tagged$upos)) %>%
   filter(Freq > sum(Freq)/1000) %>%
   mutate(rel = ifelse(Var1 %in% c("NOUN", "PROPN"), "1","0"))


# Prepare setup for POS-tagging
# function do the annotaion
annotate_splits <- function(x, file) {
  ud_model <- udpipe_load_model(file)
  x <- as.data.table(udpipe_annotate(ud_model,
                                    x = x$text,
                                    doc_id = x$doc_id))
 return(x)
}

# function to split articles based on available cores and apply annotator
annotator <- function (dataframe){
    ud_model <- udpipe_download_model(language = "english", overwrite = F)
    ud_en <- udpipe_load_model(ud_model)
    corpus_splitted <- split(dataframe, seq(1, nrow(dataframe), by = 100))
    annotation <- future_lapply(corpus_splitted, annotate_splits, file = ud_model$file_model)
    annotation <- rbindlist(annotation)
  return(annotation)
}

## Define number of cores to be used
#ncores <- 2L
#plan(multiprocess, workers = ncores)
#
## apply fuction to articles
#pos_start <- Sys.time()
#articles_tagged <- annotator(articles_bigram)
#pos_end <- Sys.time()
#
#pos_runtime <- pos_start - pos_end

articles_composition <- data.frame(table(articles_tagged$upos)) %>%
  filter(Freq > sum(Freq)/1000) %>%
  mutate(rel = ifelse(Var1 %in% c("NOUN", "PROPN"), "1","0"))


articles_composition %>%
  ggplot(aes(Freq, reorder(Var1, Freq))) +
    geom_col(aes(fill=rel)) +
    scale_fill_manual(values = c("grey", "navyblue")) +
    xlab("") + ylab("") + theme_minimal() + theme(legend.position="none")

articles_nouns <- articles_tagged %>%
  filter(upos %in% c("NOUN", "PROPN")) %>%
  select(doc_id, lemma) %>%
  group_by(doc_id) %>%
  mutate(text = paste0(lemma, collapse = " ")) %>%
  select(-lemma) %>%
  mutate(doc_id = as.numeric(str_sub(doc_id, 4))) %>%
  unique() %>%
  left_join(articles %>% select(doc_id, pub_date, web_url), by = "doc_id")




################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################
####  PART II: Build and Preprocess Corpus  ####################################
################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################


################################################################################
## 1) Create corpus  ###########################################################
################################################################################

# Model 1 (basic):
corpus_basic <- VCorpus(DataframeSource(articles))
subcorpus <- VCorpus(DataframeSource(sub))
# Model 2 (bigrams):
corpus_bigrams <- VCorpus(DataframeSource(articles_bigram))
# Model 3 (nouns):
corpus_nouns <- VCorpus(DataframeSource(articles_nouns))



################################################################################
## 2) Build preprocessing functions  ###########################################
################################################################################

# define special functions used for cleansing
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x) # removes everything that is not alphanumerical
bigramsReadable <- function (x) mgsub(x, pattern = as.vector(bigrams$bigram_smooth),
                                         replacement = as.vector(bigrams$bigram))
numbers_char <- c("null", "zero","one", "two", "three", "four", "five", "six",
                  "seven", "eight", "nine", "ten", "first", "second", "third")

# Preprocesser 1: Common basic preprocessing steps
preprocesser1 <- function (corpus){
  processed <- corpus %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(content_transformer(removeSpecialChars)) %>% # includes removing punctuation
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeWords), c(stopwords("english"), numbers_char)) %>%
    tm_map(content_transformer(stemDocument), language = "en") %>%
    tm_map(content_transformer(stripWhitespace))
  return(processed)
}

# Preprocesser 2: preprocesser 1 + lemmatization
preprocesser2 <- function (corpus){
  processed <- corpus %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(content_transformer(removeSpecialChars)) %>% # includes removing punctuation
    tm_map(content_transformer(removeWords), c(stopwords("english"), numbers_char)) %>%
    tm_map(content_transformer(lemmatize_strings)) %>%
    tm_map(content_transformer(bigramsReadable)) %>%
    tm_map(content_transformer(stemDocument), language = "en") %>%
    tm_map(content_transformer(stripWhitespace))
  return(processed)
}

# Preprocesser 3: adjusted to noun corpus
preprocesser3 <- function (corpus){
  processed <- corpus %>%
    tm_map(content_transformer(removeSpecialChars)) %>% # includes removing punctuation
    tm_map(content_transformer(stripWhitespace)) %>%
    tm_map(content_transformer(bigramsReadable))
  return(processed)
}



################################################################################
## 3) Apply preprocessers to corpus  ###########################################
################################################################################
# Model 1:
corpus_basic_p <- preprocesser1(corpus_basic)
# Model 2:
corpus_bigram_p <- preprocesser2(corpus_bigrams)
# Model 3:
corpus_nouns_p <- preprocesser3(corpus_nouns)

save(corpus_basic_p, file = paste0(imppath, 'corpus_basic_p.RData'))
save(corpus_bigram_p, file = paste0(imppath, 'corpus_bigram_p.RData'))
save(corpus_nouns_p, file = paste0(imppath, 'corpus_nouns_p.RData'))

# load files if you do not want to rerun preprocessing
load(paste0(imppath, 'corpus_basic_p.RData'))
load(paste0(imppath, 'corpus_bigram_p.RData'))
load(paste0(imppath, 'corpus_nouns_p.RData'))


################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################
####  PART III: Document-Term-Matrix and LDA  ##################################
################################################################################
# + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
################################################################################


################################################################################
## Dscription unprocessed corpus ###############################################
################################################################################

# Document-term-matrix
dtm <- DocumentTermMatrix(corpus_basic)
# word frequencies
freq <- sort(colSums(as.matrix(dtm)), decreasing = T)
# wordcloud
wordcloud(names(freq), freq, max.words = 100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))



################################################################################
## Model 1  ####################################################################
################################################################################

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 1) Create and visualize document-term-matrix  - - - - - - - - - - - - - - - -
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Document-term-matrix
dtm1 <- DocumentTermMatrix(corpus_bigram_p)
inspect(dtm1)
# Sparse document-term-matrix
dtms1 <- removeSparseTerms(dtm1, sparse = 0.8)
inspect(dtms1)
# Remove empty rows for LDA
sel_idx1 <- slam::row_sums(dtms1) > 0
dtms1 <- dtms1[sel_idx1, ]


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 2) Analyze word frequencies across whole corpus - - - - - - - - - - - - - - -
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Word frequencies
freq1 <- sort(colSums(as.matrix(dtms1)), decreasing = T)

# Wordcloud whole corpus
wordcloud(names(freq1), freq1, max.words = 100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 3) Evaluate  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#
evaluate <- function(corpus, K, name, sparsity, data = articles){
  set.seed(1)
  colors <- distinctColorPalette(K)

  # set up document term matrix
  dtm <- DocumentTermMatrix(corpus)
  inspect(dtm)
  # Sparse document-term-matrix
  dtms <- removeSparseTerms(dtm, sparse = sparsity)
  inspect(dtms)
  # Remove empty rows for LDA
  sel_idx1 <- slam::row_sums(dtms) > 0
  dtms <- dtms[sel_idx1, ]

  # produce wordcloud
  # freq <- sort(colSums(as.matrix(dtms)), decreasing = T)
  # pdf(paste0(outpath, 'wordcloud_', name, '.png'), width = 5, height = 5)
  # wordcloud(names(freq), freq, max.words = 100, scale=c(8, 1),
  #         colors=brewer.pal(6, "Dark2"))
  # dev.off()

  # run LDA
  lda <- LDA(dtms, k = K)
  print('Calculated LDA')

  # analyze resulting topics
  topics <- tidy(lda, matrix = "beta")
# top terms per topic
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic),
         topic = paste0('Topic ', topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
    scale_fill_manual(values = colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
    ylab('') + xlab('') +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_reordered() +
    ggsave(paste0(outpath, 'top_terms_', name, '_', K, '.png'))
  print(paste0('Saved top terms at: ', outpath, 'top_terms_', name, '_', K, '.png'))


  # analyze shares over time
  doc <- tidy(lda, matrix = 'gamma')
  doc$document <- as.integer(doc$document)
  doc_wide <- spread(doc, key = 'topic', value = 'gamma')
  docs <- data %>% select(doc_id, pub_date) %>% # filter(doc_id != 6402) %>%
  left_join(doc_wide, by = c('doc_id' = 'document')) %>% select(-doc_id) %>% reshape2::melt(id.vars = 'pub_date') %>%
  mutate(month = format(pub_date, "%Y-%m"),
         quarter = lubridate::quarter(pub_date, with_year = T),
         year = lubridate::year(pub_date))

  # get max prob, only if over 18%, quarterly
  doc %>% group_by(document) %>% filter(gamma == max(gamma)) %>% filter(gamma > 0.18) %>%
  left_join(data %>% select(doc_id, pub_date), by = c('document' = 'doc_id')) %>%
  mutate(month = format(pub_date, "%Y-%m"),
         quarter = lubridate::quarter(pub_date, with_year = T),
         year = lubridate::year(pub_date)) %>%
  group_by(quarter, topic) %>%
  count() %>% ungroup() %>%
  group_by(quarter) %>%
  mutate(total = sum(n),
         perc = n/total,
         topic = as.character(topic)) %>%
    ggplot() + geom_area(aes(x = quarter, y = perc, fill = topic), stat = "identity") +
    scale_fill_manual(values = colors) +
        theme_minimal() +
  ggsave(paste0(outpath, 'max_shares_quarterly_', name, '_', K, '.png'))
  print(paste0('Saved quarterly max shares at: ', outpath, 'max_shares_quarterly_', name, '_', K, '.png'))

    # get max prob, only if over 18%, monthly
  doc %>% group_by(document) %>% filter(gamma == max(gamma)) %>% filter(gamma > 0.18) %>%
  left_join(data %>% select(doc_id, pub_date), by = c('document' = 'doc_id')) %>%
  mutate(month = format(pub_date, "%Y-%m"),
         quarter = lubridate::quarter(pub_date, with_year = T),
         year = lubridate::year(pub_date)) %>%
  group_by(month, topic) %>%
  count() %>% ungroup() %>%
  group_by(month) %>%
  mutate(total = sum(n),
         perc = n/total,
         topic = as.character(topic),
         month = as.Date(paste0(month, "-01"), format='%Y-%m-%d')) %>%
    ggplot() + geom_area(aes(x = month, y = perc, fill = topic), stat = "identity") +
    scale_fill_manual(values = colors) +
        theme_minimal() +
  ggsave(paste0(outpath, 'max_shares_monthly_', name, '_', K, '.png'))
  print(paste0('Saved monthly max shares at: ', outpath, 'max_shares_monthly_', name, '_', K, '.png'))

  # plot quarterly
  docs %>%
  group_by(quarter, variable) %>%
  summarise(share = sum(value)) %>% ungroup() %>%
  group_by(quarter) %>%
  mutate(total = sum(share),
         perc = share/total) %>%
    ggplot() + geom_area(aes(x = quarter, y = perc, fill = variable), stat = "identity") +
    scale_fill_manual(values = colors) +
        theme_minimal() +
  ggsave(paste0(outpath, 'shares_quarterly_', name, '_', K, '.png'))
  print(paste0('Saved quarterly shares at: ', outpath, 'shares_quarterly_', name, '_', K, '.png'))

  # plot yearly
  docs %>%
      group_by(year, variable) %>%
  summarise(share = sum(value)) %>% ungroup() %>%
  group_by(year) %>%
  mutate(total = sum(share),
         perc = share/total) %>%
    ggplot() + geom_area(aes(x = year, y = perc, fill = variable), stat = "identity") +
    scale_fill_manual(values = colors) +
        theme_minimal() +
  ggsave(paste0(outpath, 'shares_yearly_', name, '_', K, '.png'))
  print(paste0('Saved yearly shares at: ', outpath, 'shares_yearly_', name, '_', K, '.png'))
}


for (k in c(3, 5, 10, 15, 25)){
  evaluate(corpus_basic_p, K = k, 'basic', 0.98)
}

for (k in c(3, 5, 10, 15, 25)){
  evaluate(corpus_bigram_p, K = k, 'bigram', 0.99)
}

for (k in c(3, 5, 10, 15, 25)){
  evaluate(corpus_nouns_p, K = k, 'nouns', 0.99)
}

