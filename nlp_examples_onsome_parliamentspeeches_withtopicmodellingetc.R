#Packages used
#library(tidyverse)
#library(stringr)
#library(Sentida)
#library(tidytext)
#library(stopwords)
#library(wordcloud)
#library(tm)
#library(quanteda)
#library(stm)
#library(topicmodels)
#library(RColorBrewer)

# this is text analysing NLP of some speeches from danish parliame --------

##

#reading data from select speeches. 
taler <- readRDS("alletaler.rds")
speaker <- readRDS("pm.rds")
sentiment_da <- read.csv("headword.csv", header = FALSE)

#
#

# transforming data. ------------------------------------------------------


taler <- taler %>%
  extract(title, into = "year", regex = "(\\d{4})", remove = FALSE)

# Laver ny variabel "speaker
taler <- merge(taler, speaker, by = "year")

#creating a variable folketing
taler <- taler %>%
  mutate(folketing = ifelse(str_detect(title, "folketing|aabningstale-1|aabningstale-2"), 1, 0))

taler_folketing <- taler %>% 
  filter(folketing == 1)

#Removing a non important text element.. "print" (as this was from a webscrape. )
taler$content <- gsub("Udskriv","", taler$content)

# Sentida
# Laver sentiment-score med sentida
taler$sentiment_scoresentida <- NA

for (i in 1:nrow(taler)) {
  score <- sentida(taler$content[i])
  taler$sentiment_scoresentida[i] <- score
}

# Selec 4 speeches for topicmodelling.
taler_samlet <- as.data.frame(matrix(ncol = 2, nrow = 4))
colnames(taler_samlet) <- c("tale", "content")

taler_samlet$tale[1] <- taler$title[40]
taler_samlet$tale[2] <- taler$title[70]
taler_samlet$tale[3] <- taler$title[100]
taler_samlet$tale[4] <- taler$title[6]
taler_samlet$content[1] <- taler$content[40]
taler_samlet$content[2] <- taler$content[70]
taler_samlet$content[3] <- taler$content[100]
taler_samlet$content[4] <- taler$content[6]
taler_samlet$content <- gsub("Udskriv", "", taler$content)

# Tokenzier 
# Sentimentanalyse
# Indlæser text og laver til dataframe med ord
# Tæller ord i hver tale

taler_count <- taler_samlet %>% 
  unnest_tokens(word, content) %>% 
  group_by(tale) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n))

#Here im remmoving danish stopwrods, creating a custom extra vector of relevant stopwords. 
stopord <- stopwords("da")
stopord <- tibble(word = stopord)
ekstra_stopord <- tibble(word = c("kan", "fik", "igen", "ved", "så", "få",
                                  "får","gør","gå","kun","uden","ingen","selvom", 
                                  "høre", "vide","står","kom","jeres","fået","nok",
                                  "bare","intet","før","gik","går","må","lige",
                                  "flere", "hver", "nye","ny","første","par", "a"))
stopord <- rbind(stopord, ekstra_stopord)
taler_count <- anti_join(taler_count, stopord, by = "word")

# Fjerner alt som ikke er ord
taler_samlet <- taler_samlet[grepl("^[A-Za-z]+$", taler_samlet$word), ]

#Ysing tidytext to create initial topic modelling
taler_count2 <- taler_count %>%
  bind_tf_idf(word, tale, n) %>% 
  # Fjerner specifikke uønskede ord
  filter(word != "ogsaa")

# Sdentiments (danish)
sentiment_da <- sentiment_da %>%
  separate_rows(V6, sep = ";") %>%
  select(V1 = V6, V5)

sentiment_da$V1 <- gsub("'", "", sentiment_da$V1)
colnames(sentiment_da) <- c("word", "score")

#merge. 
merged_text <- merge(taler_count, sentiment_da, by = "word")

#here we calc Sentiment-scores on the speeches.. 
average_score_per_tale <- taler_count %>%
  group_by(tale) %>%
  summarise(total_weighted_score = sum(n * score),
            total_words = sum(n)) %>%
  mutate(average_score = total_weighted_score / total_words)

# VIsua tf_idf
top_10_df <- taler_count2 %>% 
  group_by(tale) %>% 
  top_n(n = 10, wt = tf_idf)

ggplot(data = top_10_df, aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = tale)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(facets = ~tale, ncol = 2, scales = "free")

# DFM og LDA
# Laver DFM
taler_tm <- taler_count %>% 
  cast_dfm(tale, word, n)

topic_model <- stm(taler_tm, K = 4, init.type = "Spectral")
summary(topic_model)

td_beta <- tidy(topic_model)

td_beta <- td_beta %>% 
  group_by(topic) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, beta))

ggplot(data = td_beta, aes(x = beta, y = term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free")

#Here we create the linear discriminant analysis...
taler_dtm <- taler_count %>% 
  cast_dtm(tale, word, n)

apLDA <- LDA(taler_dtm, k = 4, control = list(seed = 1234))

ap_topics <- tidy(apLDA, matrix = "beta")

ap_topics <- ap_topics %>%
  mutate(term = str_remove_all(term, "[^[:alpha:]]")) %>%
  filter(term != "")

# ap1,, arraging after topics.... (n)
ap1 <- ap_topics %>%
  filter(topic == 1) %>%
  arrange(desc(beta))

# ap2
ap2 <- ap_topics %>%
  filter(topic == 2) %>%
  arrange(desc(beta))

#ap3
ap3 <- ap_topics %>%
  filter(topic == 3) %>%
  arrange(desc(beta))

#ap4
ap4 <- ap_topics %>%
  filter(topic == 3) %>%
  arrange(desc(beta))

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# calc the gamma of topicmodel as factor
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(taler_tm))

# Laver til faktor
td_gamma$topic <- as.factor(td_gamma$topic)
td_gamma$document <- as.factor(td_gamma$document)

#Plotting calculated gamma. 
ggplot(td_gamma, aes(gamma, fill = topic)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3)

ggplot(data = td_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradient(low = "#ffffff", high = "#ff0000")

ggplot(head(ap1,10), aes(beta, term, fill = topic)) + 
  geom_col() +
  labs(x = "\nSandsynlighed for gentagende ord", y = "Ord",
       title = "Sprogbruget i tekstens opgaver vises at være ensartet",
       subtitle = "Analyse af tekstmæssig ensartethed i eksamensopgaven",
       caption = "\nData: AssociatedPress") +
  theme(plot.title = element_text(face = "bold"),
        text = element_text(family = "verdana"),
        legend.position = "none")

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

top_terms <- beta_wide %>% arrange(desc(log_ratio)) %>% head(10)
bottom_terms <- beta_wide %>% arrange(log_ratio) %>% head(10)
plot_data <- bind_rows(top_terms, bottom_terms)

#Plotting using ggplot and the data with calculations above - log ratio. 

ggplot(plot_data, aes(x = reorder(term, log_ratio), y = log_ratio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Terms",
    y = "Log2 ratio of beta in topic 2 / topic 1",
    title = "Top and Bottom Terms by Log2 Ratio"
  ) +
  theme_minimal()

# Here i create the wordcloud
df_count <- taler_count %>% 
  filter(tale == "aabningstale-1964")

png("test.png", width = 2000, height = 2000, res = 300)
wordcloud(words = df_count$word,
          freq = df_count$n,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"),
          scale=c(2,0.25))
dev.off()



