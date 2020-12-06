library(tidyverse)
library(tidytext)
library(rethinking)
library(stringi)
library(magrittr)
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

df <- read_csv('td_data.csv')

# Look at the most commonly occurring comments.
df %>% filter(text != '') %>% group_by(text) %>% count() %>% arrange(desc(n))

# Group by text & date & summarize by selecting last observation.
df <- df %>%
  filter(text != '') %>%
  group_by(text, date) %>%
  summarize(upvotes = last(upvotes), downvotes = last(downvotes))

# Categorize comments based on their topics.
df <- df %>% mutate(
  contains_tucker = if_else(str_detect(text, fixed('tucker', ignore_case = TRUE)) ||
                              str_detect(text, fixed('carlson', ignore_case = TRUE)), T, F),
  contains_fox = if_else(str_detect(text, fixed('fox', ignore_case = TRUE)) ||
                           str_detect(text, fixed('faux', ignore_case = TRUE)), T, F),
  contains_oann = if_else(str_detect(text, fixed('oann', ignore_case = TRUE)) ||
                            str_detect(text, fixed('one america news', ignore_case = TRUE)), T, F),
  contains_newsmax = if_else(str_detect(text, fixed('newsmax', ignore_case = TRUE)), T, F),
  contains_cnn = if_else(str_detect(text, fixed('cnn', ignore_case = TRUE)), T, F)
  )

# Get some comments about CNN to make sure we're actually capturing the mentions correctly.
df %>% filter(contains_cnn) %>% select(text)

# We consider a comment to have happened post-election iff it was posted after midnight
# on the eve of Election Day. That's also around the time when Fox News called Arizona
# for Biden.
# Midnight ET translates to 5 am UTC.
df <- df %>% mutate(is_post_election = ifelse(date > lubridate::ymd_hms('20201105T05:00:00'), T, F))

# We also do the same thing for the end of Tucker Carlson's segment critiquing Sidney Powell.
# 9 pm ET (which is 1h after the show's start) translates to 2 am UTC.
df <- df %>% mutate(is_post_tucker_critique = ifelse(date > lubridate::ymd_hms('20201121T02:00:00'), T, F))

# Make sure that the cut-off dates are correct.
# min((df %>% filter(is_post_election) %>% select(date))$date)
# min((df %>% filter(is_post_tucker_critique) %>% select(date))$date)

# Calculate log difference of upvotes & downvotes & scale it.
df$vote_score <- log(df$upvotes + 1) - log(df$downvotes + 1)
df$vote_score <- scale(df$vote_score)

# Convert text column to UTF-8.
df$text <- df$text %>% stri_enc_toutf8(is_unknown_8bit = TRUE, validate = TRUE) %>% str_trim()

# Analyse sentiment of each comment. This may require a lot of memory?
# See https://stackoverflow.com/a/52612921/1077679
(out <- with(
  df, 
  sentiment_by(
    get_sentences(text), 
    list(date, text)
  )
))
df$sentiment <- out$ave_sentiment

# write_csv(df, 'td_data_processed.csv')
# df <- read_csv('td_data_processed.csv')

# Plot the distributions of the score & sentiment values.
dens(df$upvotes)
dens(df$downvotes)
dens(df$vote_score)
dens(df$sentiment)

# Look at the ten most positive & the ten most negative comments about Fox.
df %>%
  filter(contains_fox && !contains_oann && !contains_newsmax) %>%
  arrange(desc(sentiment)) %>%
  top_n(10, wt = sentiment) %>%
  select(text, sentiment)

df %>%
  filter(contains_fox && !contains_oann && !contains_newsmax) %>%
  arrange(sentiment) %>%
  top_n(10, wt = sentiment) %>%
  select(text, sentiment)

# Plot distribution of sentiment before & after election.
ggplot(df %>% filter(contains_fox && !contains_oann && !contains_newsmax) %>% filter(vote_score > 0)) + 
  geom_density(aes(sentiment, color = is_post_election, fill = is_post_election), alpha = .2) +
  xlim(-1, 1) +
  labs(x = "Sentiment towards Fox News",
       y = "Density",
       caption = "Sentiment towards Fox News before & after 2020 U.S. election night.\nDrawn from thedonald.win. Includes upvoted comments only. N = 294.") +
  scale_fill_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  scale_color_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), legend.position = "bottom")
ggsave('sentiment_density_fox.png', width = 5, height = 4, dpi = 600)

ggplot(df %>% filter(contains_oann && !contains_fox && !contains_newsmax) %>% filter(vote_score > 0)) + 
  geom_density(aes(sentiment, color = is_post_election, fill = is_post_election), alpha = .2) +
  xlim(-1, 1) +
  labs(x = "Sentiment towards OANN",
       y = "Density",
       caption = "Sentiment towards OANN before & after 2020 U.S. election night.\nDrawn from thedonald.win. Includes upvoted comments only. N = 20.") +
  scale_fill_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  scale_color_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), legend.position = "bottom")

ggplot(df %>% filter(contains_newsmax && !contains_oann && !contains_fox) %>% filter(vote_score > 0)) + 
  geom_density(aes(sentiment, color = is_post_election, fill = is_post_election), alpha = .2) +
  xlim(-1, 1) +
  labs(x = "Sentiment towards Newsmax",
       y = "Density",
       caption = "Sentiment towards Newsmax before & after 2020 U.S. election night.\nDrawn from thedonald.win. Includes upvoted comments only. N = 30.") +
  scale_fill_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  scale_color_discrete(name = element_blank(), labels = c("Before election night", "After election night")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), legend.position = "bottom")

# Plot distribution of sentiment of all 3 broadcasters.
ggplot(df %>%
         filter(vote_score > 0) %>%
         filter((contains_oann && !contains_fox && !contains_newsmax) ||
                       (!contains_oann && contains_fox && !contains_newsmax) ||
                       (!contains_oann && !contains_fox && contains_newsmax))) + 
  geom_density(aes(sentiment,
                   ..count..,
                   color = interaction(contains_oann, contains_fox, contains_newsmax),
                   fill = interaction(contains_oann, contains_fox, contains_newsmax)),
               alpha = .2) +
  xlim(-1, 1) +
  labs(x = "Sentiment",
       y = "Density",
       caption = "Sentiment towards OANN, Fox News & Newsmax around the 2020 U.S. election.\nDrawn from thedonald.win. Includes upvoted comments only. N = 344.") +
  scale_fill_discrete(name = element_blank(), labels = c("OANN", "Fox News", "Newsmax")) +
  scale_color_discrete(name = element_blank(), labels = c("OANN", "Fox News", "Newsmax")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), legend.position = "bottom")
ggsave('sentiment_density_all_networks.png', width = 5, height = 4, dpi = 600)

# Get mean sentiment for each network & datetime.
ggplot(df %>% filter(contains_fox) %>% filter(score_ratio > 0),
       aes(date, sentiment)) +
  geom_point(alpha = .5)
 #  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')

# Calculate mean by date & network.
df_by_day <- df %>%
  mutate(day = lubridate::date(date)) %>%
  filter(vote_score > 0) %>%
  filter(contains_fox) %>%
  group_by(day) %>%
  summarize(sentiment_mean = mean(sentiment),
            sentiment_iqr = IQR(sentiment, na.rm = TRUE),
            rating_mean = mean(vote_score),
            rating_iqr = IQR(vote_score, na.rm = TRUE),
            count = n())

# Get mean sentiment for each network & datetime.
ggplot(df_by_day, aes(day, sentiment_mean, alpha = count)) +
  geom_point() +
  geom_linerange(aes(ymin = sentiment_mean - sentiment_iqr/2,
                 ymax = sentiment_mean + sentiment_iqr/2)) +
  theme_minimal()

df %>% filter(sum(contains_fox, contains_oann, contains_newsmax) == 1)

##
# Build Bayesian model for estimating attitudes.

# Prepare dataframe for putting into Stan, converting all network/time period booleans
# into a factor variable.
df_for_stan <- df %>%
  filter((contains_fox && !contains_oann && !contains_newsmax) ||
           (!contains_fox && contains_oann && !contains_newsmax) ||
           (!contains_fox && !contains_oann && contains_newsmax)) %>%
  mutate(network = case_when(!is_post_election && contains_fox ~ 1,
                             is_post_election && contains_fox ~ 2,
                             !is_post_election && contains_oann ~ 3,
                             is_post_election && contains_oann ~ 4,
                             !is_post_election && contains_newsmax ~ 5,
                             is_post_election && contains_newsmax ~ 6))

# Draw assumed causal relationships. A = Attitude, S = Sentiment, V = Voting score.
library(dagitty)
dag <- dagitty('dag {
  A [pos="1,0"]
  S [pos="0,1"]
  V [pos="2,1"]
  
  A -> S
  A -> V <- S
}')
plot(dag)
adjustmentSets(dag, exposure = "A", outcome = "S")
adjustmentSets(dag, exposure = "A", outcome = "V")

# Build the statistical model. It's a multi-level model with partial pooling.
m <- ulam(
  alist(
    vote_score ~ dnorm(mu1, sigma1),
    mu1 <- a + (mu2_bar + z[network] * sigma3) * sentiment,
    sentiment ~ dnorm(mu2_bar + z[network] * sigma3, sigma2),
    
    # Priors
    a ~ dnorm(0, 10),
    z[network] ~ dnorm(0, 1),
    mu2_bar ~ dnorm(0, 1.5),
    sigma1 ~ dexp(1),
    sigma2 ~ dexp(1),
    sigma3 ~ dexp(1),
    
    gq> vector[network]:mu2 <<- mu2_bar + z * sigma3
  ),
  data = df_for_stan[, c("sentiment", "vote_score", "network")],
  chains = 4,
  cores = 4,
  log_lik = TRUE,
  iter = 3000,
  control = list(adapt_delta = .99)
)

precis(m, 2)
pairs(m)

# Sample posterior distributions & analyse them.
post <- extract.samples(m, n = 1e4)
precis(post, 2)
plot(precis(m, 2, pars = "mu2"))

# Get contrasts between post-election/pre-election attitudes.
diffs <- as.data.frame(list(
  db12 <- post$mu2[,2] - post$mu2[,1],
  db34 <- post$mu2[,4] - post$mu2[,3],
  db56 <- post$mu2[,6] - post$mu2[,5]
), col.names = c('fox_news','oann','newsmax'))
plot(precis(diffs), labels = c("Fox News", "OANN", "Newsmax"))

# Calculate probability that a contrast sample lies below/above 0.
sum(diffs$fox_news < 0) / length(diffs$fox_news)
sum(diffs$oann > 0) / length(diffs$oann)
sum(diffs$newsmax < 0) / length(diffs$newsmax)

# Prepare a data frame for plotting.
diffs_by_network <- data.frame(network = c('Fox News', 'OANN', 'Newsmax'),
                               mean = sapply(diffs, mean),
                               pi_low = sapply(diffs, function (x) { PI(x, prob = .93)[1] }),
                               pi_high = sapply(diffs, function (x) { PI(x, prob = .93)[2] }))

# Plot mean & 93% percentile interval contrasts.
ggplot(diffs_by_network, aes(network, mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(ymin = pi_low, ymax = pi_high)) +
  ylim(-.2, .2) +
  labs(x = element_blank(),
       y = "Post/pre-election contrast",
       caption = "Mean & 93% percentile interval contrasts between post/pre-election attitude posteriors.\nValues below 0 indicate attitude got more negative after election night. N = 749.") +
  theme_classic()
ggsave('model_contrasts.png', width = 5, height = 3, dpi = 600)
