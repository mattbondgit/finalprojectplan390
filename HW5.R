library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)
speeches = read_csv("presidential_speeches_sample.csv")
view(speeches)
tokens = unnest_tokens(speeches, word, content)
head(tokens)
tokens = anti_join(tokens, stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 15, control=list(seed=42))
beta = tidy(model, matrix="beta")
View(beta)
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
unique(filter(tokens, lemma=="american")$word)
custom_stop_words = bind_rows(
  stop_words, 
  tibble(word=c("people", "america", "americans", "american", "laughter", "president",
                "country", "nation" ))
)
#Filtered out these words because they would be used in every speech
tokens = anti_join(tokens, custom_stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 15, control=list(seed=42))
beta = tidy(model, matrix="beta")
View(beta)
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
gamma = tidy(model, matrix="gamma")
filter(gamma, topic==1) %>% arrange(-gamma)
 #Topic 1 seems to be about crime like gun violence and drug abuse
filter(gamma, topic==13) %>% arrange(-gamma)
  #Topic 13 is about the war on terror and war in Iraq
filter(gamma, topic==15) %>% arrange(-gamma)
  #Topic 15 is about international diplamacy
speeches[,c("year")] = str_match(speeches$document, "[[:digit:]]{4}")
  #Used this regular expression to create the year column 
speeches
view(speeches)
speeches$year
as.numeric(str_extract(speeches$year, "[[:digit:]]{4}"))
speeches = mutate(speeches, year=as.character(str_extract(speeches$year, "[[:digit:]]{4}")))
tokens = unnest_tokens(speeches, word, content)
head(tokens)
tokens = anti_join(tokens, custom_stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 15, control=list(seed=42))
beta = tidy(model, matrix="beta")
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
gamma2 = left_join(speeches, gamma, on="documents")
 #leftjoined these two so the year column could be with gammas
ggplot(gamma2, aes(y=gamma, x=year)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
 #Did this to graph the frequency these topics showed up in speeches by year
unique(speeches$year)


