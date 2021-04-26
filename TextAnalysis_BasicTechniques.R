
## INTRO: Package installation ------------------------------------------------------------------------------- 

install.packages(c("tidyverse", "tidytext", "gutenbergr", "SnowballC", "ggraph", "igraph", "wordcloud", "topicmodels", "textdata"))

## 1) Basic String Manipulation -------------------------------------------------------------------------------

library(stringr)

fruit <- c("apple", "orange", "banana", "kiwi")

?str_c # function for joining multiple string into a single string
str_c("Fruit", fruit, sep=":") 
str_c(fruit, collapse = ", ")

?str_sub # Extract and replace substrings from a character vector
str_sub(fruit, start = 1, end = 3)
str_sub(fruit, start = -1)

?str_replace # replace matched patterns in a string
str_replace(fruit, pattern = "a", replacement = "A")

?unlist # escape characters, flatten lists, creation of a vector containing all the atomic components which occur in x. 
?str_split # split up a string into pieces
fruit_2 <- c("apple.orange", "banana.kiwi")
unlist(str_split(fruit_2, pattern = "."))
unlist(str_split(fruit_2, pattern = "\\."))

?str_subset # Keep strings matching a pattern or find positions
?str_extract # Extract matching patterns from a string
?str_replace_all # Replace matched patterns in a string

user_ids <- c("User123", "User234", "hello world", "banana", "User345")
str_subset(user_ids, pattern = "\\d")
str_extract(user_ids, pattern = "\\d{3}")
str_extract(user_ids, pattern = "[A-Z a-z]+")
str_subset(user_ids, pattern = "bann?ana")
str_replace_all("abcd2ef7gh9ij2klmn98op", 
                pattern = "[1-9]+", 
                replacement = "")

### 2) Tidying and Summarising Text Data -------------------------------------------------------------------------------

# download a book as data.frame

library(gutenbergr)

gutenberg_works(title == "The Wonderful Wizard of Oz") 
wizardOfOz <- gutenberg_download(55, mirror = "http://mirrors.xmission.com/gutenberg/") # download text
head(wizardOfOz) 
View(wizardOfOz)

# convert text into tokenised format, this means a low level as a meaningful unit of text

library(tidytext)
library(dplyr)

?unnest_tokens # split a column into tokens
?count # count observation by group
# %>%: pipe operator making the workflow readable

tidyWizard <- wizardOfOz %>% unnest_tokens(word, text)
head(tidyWizard)
View(tidyWizard)
tidyWizard %>% count(word, sort = TRUE) %>% head()

# remove stop words
head(stop_words)
View(stop_words)

?anti_join # ? 

tidyWizard %>%
  anti_join(stop_words, by = "word") %>% # remove stopwords like articles
  count(word, sort = TRUE) %>%
  head()

# remove other specific terms
characters <- data.frame(word = c("dorothy", "scarecrow", "woodman",
                                  "lion", "tin", "witch", "toto"))

tidyWizardNoCharacters <-  tidyWizard %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(characters, by = "word") %>%
  count(word, sort = TRUE)

head(tidyWizardNoCharacters)
View(tidyWizardNoCharacters)

# stemming 

library(SnowballC)

?wordStem # get the stem of words

wordStem(c("fear", "fearing", "fearful", "play", "played", "playing"))

tidyWizardStemmed <-  tidyWizard %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>% # transform the words in stems
  count(word, sort = TRUE)

head(tidyWizardStemmed)
View(tidyWizardStemmed)

## 3) Ngrams (relationships between words) = tokenise pairs of adjacent words rather that individual words -------------------------------------------------------------------------------

# tokenise into bigrams
tidyWizardNgram <- wizardOfOz %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) # take text (input) column and covert in word columm (output), with unit "ngrams" or 2 words. 

tidyWizardNgram  %>%
  count(word, sort = TRUE) %>%
  head()

View(tidyWizardNgram)

# make tidy ngram data
tidyWizardNgram  <- wizardOfOz %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord"), sep = " ") # separate the words in two columns

head(tidyWizardNgram)
View(tidyWizardNgram)

# remove stop words
tidyWizardNgram <- tidyWizardNgram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word"))

head(tidyWizardNgram)
View(tidyWizardNgram)

# 4) Visualising related words -------------------------------------------------------------------------------

library(ggraph)
library(igraph)
library(tidyr)

# Filter so only common pairs about Dorothy or the scarecrow remain
tidyWizardNgram <- tidyWizardNgram %>%
  filter((firstWord  %in% c("dorothy", "scarecrow")) | # OR
           secondWord %in% c("dorothy", "scarecrow"), # %in% ??? Explanation...
         n > 1)

?filter

head(tidyWizardNgram)
View(tidyWizard)

# Create the igraph object
igraph_wizard <- graph_from_data_frame(tidyWizardNgram)   

?graph_from_data_frame # Creating igraph graphs from data frames or vice-versa

# Plot the ggraph  
ggraph(igraph_wizard, layout = 'stress') + # More layouts: http://mr.schochastics.net/netVizR.html
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) + # aes(edge_alpha = n) ??? Explanation...
  geom_node_point(color = "coral", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

?ggraph() # create a ggraph plot 
?aes # construct aesthetic mappings, describe how variables in the data are mapped to visual proprieties (from ggplot2)
?edge_alpha
?layout # The “stress” layout is part of the graphlayouts package and is always a safe choice 
# since it is deterministic and produces nice layouts for almost any graph. 
?geom_edge_link # Draw edges as straight lines between nodes
?geom_node_point() # Show nodes as points
?geom_node_text # Annotate nodes with text
?theme_void # complete themes which control all non-data display

## 4) Sentiment Analysis ---------------------------------------------------

library(gutenbergr)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(wordcloud)

# get tidy data
wizardOfOz <- gutenberg_download(55, mirror = "http://mirrors.xmission.com/gutenberg/")

tidyWizard <- wizardOfOz %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(tidyWizard)
View(wizardOfOz)

# get sentiments - BING

?get_sentiments # get a tidy data frame of a single sentiment lexicon
?inner_join # mutating join
?group_by # group by one or more variables
?summarise # summarize each group to fewer rows

sentiment <- get_sentiments(lexicon = "bing") # other oprtions instead of bing are, afinn, loughran
View(sentiment)

frequency_words <- tidyWizard %>% 
  inner_join(sentiment, by = "word")
View(frequency_words)

tidyWizard %>% 
  inner_join(sentiment, by = "word") %>% 
  group_by(sentiment) %>% 
  summarise(total = sum(n)) # get how many words are negative and how many are positive in the Console

# quantitative results - AFINN

tidyWizard %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word") %>% 
  head()

afinnWizard <- wizardOfOz %>% 
  mutate(lineNumber = row_number()) %>%   # add row number to df
  unnest_tokens(word, text) %>% # tokenise
  anti_join(stop_words) %>% # remove stop words
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")

afinnWizard <- afinnWizard %>% 
  mutate(booksection = lineNumber %/% 100) %>% # do not understand this bit ??? Explanation...
  group_by(booksection) %>% 
  summarise(score = mean(value))

afinnWizard %>% ggplot(aes(booksection, score)) +
  geom_point() +
  geom_line() + 
  labs(title = "Sentiment throughout the Wizard of Oz",
       y = "Average Afinn Sentiment Score")

?labs # modify axis, legend, and plot labels

View(afinnWizard)

### 5) Word clouds----------------------------------------------------------------------------------------

library(wordcloud)

cloudWizard <- wizardOfOz %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

View(cloudWizard)

wordcloud(words = cloudWizard$word, 
          freq = cloudWizard$n, 
          max.words = 50, 
          colors = "cornflowerblue")

# comparison cloud

# term-frequency matrix
compCloud <- wizardOfOz %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(lexicon = "bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>%
  pivot_wider(names_from = "sentiment", 
              values_from = "n", 
              values_fill = list(n = 0)) %>% 
  data.frame()

View(compCloud)
?pivot_wider # pivot data from long to wide, increase the number of columns and decreases the number of rows, (<> pivot_longer)
# names_from or values_from: A pair of arguments describing which column (or columns) to get the name of the output column (names_from)
# and which column (or columns) to get the cell values from (values_from).
# values_fill: Optionally, a (scalar) value that specifies what each value should be filled in with when missing. This can be a 
# named list if you want to apply different aggregations to different value columns.

# row names
rownames(compCloud) <- compCloud$word # name the rows with the value in the word column, rather then a default number. 
compCloud <- select(compCloud, -word) # remove word column

head(compCloud)

comparison.cloud(compCloud, 
                 colors = c("darkred", "darkgreen"), 
                 max.words = 50)


## 06) Word Document Frequency -------------------------------------------------------------------------------------------

# import the text
wizardOfOz <- gutenberg_download(55, mirror = "http://mirrors.xmission.com/gutenberg/")

# make into tidy form and add line numbers
tidyWizard <- wizardOfOz %>%
  mutate(lineNumber = row_number()) %>% 
  unnest_tokens(word, text)

View(tidyWizard)

# label into sections by line number
tidySplitWizard_1 <- tidyWizard %>%
  mutate(booksection = lineNumber %/% 1000)  

View(tidySplitWizard_1)

# add document-term count
tidySplitWizard_2 <- tidySplitWizard_1 %>%
  group_by(word, booksection) %>%
  summarise(count = n()) %>%
  ungroup() %>% # empty because it contains only the tidySplitWizard_1 data
  arrange(-count) # arrange rows by column values, minus indicate descending order

?ungroup # ungroup by one or more variables

View(tidySplitWizard_2)

# apply TF-IDF
tidySplitWizard <- tidySplitWizard_2 %>%
  bind_tf_idf(term = word, document = booksection, n = count) 

?bind_tf_idf # Bind the term frequency and inverse document frequency of a tidy text dataset to the dataset 
# term = Column containing terms as string or symbol
# document = Column containing document IDs as string or symbol
# n = Column containing document-term counts as string or symbol

head(tidySplitWizard, 8)
View(tidySplitWizard)

# Visualise top word by section

tidySplitWizard %>%
  group_by(booksection) %>%
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = booksection),
             y = tf_idf)) +
    geom_col(show.legend = FALSE, fill = "azure4") +
    facet_wrap(vars(booksection), ncol = 2, scales = "free_y") +
    coord_flip() +
    scale_x_reordered("words")

?top_n # select top or bottom n rows by value
?reorder_within # reorder an x or y axis within facets 
?geom_col # If you want the heights of the bars to represent values in the data, use geom_col() instead
?facet_wrap # facet_wrap() wraps a 1d sequence of panels into 2d.
?vars # Just like aes(), vars() is a quoting function that takes inputs to be evaluated in the context of a dataset. 
?coord_flip # Cartesian coordinates with x and y flipped
?scale_x_reordered # Reorder an x or y axis within facets

### 07 Web Scraping ---------------------------------------------------------

library(rvest)

# Read from local copy
WikiPage <- read_html("https://en.wikipedia.org/wiki/World_population")

# or read from local file
# WikiPage <- read_html("../data/WorldPopulationWikipedia.html")

WikiNodes <- html_nodes(WikiPage, "h2 .mw-headline")
?html_nodes # select nodes from an HTML document
View(WikiNodes)

?html_text ?html_attr  # Extract attributes, text and tag name from html. The result is equivalent to the sections titles of the Wiki page.
HTML_text <- html_text(WikiNodes)
html_attr(WikiNodes, "id")
html_attr(WikiNodes, "class")

print(HTML_text)

## tables

# read all html tables
WikiDfList <- html_table(WikiPage, header = TRUE, fill = TRUE)
?html_table # Parse an html table into a data frame

# print just the 6th
WikiDfList[6] # number indicate the table selected, this webpage has a total of 26 tables
WikiDfList[8]

# gather all the html table nodes
WikiTables <- html_nodes(WikiPage, "table")

# convert just the 6th into a data frame
html_table(WikiTables[6], header = TRUE)
