# 1) Basic String Manipulation -------------------------------------------------------------------------------

library(stringr)

fruit <- c("apple", "orange", "banana", "kiwi")

str_c("Fruit", fruit, sep=":") 
str_c(fruit, collapse = ", ")
str_sub(fruit, start = 1, end = 3)
str_sub(fruit, start = -1)

fruit_2 <- c("apple.orange", "banana.kiwi")
unlist(str_split(fruit_2, pattern = "."))
unlist(str_split(fruit_2, pattern = "\\."))

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
## %in% is shorthand way of saying, in this case, firstword = "dorothy" or firstword = "scarecrow". So, you can use a list of words in a filter so all those words are matched to 

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

# get sentiments - BING

frequency_words <- tidyWizard %>% 
  inner_join(sentiment, by = "word")
View(frequency_words)

?labs # modify axis, legend, and plot labels

View(afinnWizard)

### 5) Word clouds---------------------------------------------------------------------------------------

# row names
rownames(compCloud) <- compCloud$word # name the rows with the value in the word column, rather then a default number. 
compCloud <- select(compCloud, -word) # remove word column

head(compCloud)

comparison.cloud(compCloud, 
                 colors = c("darkred", "darkgreen"), 
                 max.words = 50)


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
