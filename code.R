#Data Wrangling 1
library(tidyverse)
install.packages("dslabs")
library(dslabs)
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

head(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  left_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

award <- AwardsPlayers %>% filter(yearID == 2016) %>%
  distinct(playerID)
award
awards <- intersect(top, award) %>% 
  select(playerID, HR, awardID)
  
awards


#Data Wrangling 2
library(tidyverse)
install.packages("dslabs")
library(dslabs)
library(rvest)
library(tidyverse)
library("pdftools")

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[7]] %>% html_table(fill = TRUE)
data.frame(polls)
polls <- rename(polls,"remain" = "leave", "leave" = "Leave")
str_replace(polls$undecided, "N/A", "0")
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date
                   
 #Data Wrangling 3

                   
             library(tidyverse)
install.packages("dslabs")
library(dslabs)
install.packages("Lahman")
library(Lahman)


library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[10]])

tab_1 <- html_table(nodes[[10]])
tab_1 <- tab_1[-c(1)]
head(tab_1)

tab_2 <- html_table(nodes[[19]])
head(tab_2)

tab_1 <- subset(tab_1, X2!="Team" & X3!="Payroll" & X4!="Average")
tab_2 <- subset(tab_2, X1!="Team" & X2!="Payroll" & X3!="Average")

tab_1 <- tab_1 %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- tab_2 %>% setNames(c("Team", "Payroll", "Average"))

full_join(tab_1, tab_2, by="Team")      
                   
                   
#Data Wrangling 4

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
install.packages("tidytext")
library(tidytext)
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)
library(sentiments)
library(textdata)
table(sentiments$lexicon)
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)
#Brexit Poll Dates
library(dslabs)
update.packages("dslabs")
library(lubridate)
data(brexit_polls)
brexit_polls %>% created_at >= ymd("2016-06-12") & 
  created_at < ymd("2016-06-18")
day <- weekdays(brexit_polls$enddate)
day
day %>% tibble(text = day) %>% unnest_tokens(word, text) %>%
  count(word) %>%
  arrange(desc(n))



#Movies
#seperate dates, label dates, unnest tokens, count, decend
data(movielens)
head(movielens)
#year
movielens %>% 
  count(year) %>%
  arrange(desc(n))


#time
x <- movielens %>%
  mutate(as_datetime(timestamp))
head(x)

x %>% rename(as_datetime(timestamp), "date")
  count(as_datetime(timestamp), sort = TRUE) %>%
  ggplot(aes(as_datetime(timesamp), n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



install.packages("gutenbergr")
install.packages("tidyverse")
install.packages("tidytext")
library(tidyverse)
library(gutenbergr)
library(tidytext)

options(digits = 3)
gutenberg_works %>% data.frame() %>% filter(str_detect(title, "Pride and Prejudice"))
pnp <- gutenberg_download(1342)
pnp1 <- pnp %>% unnest_tokens(word, text)
pnp1 <-  pnp1 %>% anti_join(stop_words) %>% tokens_select(words, "^\\d+$", selection = "remove")
pnp1
pnp1 %>% as.tibble() %>% count(word, sort=TRUE)


pnp1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
install.packages("afinn")
afinn <- "https://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010"
head(afinn)
get_sentiments("afinn")

#Data Wrangling 5
                   
   install.packages("dslabs")
library(dslabs)
install.packages("tidyverse")
library(tidyverse)
install.packages("pdftools")
library(pdftools)
install.packages("htmltools")
library(rvest)


options(digits = 3)    # report 3 significant digits
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

txt <- pdf_text(fn)
txt
h <- read_html(fn)
tab <- h %>% html_nodes("table")
class(tab)
tab


x <- str_split(txt[9], " \n ")
z <- x %>% as.tibble()
y <- z %>% html_table
class(x)
tab <- x %>% html_table()
class(tab)                

#Data Wrangling 6
    library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes("table")
tab
fn
txt <- pdf_text(fn)
txt[9]
x <- str_split(txt[9], "\n")
x
class(x)
length(x)
library(magrittr)
s <- x %>% '[['(1)
s
class(s)
length(s)

s <- s %>% str_trim
s
s[2]
header_index <- str_which(s, "2015")
header_index

class(s)
colnames(s)
head(s)
t(s)

month <- 
header <- s[2]
header

tail_index <- str_which(s, "Total")
tail_index

n <- str_count(s, pattern = "\\d+")
n

s <- str_remove_all(s, "[^\\d\\s]")
s

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s  
                   
                 
                   
                                   
