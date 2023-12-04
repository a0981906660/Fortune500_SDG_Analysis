rm(list = ls())
library(tidyverse)
library(tidytext)
library(ggplot2)
source("./code/utils/func.R",encoding = "")
source("./code/utils/weakWords.R")

#df_final_key 如果要用SDSN 的Keyword改這邊
# TODO: pick up a set of keywords to use
df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")
# df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

# Load the NAICS code you want
## For example:
## df.doc <- readReports(NAICS2_CODE = 31)
## df.doc <- readReports(NAICS2_CODE = 21)
## df.doc <- readReports(NAICS2_CODE = 33)
NAICS2 <- 31
# We have name, rank, year, value (the content of the report) in the following datafram
df.doc <- readReports(NAICS2_CODE = NAICS2)


# Here we tokenize the content of the report 
df.word <- df.doc %>% 
  unnest_tokens(output = word, input = value, token = "words") %>% 
  filter(!word %in% stopwords::stopwords()) %>% 
  filter(!word %in% base::letters) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d+\\w")) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% weak_words)

# Here we compute the total number of words used in the reports for a firm within a year
# this number is going to be the denominator
df.wordlen <- df.word %>%
  group_by(year, rank) %>%
  count(name) %>%
  mutate(name = str_replace(name, "\\d+\\s", "")) %>% 
  ungroup()
df.wordlen <- df.wordlen %>% 
  mutate(name = str_remove(name, "_20\\d{2}")) %>% 
  group_by(rank, name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


# Join two dataframes from script 1 and 2
df.word %>% head()
df_final_key %>% head()
# TODO: make sure the names of firms are aligned
df.wordCount <- read_rds(paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2, ".rds"))

df.combine <- df.wordCount %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>% 
  mutate(year = as.numeric(str_extract(name, "20\\d+")),
         name = str_replace(name, "_20\\d+", ""),
         rank = as.numeric(str_extract(name, "\\d+")),
         name = str_replace(name, "\\d+\\s", ""))


## Find the numerator regardless of year
## the group's primary key is firm's name and SDG category
df.long <- df.combine %>% 
  filter(n_keyword > 0) %>%
  group_by(rank, name, sdg) %>% 
  summarise(n_keyword = sum(n_keyword),
            rank = head(rank, 1)
            ) %>% 
  ungroup()

## combine the 
# df.long_join <- df.long %>% 
#   left_join(df.wordlen %>% 
#               group_by(rank, name) %>% 
#               summarise(n = sum(n)) %>% 
#               mutate(year = str_extract(name, "_20\\d{2}"),
#                      year = as.numeric(str_remove(year, "_")),
#                      name = str_remove(name, "_20\\d{2}")) %>% 
#               ungroup(),
#             by = c("rank", "name")
#             ) %>%
#   mutate(per_keyword = n_keyword/n)
df.long_join <- df.long %>% 
  left_join(df.wordlen, by = c("rank", "name")) %>%
  mutate(per_keyword = n_keyword/n)

# Plot
df.plot <- df.long_join %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>% 
  filter(!is.na(sdg))

# manually change company names
# df.plot <- df.plot %>% 
  # mutate(name = ifelse(name == "Oil", "Oil & Natural Gas", name))

## assign factor level to the names
df.plot <- df.plot %>% 
  # put NAICS code back by merging two dataframe
  left_join(df.RankCode %>% select(-name), by = c('rank')) %>% 
  # mutate(name = paste0(name, " ", naics)) %>% 
  mutate(name = as_factor(name)) %>% 
  mutate(name = fct_reorder(name, naics))
  
## Just checking that names are "factor" now
# df.plot$name
df.plot$name %>% unique()


# sort companies by NAICS code
p1 <- df.plot %>%
  ggplot(aes(x = name, y = sdg, fill = per_keyword)) + 
  # grid plot
  geom_tile() +
  coord_flip() +
  theme_bw() +
  scale_linetype(guide = "none") +
  scale_fill_gradient(low = "snow", high = "navyblue", # change color
                      labels = scales::percent,
                      breaks=c(0, 0.0025, 0.005) # breaks indicate percentile
  ) +
  labs(x = "Company", y = "SDG", 
       title = "Mining, Quarrying, and Oil and Gas Extraction", 
       fill = "percentage") +
  # fill in colors in blank grids
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom",
        #這兩行調x y 軸字大小
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 8)) #+
  
# set font
  # theme(plot.title = element_text(family = "Noto Sans CJK TC Medium", face = "plain", size = 18),
  #       legend.text = element_text(family = "Noto Sans CJK TC Medium", face = "plain"), 
  #       text = element_text(family = "Noto Sans CJK TC Medium"))
p1

## Save plot
p1 %>% 
  ggsave(filename = paste0("./data/result/fig_heatmap_NAICS", NAICS2, ".png"), 
         device = "png",
         dpi = 300, 
         units = "in",
         # 調圖片長寬比（圖片大小）
         height = 6, width = 11)

