rm(list = ls())
library(tidyverse)
library(tidytext)
library(ggplot2)
source("./code/utils/func.R",encoding = "")
source("./code/utils/weakWords.R")
source("./code/config.R", encoding = '')

#df_final_key 如果要用SDSN 的Keyword改這邊
# TODO: pick up a set of keywords to use
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))

# IO of annual reports
df.Gvkey <- getGvkeyMap(glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"))


# Load the NAICS code you want
NAICS2 <- 21
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
  group_by(year, gvkey) %>%
  count(name) %>%
  mutate(name = str_replace(name, "\\d+\\s", "")) %>% 
  ungroup()
df.wordlen <- df.wordlen %>% 
  mutate(name = str_remove(name, "_20\\d{2}")) %>% 
  group_by(gvkey, name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


# Join two dataframes from script 1 and 2
df.word %>% head()
df_final_key %>% head()
# TODO: make sure the names of firms are aligned
df.wordCount <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS", NAICS2, ".rds")) %>%
  mutate(company_name = stringr::str_remove(name, "_\\d+$"),
         year = stringr::str_extract(name, "\\d{4}$")) %>%
  mutate(name = company_name) %>%
  select(-company_name) %>%
  # attach gvkey
  left_join(df.Gvkey, by = c("name" = "name"))

df.combine <- df.wordCount %>%
  left_join(df_final_key, by = c("keyword" = "word")) #%>% 
  # mutate(year = as.numeric(str_extract(name, "20\\d+")),
  #        name = str_replace(name, "_20\\d+", ""),
  #        # gvkey = as.numeric(str_extract(name, "\\d+")),
  #        name = str_replace(name, "\\d+\\s", "")) 


## Find the numerator regardless of year
## the group's primary key is firm's name and SDG category
df.long <- df.combine %>% 
  filter(n_keyword > 0) %>%
  group_by(gvkey, name, sdg) %>% 
  summarise(n_keyword = sum(n_keyword),
            gvkey = head(gvkey, 1)
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

# \begin{align*}
# \text{heatmap percentage}_{i, j} = \frac{ \sum_{t=1}^{T} \text{keyword mention}_{i, j, t} }{ \sum_{t=1}^{T} \text{all words in doc}_{i, j, t} }
# \end{align*}

df.long_join <- df.long %>% 
  left_join(df.wordlen, by = c("gvkey", "name")) %>%
  # sum keyword mention_{ijt} over t / sum total word in doc_{ijt} over t
  mutate(per_keyword = n_keyword/n) %>%
  # sum keyword mention alt_{ijt} over t / sum total keyword in doc_{ijt} over jt 
  group_by(gvkey, name) %>%
  mutate(n_alt = sum(n_keyword),
         per_keyword_alt = n_keyword / n_alt) %>%
  ungroup()

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
  left_join(df.Gvkey %>% select(-name), by = c('gvkey')) %>% 
  # mutate(name = paste0(name, " ", naics)) %>% 
  mutate(name = as_factor(name)) %>% 
  mutate(name = fct_reorder(name, naics))
  
## Just checking that names are "factor" now
# df.plot$name
df.plot$name %>% unique()


## Paginated save: split into pages when there are too many companies
## Each company = 1 row in the heatmap, so height scales with n_companies
companies_per_page <- 30     # companies per page
height_per_company <- 0.45   # inches per company row
height_padding     <- 3      # inches for title, legend, axes

companies   <- levels(df.plot$name)
n_companies <- length(companies)

pages <- split(companies,
               ceiling(seq_along(companies) / companies_per_page))

for (page_i in seq_along(pages)) {
  page_companies <- pages[[page_i]]
  n_this_page    <- length(page_companies)
  plot_height    <- n_this_page * height_per_company + height_padding

  df.page <- df.plot %>%
    filter(name %in% page_companies) %>%
    droplevels() %>%
    mutate(name = fct_reorder(name, naics))

  suffix <- if (length(pages) > 1) glue("_p{page_i}") else ""

  # p1: denominator = total words in doc
  p1 <- df.page %>%
    ggplot(aes(x = name, y = sdg, fill = per_keyword)) +
    geom_tile() +
    coord_flip() +
    theme_linedraw() +
    scale_linetype(guide = "none") +
    scale_fill_gradient(low = "snow", high = "red3",
                        labels = scales::percent) +
    labs(x = "Company", y = "SDG",
         title = "Mining, Quarrying, and Oil and Gas Extraction",
         fill = "Percentage (%)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = "bottom",
          axis.text.y     = element_text(size = 18),
          axis.text.x     = element_text(size = 16),
          plot.title      = element_text(size = 24),
          axis.title.x    = element_text(size = 20),
          axis.title.y    = element_text(size = 20),
          legend.text     = element_text(size = 9),
          legend.title    = element_text(size = 18))

  ggsave(
    plot     = p1,
    filename = glue(
      "./data/result/Heatmap/fig_heatmap_NAICS{NAICS2}{suffix}.png"
    ),
    device = "png", dpi = 600, units = "in",
    height = plot_height, width = 19
  )

  #' @section denominator = sum of all SDG keywords (not total words)
  p2 <- df.page %>%
    ggplot(aes(x = name, y = sdg, fill = per_keyword_alt)) +
    geom_tile() +
    coord_flip() +
    theme_linedraw() +
    scale_linetype(guide = "none") +
    scale_fill_gradient(low = "snow", high = "navy",
                        labels = scales::percent) +
    labs(x = "Company", y = "SDG",
         title = "Mining, Quarrying, and Oil and Gas Extraction",
         fill = "Percentage") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = "bottom",
          axis.text.y     = element_text(size = 18),
          axis.text.x     = element_text(size = 16),
          plot.title      = element_text(size = 24),
          axis.title.x    = element_text(size = 20),
          axis.title.y    = element_text(size = 20),
          legend.text     = element_text(size = 9),
          legend.title    = element_text(size = 18))

  ggsave(
    plot     = p2,
    filename = glue(
      "./data/result/Heatmap/fig_heatmap_alt_NAICS{NAICS2}{suffix}.png"
    ),
    device = "png", dpi = 600, units = "in",
    height = plot_height, width = 19
  )
}


