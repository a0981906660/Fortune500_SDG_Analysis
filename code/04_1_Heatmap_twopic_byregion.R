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
df.wordCount <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS", NAICS2, "_correct.RDS")) %>%
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
  mutate(name = as_factor(name))

# ---- Cluster companies by REGION on the y-axis -----------------------------
# Only the row ORDER changes. Every per_keyword / per_keyword_alt value stays
# exactly the same. Country comes from the master sheet; the non-breaking
# space in 'PBF Energy' is normalised so its name join does not silently fail.
ref_country <- readxl::read_excel(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"),
  sheet = "master"
) %>%
  transmute(name    = str_squish(str_replace_all(Name, "\u00a0", " ")),
            country = Country) %>%
  distinct(name, .keep_all = TRUE)

# ISO2 country -> region block (covers every country in the master sheet).
# Judgement calls: RU and TR -> Europe (HQ based); SA -> Asia (Middle East).
region_lookup <- c(
  US = "Americas", CA = "Americas", MX = "Americas", BR = "Americas",
  AT = "Europe", BE = "Europe", CH = "Europe", DE = "Europe", DK = "Europe",
  ES = "Europe", FI = "Europe", FR = "Europe", GB = "Europe", GR = "Europe",
  HU = "Europe", IE = "Europe", IT = "Europe", LU = "Europe", NL = "Europe",
  NO = "Europe", PL = "Europe", PT = "Europe", RU = "Europe", SE = "Europe",
  TR = "Europe", UK = "Europe",
  CN = "Asia", ID = "Asia", IN = "Asia", JP = "Asia", KR = "Asia",
  MY = "Asia", SA = "Asia", SG = "Asia", TH = "Asia", TW = "Asia", VN = "Asia",
  AU = "Oceania"
)

# Block sequence TOP -> BOTTOM: with facet_grid(rows = vars(region)) the region
# panels stack in this factor order, so the FIRST entry is the TOP band and the
# last is the BOTTOM band. Reorder this vector to change which region sits on
# top. Any company whose country is not in region_lookup falls into 'Other'.
region_order <- c("Americas", "Europe", "Asia", "Oceania", "Other")

df.plot <- df.plot %>%
  mutate(name = str_squish(as.character(name))) %>%
  left_join(ref_country, by = "name") %>%
  mutate(region = unname(region_lookup[country]),
         region = if_else(is.na(region), "Other", region),
         region = factor(region, levels = region_order)) %>%
  # within a region: keep firms together by country, then NAICS, then name
  arrange(region, country, naics, name) %>%
  mutate(name = factor(name, levels = unique(name)))

# sanity check: any company with no region? (should print 0)
cat(">>> companies with no region (expect 0):",
    df.plot %>% filter(region == "Other") %>% distinct(name) %>% nrow(), "\n")

## Just checking that names are "factor" now
# df.plot$name
df.plot$name %>% unique()


## Paginated save: split companies EVENLY across exactly TWO pages (2 heatmaps)
## Each company = 1 row, so each page's height scales with its own company count.
height_per_company <- 0.45   # inches per company row (lower this to shorten)
height_padding     <- 3      # inches for title, legend, axes
fig_dpi            <- 300     # 600 makes a very large PNG; 300 is plenty here

companies   <- levels(df.plot$name)
n_companies <- length(companies)

n_pages            <- 2
companies_per_page <- ceiling(n_companies / n_pages)  # even split: page 1 gets
# the extra row if n is odd

pages <- split(companies,
               ceiling(seq_along(companies) / companies_per_page))

for (page_i in seq_along(pages)) {
  page_companies <- pages[[page_i]]
  n_this_page    <- length(page_companies)
  plot_height    <- n_this_page * height_per_company + height_padding
  
  df.page <- df.plot %>%
    filter(name %in% page_companies) %>%
    droplevels()   # keep the REGION ordering set above; do NOT re-sort by naics
  
  suffix <- glue("_p{page_i}")   # always 2 pages -> _p1, _p2
  
  # p1: denominator = total words in doc (keyword density)
  # Region appears as a labelled band on the LEFT (facet strip); companies are
  # grouped within their region. coord_flip() is dropped so we can facet by
  # region down the y-axis with equal-height rows (space = "free_y").
  p1 <- df.page %>%
    ggplot(aes(x = sdg, y = name, fill = per_keyword)) +
    geom_tile() +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y",
               switch = "y") +
    theme_linedraw() +
    scale_linetype(guide = "none") +
    scale_fill_gradient(low = "snow", high = "red3",
                        labels = scales::percent) +
    labs(x = "SDG", y = "Company",
         title = "Mining, Quarrying, and Oil and Gas Extraction",
         fill = "Percentage (%)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing    = grid::unit(0.4, "lines")) +
    theme(legend.position   = "bottom",
          axis.text.y       = element_text(size = 18),
          axis.text.x       = element_text(size = 16),
          plot.title        = element_text(size = 24),
          axis.title.x      = element_text(size = 20),
          axis.title.y      = element_text(size = 20),
          legend.text       = element_text(size = 9),
          legend.title      = element_text(size = 18),
          strip.placement   = "outside",
          strip.background  = element_rect(fill = "grey85", colour = "grey40"),
          strip.text.y.left = element_text(angle = 0, size = 16,
                                           face = "bold", colour = "black"))
  
  ggsave(
    plot     = p1,
    filename = glue(
      "./data/result/Heatmap/fig_heatmap_NAICS{NAICS2}_byregion_labelled{suffix}.png"
    ),
    device = "png", dpi = fig_dpi, units = "in",
    height = plot_height, width = 19, limitsize = FALSE
  )
  
  #' @section denominator = sum of all SDG keywords (not total words)
  p2 <- df.page %>%
    ggplot(aes(x = sdg, y = name, fill = per_keyword_alt)) +
    geom_tile() +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y",
               switch = "y") +
    theme_linedraw() +
    scale_linetype(guide = "none") +
    scale_fill_gradient(low = "snow", high = "navy",
                        labels = scales::percent) +
    labs(x = "SDG", y = "Company",
         title = "Mining, Quarrying, and Oil and Gas Extraction",
         fill = "Percentage") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.spacing    = grid::unit(0.4, "lines")) +
    theme(legend.position   = "bottom",
          axis.text.y       = element_text(size = 18),
          axis.text.x       = element_text(size = 16),
          plot.title        = element_text(size = 24),
          axis.title.x      = element_text(size = 20),
          axis.title.y      = element_text(size = 20),
          legend.text       = element_text(size = 9),
          legend.title      = element_text(size = 18),
          strip.placement   = "outside",
          strip.background  = element_rect(fill = "grey85", colour = "grey40"),
          strip.text.y.left = element_text(angle = 0, size = 16,
                                           face = "bold", colour = "black"))
  
  ggsave(
    plot     = p2,
    filename = glue(
      "./data/result/Heatmap/fig_heatmap_alt_NAICS{NAICS2}_byregion_labelled{suffix}.png"
    ),
    device = "png", dpi = fig_dpi, units = "in",
    height = plot_height, width = 19, limitsize = FALSE
  )
}