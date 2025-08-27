

df <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, fileEncoding = "Latin1") # select your CSV data, variable names need to be unique
library(dplyr)
library(tidyr)
library(stringr)


df1 <- df %>%
  # Ensure key columns are character
  mutate(
    Excerpt.Range = as.character(Excerpt.Range),
    Codes.Applied.Combined = as.character(Codes.Applied.Combined),
    Excerpt.Creator = as.character(Excerpt.Creator)
  ) 
  
  # Keep only first number in Excerpt.Range 
  # because we're not worried about matching the selection EXACTLY and the first number is most likely to match
df2 <- df1 %>% 
  mutate(Excerpt.Range = str_extract(Excerpt.Range, "^[0-9]+")) 
  
  # Split multiple codes into separate rows
df3 <- df2 %>% 
  separate_rows(Codes.Applied.Combined, sep = ",\\s*") %>%
  mutate(Codes.Applied.Combined = trimws(Codes.Applied.Combined))
  
   # Index within each coder/excerpt
df4 <- df3 %>% group_by(Media.Title, Excerpt.Range, Excerpt.Creator) %>%
  mutate(code_order = row_number()) %>%
  ungroup()
  
  # Pivot so coder names become columns, with named values_fn
df5 <- df4 %>% pivot_wider(
    id_cols = c(Media.Title, Excerpt.Range, code_order),
    names_from = Excerpt.Creator,
    values_from = Codes.Applied.Combined,
    values_fn = list(Codes.Applied.Combined = ~ first(.x))
  ) 

df4 <- df3 %>% as.data.frame() %>%  # ensure proper data frame for dplyr
  
  # Replace NAs with empty strings
  df6 <- df5 %>% 
  mutate(across(-c(Media.Title, Excerpt.Range, code_order), ~replace_na(.x, ""))) 
  
  # Create a row-wise match_key to collapse identical rows
df6 <- df5 %>% 
  mutate(match_key = apply(select(., -Media.Title, -Excerpt.Range, -code_order), 1,
                           function(x) paste(sort(x), collapse = "|"))) 
  
  # Collapse rows with identical codes across coders
df7 <- df6 %>% 
  group_by(Media.Title, Excerpt.Range, match_key) %>%
  summarise(across(-c(Media.Title, Excerpt.Range, match_key), ~first(.x)), .groups = "drop") %>%
  
  # Drop helper columns
  select(-match_key, -code_order)
