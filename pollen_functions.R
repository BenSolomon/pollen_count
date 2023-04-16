library(dplyr)
library(here)
library(ggplot2)


parse_pollen_xlsx <- function(path){
  # Read xlsx
  df <- readxl::read_xlsx(path)

  # Find date
  date_col <- which(grepl("Date:", apply(df,1, paste, collapse = " ")))
  date_row <- which(grepl("Date:", apply(df,2, paste, collapse = " ")))
  date_coord <- paste(LETTERS[date_row + 1],date_col+1,sep="")
  date_coord <- paste(date_coord, date_coord, sep = ":")
  pollen_date <- readxl::read_xlsx(path, range = date_coord, col_names = "date") %>%
    pull(date)

  # Select only rows corresponding to species
  species_key <- readr::read_csv(here("data/species.csv"))
  species <- species_key$species
  df <- df[df[[1]] %in% species, ]

  # Select the count column
  df <- df %>%
    select_if(~!all(is.na(.))) %>%
    select(1,3)
  colnames(df) <- c("species", "count")

  # Apply the date
  df %>%
    mutate(date = pollen_date) %>%
    mutate(count = as.numeric(count)) %>%
    left_join(species_key, by = "species")
}


df <- tibble(file = list.files(here("data/pollen_data/"))) %>%
  mutate(path = here(sprintf("data/pollen_data/%s", file))) %>%
  mutate(data = purrr::map(path, ~suppressMessages(parse_pollen_xlsx(.)))) %>%
  tidyr::unnest(data) %>%
  distinct() %>%
  group_by(group, date, file) %>%
  summarise(count = sum(count, na.rm = T))

ggpollen <- function(df, grouping, max){
  df %>% filter(group == grouping) %>%
  ggplot(aes(x = date, y = count, group = group))+
    geom_path()+
    scale_y_continuous(limits = c(0,max)) +
    theme_classic() +
    ggtitle(grouping)
}

plt <- ggpollen(df, "tree", 1500)
layer_scales(plt)$x$range$range


df2 <- readr::read_csv(here("data/scale_key.csv")) %>%
  filter(scale == "very high") %>%
  mutate(max = min * 1.1) %>%
  mutate(plot = purrr::map2(group, max, function(g,m) ggpollen(df, g, m)))

df2$plot[1]

cowplot::plot_grid(plotlist = df2$plot)

