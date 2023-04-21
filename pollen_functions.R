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

ggpollen <- function(grouping){
  scale_df <- readr::read_csv(here("data/scale_key.csv")) %>%
    filter(group == grouping)

  max <- scale_df %>%
    filter(scale == "very high") %>%
    mutate(max = min * 1.1) %>%
    pull(max)

  annotation_layers <- scale_df %>%
    mutate(color = case_when(
      scale == "low" ~"green",
      scale == "moderate" ~ "yellow",
      scale == "high" ~ "red",
      scale == "very high" ~ "purple",
    )) %>%
    mutate(layer = purrr::pmap(
      list(min, max, color),
      function(min, max, color){
        if (is.na(max)) {max <- Inf}
        annotate("rect", ymin=min, ymax=max,
                 xmin=as.POSIXct(-Inf), xmax=as.POSIXct(Inf),
                 fill = color, alpha = 0.2)

      }
    ))


  df %>% filter(group == grouping) %>%
  ggplot(aes(x = date, y = count, group = group))+
    annotation_layers$layer +
    geom_path()+
    scale_y_sqrt(limits = c(0,max)) +
    theme_bw() +
    ggtitle(stringr::str_to_title(grouping)) +
    labs(x="", y="Count")

}

# annotate("rect", ymax=1000,ymin=500,
#          xmin=as.POSIXct(-Inf), xmax=as.POSIXct(Inf),
#          fill = "red", alpha = 0.2)

plt <- ggpollen("tree")

df2 <- tibble(group = c("tree", "grass", "weed", "mold")) %>%
  mutate(plot = purrr::map(group, function(g) ggpollen(g)))

# df2$plot[1]

cowplot::plot_grid(plotlist = df2$plot)

