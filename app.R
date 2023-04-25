library(shiny)
library(shinythemes)
library(here)
source("pollen_functions.R")

df <- tibble(file = list.files(here("data/pollen_data/"))) %>%
    mutate(path = here(sprintf("data/pollen_data/%s", file))) %>%
    mutate(data = purrr::map(path, ~suppressMessages(parse_pollen_xlsx(.)))) %>%
    tidyr::unnest(data) %>%
    distinct() %>%
    group_by(group, date, file) %>%
    summarise(count = sum(count, na.rm = T))

df <- tibble(group = c("tree", "grass", "weed", "mold")) %>%
    mutate(plot = purrr::map(group, function(g) ggpollen(df, g)))

header_tag <- tags$head(
    # includeHTML("google-analytics.js"),
    tags$style(HTML("
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "))
)


ui <- navbarPage("Pollen", theme = shinytheme("flatly"), header = header_tag,
    tabPanel("Pollen", id = "pollen",
        fluidRow(
            plotOutput("pollen"))
        ))

server <- function(input, output) {
    output$pollen <- renderPlot({
        cowplot::plot_grid(plotlist = df$plot)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
