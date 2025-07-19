library(shiny)
library(tidyverse)


restrict_data <- function(rating_data, juniors, inactives, floor,
                          birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data %>%
    filter(if (inactives) TRUE else active) %>%
    filter(if (birth_uncertain) TRUE else !is.na(born)) %>%
    filter(born <= max_byear | is.na(born), rating >= floor)
}


participation_gap <- function(rating_data) {
  rating_data %>%
    count(fed, sex, name = "no_of_players") %>%
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) %>%
    mutate(frac_women = `F` / (`F` + `M`))
}


stats_participation <- function(juniors, inactives, floor, metric, rating_data) {
  rating_data %>%
    restrict_data(juniors == {{juniors}}, inactives == {{inactives}},
                  floor == {{floor}}) %>%
    participation_gap()
}


p_anal <- function(pvalues, signif = 0.05, method = "fdr") {
  p_female <- p.adjust(1 - pvalues, method = method)
  p_male <- p.adjust(pvalues, method = method)
  # The factor of 2 simply introduces a symbol to distinguish women (2) from men (1):
  signif_female <- 2L * (p_female < signif / 2)
  signif_male <- 1L * (p_male < signif / 2)
  # The nonzero entries of signif_female and signif_male are completely nonoverlapping:
  s <- signif_female + signif_male
  # Translate the arbitrary symbols 2 and 1 into test describing significance:
  case_when(
    s %in% 1:2 ~ "Significant under the participation rate hypothesis",
    s == 0 ~ "Not significant under the participation rate hypothesis",
    .default = "ERROR - BOTH SEXES ARE SIGNIFICANT"
  )
}


# Table of (corrected) significances for each federation:
p_values <- function(null_stats, signif = 0.05, method = "fdr") {
  null_stats %>%
    filter(stat == "ptpval") %>%
    mutate(signif = p_anal(value, signif, method),
           .by = c(juniors, inactives, floor, metric)) %>%
    select(!stat & !value)
}


gap_plot <- function(juniors, inactives, floor, metric, qty, main_table, rating_data) {
  main_table %>%
    filter(juniors == {{juniors}}, inactives == {{inactives}}, floor == {{floor}},
           metric == {{metric}}, qty == {{qty}}) %>%
    left_join(
      stats_participation(juniors = juniors, inactives = inactives, floor = floor,
                          metric = metric, rating_data),
      by = join_by(fed)
    ) %>%
    ggplot(aes(x = frac_women, y = gap, color = signif, label = fed)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4, linetype = "dashed") +
    geom_text(fontface = "bold") +
    scale_color_manual(name = NULL, values = c("firebrick", "gray50"), drop = FALSE) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
    labs(
      x = "Percentage of players who are women",
      y = expression(paste("Rating gap ", (M - W)))
    ) +
    guides(color = guide_legend(ncol = 1)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
}



rating_data <- read_csv("data/rating-data.csv", col_types = "ccciiil") %>%
  restrict_data(juniors = TRUE, inactives = TRUE, floor = 1000)

null_stats <- read_csv("data/null-stats.csv", col_types = "llicccd") %>%
  filter(fed != "ALL")

main_table <- null_stats %>%
  filter(stat == "obs") %>%
  rename(y = value) %>%
  left_join(read_csv("data/age-experience-tab.csv", col_types = "cllicddddd"),
            by = join_by(metric, juniors, inactives, floor, fed)) %>%
  select(!c(stat, E, A, weight)) %>%
  full_join(p_values(null_stats, signif = 0.05, method = "fdr"),
            by = join_by(juniors, inactives, floor, metric, fed)) %>%
  mutate(
    signif = fct_relevel(signif, "Significant under the participation rate hypothesis")
  ) %>%
  pivot_longer(cols = starts_with("y"), names_to = "qty", values_to = "gap")



ui <- fluidPage(
  title = "Chess data explorer",
  titlePanel("Chess data explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "inactives",
        label = "Include inactive players?",
        choiceNames = c("Yes", "No"),
        choiceValues = c(TRUE, FALSE),
        selected = FALSE,
        inline = TRUE
      ),
      radioButtons(
        inputId = "juniors",
        label = "Include junior players?",
        choiceNames = c("Yes", "No"),
        choiceValues = c(TRUE, FALSE),
        selected = TRUE,
        inline = TRUE
      ),
      radioButtons(
        inputId = "floor",
        label = "Rating floor:",
        choiceNames = c("1000", "1400", "1600"),
        choiceValues = c(1000, 1400, 1600),
        selected = 1000,
        inline = TRUE
      ),
      radioButtons(
        inputId = "metric",
        label = "Metric:",
        choiceNames = c("Overall mean gap", "Overall median gap", "Top 10 gap",
                        "Top 1 gap", "Standard deviation"),
        choiceValues = c("mean", "median", "top10", "top1", "sd"),
        selected = "mean",
        inline = FALSE
      ),
      radioButtons(
        inputId = "qty",
        label = "Correction to ratings:",
        choiceNames = c("None (raw ratings)", "Participation correction",
                        "Participation, age & experience correction"),
        choiceValues = c("y", "yP", "yPEA"),
        selected = "y",
        inline = FALSE
      )
    ),
    mainPanel(plotOutput("plot"))
  )
)


server <- function(input, output) {
  output$plot <- renderPlot({
    gap_plot(juniors = input$juniors, inactives = input$inactives, floor = input$floor,
             metric = input$metric, qty = input$qty, main_table, rating_data)
  }, width = 500, height = 500)
}


shinyApp(ui = ui, server = server)

