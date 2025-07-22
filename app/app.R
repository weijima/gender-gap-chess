library(shiny)
library(tidyverse)



# Utility functions ---------------------------------------------------------------------

restrict_data <- function(rating_data, juniors, inactives, floor,
                          birth_uncertain = FALSE) {
  if (juniors) max_byear <- 2019 else max_byear <- 1999
  rating_data |>
    filter(if (inactives) TRUE else active) |>
    filter(if (birth_uncertain) TRUE else !is.na(born)) |>
    filter(born <= max_byear | is.na(born), rating >= floor)
}


participation_gap <- function(rating_data) {
  rating_data |>
    count(fed, sex, name = "no_of_players") |>
    pivot_wider(names_from = "sex", values_from = "no_of_players", values_fill = 0) |>
    mutate(frac_women = `F` / (`F` + `M`))
}


stats_participation <- function(juniors, inactives, floor, metric, rating_data) {
  rating_data |>
    restrict_data(juniors == {{juniors}}, inactives == {{inactives}},
                  floor == {{floor}}) |>
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
    s %in% 1:2 ~ "Significant",
    s == 0 ~ "Not significant",
    .default = "ERROR - BOTH SEXES ARE SIGNIFICANT"
  )
}


simple_stats <- function(tab) {
  mean_gap <- round(mean(tab$gap), 1)
  stat_tab <- tab |>
    count(pos = gap > 0, sig = signif == "Significant") |>
    full_join(crossing(pos = c(FALSE, TRUE), sig = c(FALSE, TRUE)),
              by = join_by(pos, sig)) |>
    mutate(n = ifelse(is.na(n), 0, n))
  n_pos <- filter(stat_tab, pos) |> pull(n) |> sum()
  n_neg <- filter(stat_tab, !pos) |> pull(n) |> sum()
  pos_sig <- filter(stat_tab, pos & sig) |> pull(n)
  neg_sig <- filter(stat_tab, !pos & sig) |> pull(n)
  pos_sig_str <- if (tab$signif[1] != "") str_c("(", pos_sig, " significant)") else ""
  neg_sig_str <- if (tab$signif[1] != "") str_c("(", neg_sig, " significant)") else ""
  HTML(str_c(
    "• Positive gap: ", n_pos, " federations ", pos_sig_str, "<br>",
    "• Negative gap: ", n_neg, " federations ", neg_sig_str, "<br>",
    "• Average gap across all federations: ", sprintf("%.1f", mean_gap)
  ))
}


color_scaling <- function(show_col_legend) {
  if (show_col_legend) {
    scale_color_manual(name = NULL, values = c("firebrick", "gray50"), drop = FALSE)
  } else {
    scale_color_manual(name = NULL, values = c("steelblue"))
  }
}


color_guide <- function(show_col_legend) {
  if (show_col_legend) {
    guides(color = guide_legend(nrow = 1), label = "none")
  } else {
    guides(color = guide_legend(override.aes = list(color = NA, shape = utf8ToInt("N"))))
  }
}


create_ylab <- function(metric, qty) {
  ylab_qty <- case_when(
    qty == "y"    ~ "Uncorrected",
    qty == "yP"   ~ "P-corrected",
    qty == "yPEA" ~ "PEA-corrected"
  )
  ylab_metric <- case_when(
    metric == "mean"  ~ paste(ylab_qty, "overall mean gap"),
    metric == "top10" ~ paste(ylab_qty, "top 10 gap"),
    metric == "top1"  ~ paste(ylab_qty, "top 1 gap"),
    metric == "sd"    ~ "Gap in standard deviations"
  )
  bquote(.(ylab_metric) ~ (M - W))
}


gap_plot <- function(main_tab, metric, qty, rating_data) {
  show_col_legend <- qty == "yP" || metric == "sd"
  main_tab |>
    add_case(signif = "Significant", .before = 1) |>
    mutate(signif = as_factor(ifelse(signif == "Significant",
                                     "Significant                         ",
                                     signif))) |>
    slice(-1) |>
    ggplot(aes(x = frac_women, y = gap, color = signif, label = fed)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.4, linetype = "dashed") +
    geom_text(fontface = "bold") +
    color_scaling(show_col_legend) +
    color_guide(show_col_legend) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
    labs(x = "Percentage of female players", y = create_ylab(metric, qty)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
}



# Load and organize data ----------------------------------------------------------------

rating_data <- read_rds("rating-data.rds") |>
  restrict_data(juniors = TRUE, inactives = TRUE, floor = 1000)

null_stats <- read_rds("null-stats.rds") |>
  filter(fed != "ALL")

age_experience <- read_rds("age-experience-tab.rds")

main_table <- null_stats |>
  filter(stat %in% c("obs", "ptpval")) |>
  pivot_wider(names_from = stat, values_from = value) |>
  rename(y = obs, pval = ptpval) |>
  relocate(y, .after = pval) |>
  left_join(age_experience, by = join_by(metric, juniors, inactives, floor, fed)) |>
  select(!E & !A & !weight)



# Shiny app -----------------------------------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),
  title = "Chess data explorer",
  titlePanel("Deconstructing the gender gap in chess ratings: data explorer"),
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
        choiceNames = c("Overall mean gap", "Top 10 gap",
                        "Top 1 gap", "Standard deviation"),
        choiceValues = c("mean", "top10", "top1", "sd"),
        selected = "mean",
        inline = FALSE
      ),
      radioButtons(
        inputId = "qty",
        label = "Correction to ratings:",
        choiceNames = c("None (raw ratings)", "Participation correction",
                        "Participation, age & experience correction"),
        choiceValues = c("y", "yP", "yPEA"),
        selected = "yP",
        inline = FALSE
      ),
      radioButtons(
        inputId = "method",
        label = "Correction to multiple testing:",
        choiceNames = c("None", "False discovery rate", "Bonferroni"),
        choiceValues = c("none", "fdr", "bonferroni"),
        selected = "fdr",
        inline = FALSE
      ),
      sliderInput(
        inputId = "signif",
        label = "Significance threshold:",
        min = 0.001,
        max = 0.1,
        value = 0.05,
        step = 0.001
      )
    ),
    mainPanel(
      plotOutput("plot"),
      br(), br(), hr(),
      htmlOutput("stats"),
      hr(),
      DT::DTOutput("table")
    )
  )
)


server <- function(input, output) {

  dat <- reactive(
    main_table |>
      mutate(signif = p_anal(pval, signif = input$signif, method = input$method),
             .after = pval,
             .by = c(juniors, inactives, floor, metric)) |>
      pivot_longer(cols = starts_with("y"), names_to = "qty", values_to = "gap") |>
      filter(juniors == input$juniors, inactives == input$inactives,
             floor == input$floor, metric == input$metric, qty == input_qty()) |>
      left_join(
        stats_participation(juniors = input$juniors, inactives = input$inactives,
                            floor = input$floor, metric = input$metric, rating_data),
        by = join_by(fed)
      ) |>
      (\(.) if (input_qty() == "yP" || input$metric == "sd") . else
        mutate(., signif = ""))()
  )

  input_qty <- reactiveVal(value = "y")

  observe({
    # Disable the "Correction to ratings" radio buttons if the desired metric is "sd":
    if (input$metric != "sd") {
      shinyjs::enable("qty")
      input_qty(input$qty)
    } else {
      shinyjs::disable("qty")
      input_qty("y")
    }
    # Only enable "Correction to multiple testing" if (i) looking at P-corrected data,
    # or (ii) looking at the "sd" metric:
    if (input$qty == "yP" || input$metric == "sd") {
      shinyjs::enable("method")
    } else {
      shinyjs::disable("method")
    }
  })

  output$plot <- renderPlot({
    gap_plot(dat(), metric = input$metric, qty = input_qty(), rating_data)
  }, width = 500, height = 450)

  output$stats <- renderText({
    simple_stats(dat())
  })

  output$table <- DT::renderDT({
    with_pval <- dat()$signif[1] != ""
    dat() |>
      select(fed, `F`, `M`, gap, pval) |>
      mutate(pval = round(pval, 4)) |>
      mutate(pval = if (with_pval) pval else NA_real_) |>
      mutate(gap = round(gap, 2)) |>
      rename(federation = fed, women = `F`, men = `M`,
             `rating gap` = gap, `p-value` = pval)
  })
}


shinyApp(ui = ui, server = server)
