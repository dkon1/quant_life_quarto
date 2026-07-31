# =============================================================================
# Regression to the mean, illustrated with generated data
#
# Heart rate 1 (HR1) ~ Normal(mean = 70, sd = 10), n = 1000.
# Heart rate 2 (HR2) = r * HR1 + c + noise,  noise ~ Normal(0, s),
#   where the constant c = 70 * (1 - r) is added so that HR2 has the same mean
#   as HR1 (equivalently, HR2 = 70 + r * (HR1 - 70) + noise).
#
# Sliders set r and s. The app shows a scatterplot with the least-squares line
# (slope and intercept displayed) and prints the mean of HR1 and HR2 for the
# people whose HR1 is below the overall HR1 mean, and again for those above it.
#
# Regression to the mean: a subgroup selected as extreme on HR1 is, on average,
# less extreme on HR2 (relative to each variable's own mean and spread) whenever
# the two are less than perfectly correlated.
#
# To run:
#   install.packages(c("shiny", "ggplot2"))
#   shiny::runApp("regression_to_mean_app.R")
# =============================================================================

library(shiny)
library(ggplot2)

# Fixed HR1 and unit noise so moving the sliders rescales the SAME data smoothly
gen_data <- function(r, s, seed, n = 1000) {
  set.seed(seed)
  HR1 <- rnorm(n, mean = 70, sd = 10)
  z   <- rnorm(n, mean = 0, sd = 1)
  HR2_raw <- r * HR1 + s * z
  # Constant (y-intercept) that gives HR2 the same mean as HR1.
  # Its expected value is 70 * (1 - r); computed from the sample so the
  # realized means are exactly equal.
  const <- mean(HR1) - mean(HR2_raw)
  df <- data.frame(HR1 = HR1, HR2 = HR2_raw + const)
  attr(df, "const") <- const
  df
}

plot_rtm <- function(df) {
  m1 <- mean(df$HR1); m2 <- mean(df$HR2)
  fit <- lm(HR2 ~ HR1, data = df)
  b0 <- coef(fit)[1]; b1 <- coef(fit)[2]
  rho <- cor(df$HR1, df$HR2)

  ggplot(df, aes(HR1, HR2)) +
    geom_point(alpha = 0.22, colour = "#3a76c2", size = 1.3) +
    geom_vline(xintercept = m1, linetype = "dashed", colour = "grey55") +
    geom_hline(yintercept = m2, linetype = "dashed", colour = "grey55") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = "#c0392b", linewidth = 1.1) +
    annotate("label", x = min(df$HR1), y = max(df$HR2), hjust = 0, vjust = 1,
             size = 4.2, label.size = 0, fill = "#ffffff", alpha = 0.7,
             label = sprintf("HR2 = %.2f + %.3f x HR1\ncorrelation = %.3f",
                             b0, b1, rho)) +
    labs(x = "Heart rate 1 (bpm)", y = "Heart rate 2 (bpm)",
         subtitle = "Dashed lines mark the mean of each variable") +
    theme_bw(base_size = 13) +
    theme(panel.grid.minor = element_blank())
}

# ---- UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
    pre { font-size: 14px; background:#fafafa; border:1px solid #e3e3e3;
      border-radius:8px; padding:12px 14px; }
    .note { background:#eef4fb; border-left:4px solid #3a76c2;
      padding:10px 14px; border-radius:6px; font-size:14px; }
  "))),

  titlePanel("Regression to the mean"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("r", "r  (HR1 multiplier)",
                  min = 0, max = 1.5, value = 0.8, step = 0.05),
      sliderInput("s", "s  (noise standard deviation, bpm)",
                  min = 0, max = 30, value = 8, step = 0.5),
      actionButton("regen", "Regenerate data", class = "btn-sm"),
      helpText("HR1 ~ Normal(70, 10), n = 1000. ",
               "HR2 = r x HR1 + c + Normal(0, s), where c = 70 x (1 - r) is ",
               "added so HR1 and HR2 share the same mean. The regression slope ",
               "is close to r; with any noise the correlation is below 1, which ",
               "is what produces regression to the mean.")
    ),

    mainPanel(
      width = 9,
      plotOutput("scatter", height = "420px"),
      tags$br(),
      h4("Group means (split at the overall HR1 mean)"),
      verbatimTextOutput("means"),
      uiOutput("interp")
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  seed <- reactiveVal(2024)
  observeEvent(input$regen, seed(seed() + 1))

  df <- reactive(gen_data(input$r, input$s, seed()))

  stats <- reactive({
    d <- df()
    m1 <- mean(d$HR1); m2 <- mean(d$HR2)
    sd1 <- sd(d$HR1);  sd2 <- sd(d$HR2)
    below <- d$HR1 < m1
    list(
      n = nrow(d), m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2,
      const = attr(d, "const"),
      n_lo = sum(below), n_hi = sum(!below),
      lo1 = mean(d$HR1[below]),  lo2 = mean(d$HR2[below]),
      hi1 = mean(d$HR1[!below]), hi2 = mean(d$HR2[!below])
    )
  })

  output$scatter <- renderPlot(plot_rtm(df()))

  output$means <- renderPrint({
    s <- stats()
    cat(sprintf("Constant c added to HR2 (y-intercept) = %5.2f   [ = 70 x (1 - r) ]\n\n",
                s$const))
    cat(sprintf("Overall (n = %d):        mean HR1 = %5.2f    mean HR2 = %5.2f\n",
                s$n, s$m1, s$m2))
    cat(strrep("-", 62), "\n")
    cat(sprintf("HR1 below mean (n = %3d): mean HR1 = %5.2f    mean HR2 = %5.2f\n",
                s$n_lo, s$lo1, s$lo2))
    cat(sprintf("HR1 above mean (n = %3d): mean HR1 = %5.2f    mean HR2 = %5.2f\n",
                s$n_hi, s$hi1, s$hi2))
  })

  output$interp <- renderUI({
    s <- stats()
    lo1z <- (s$lo1 - s$m1) / s$sd1; lo2z <- (s$lo2 - s$m2) / s$sd2
    hi1z <- (s$hi1 - s$m1) / s$sd1; hi2z <- (s$hi2 - s$m2) / s$sd2
    rtm  <- abs(lo2z) < abs(lo1z) && abs(hi2z) < abs(hi1z)

    verdict <- if (rtm)
      "Each extreme group is nearer the mean on HR2 than on HR1 &mdash; regression to the mean."
    else
      "HR2 is not less extreme than HR1 here; try adding noise or setting r &lt; 1."

    div(class = "note",
        HTML(sprintf(
          "HR1 and HR2 now share a mean of about %.1f bpm, so the group means ",
          s$m1),
          sprintf("compare directly. The below-average group averages %.1f on HR1 ",
                  s$lo1),
          sprintf("but %.1f on HR2; the above-average group averages %.1f on HR1 ",
                  s$lo2, s$hi1),
          sprintf("but %.1f on HR2. %s", s$hi2, verdict)))
  })
}

shinyApp(ui, server)
