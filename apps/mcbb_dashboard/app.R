## ============================================================
## konenMCBB — MCBB Advanced Stats Dashboard
## ============================================================
## Run from the project root:
##   shiny::runApp("apps/mcbb_dashboard")
##
## Required packages (beyond konenMCBB):
##   shiny, bslib, DT, ggplot2, dplyr, tidyr, scales
## ============================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Load the package (works installed or from source)
if (!requireNamespace("konenMCBB", quietly = TRUE)) {
  # Dev mode: load from source two directories up
  devtools::load_all(normalizePath(file.path("..", ".."), mustWork = TRUE))
} else {
  library(konenMCBB)
}

# Optional dependency — only used for scatter labels
have_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

# -- Icon helper (must be defined before UI) -----------------------------------
icon_text <- function(icon_name, text) {
  tagList(tags$i(class = paste0("fas fa-", icon_name)), " ", text)
}

# ── Theme ─────────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "cosmo",
  primary    = "#1a3a5c",
  secondary  = "#e07b24",
  base_font  = font_google("Inter"),
  code_font  = font_google("Fira Code")
)

# ── Helpers ───────────────────────────────────────────────────────────────────
pct_fmt  <- function(x) sprintf("%.1f%%", x * 100)
num_fmt  <- function(x) sprintf("%.1f", x)

conf_choices <- c(
  "All Conferences" = "All",
  "ACC", "B10", "B12", "BE", "P12", "SEC",
  "Amer", "A10", "MWC", "WCC", "CUSA", "MAC",
  "MVC", "SBC", "CAA", "Horz", "OVC", "BSky",
  "SC", "ASun", "Sum", "NEC", "Pat", "MEAC", "SWAC", "Ind"
)

# Torvik uses season-end year convention (year=2026 = 2025-26 season).
# Nov-Dec: season just started, so current_year + 1 is the active season.
# Jan-Apr: active season, current_year.
# May-Oct: off-season — most recent completed season was current_year.
.month <- as.integer(format(Sys.Date(), "%m"))
current_year <- if (.month >= 11L) {
  as.integer(format(Sys.Date(), "%Y")) + 1L
} else {
  as.integer(format(Sys.Date(), "%Y"))
}
# In-season months: Nov–Apr. If we're in the off-season (May–Oct), display a banner.
is_offseason <- .month >= 5L && .month <= 10L

year_choices <- setNames(
  seq(current_year, 2008, by = -1),
  paste0(seq(current_year, 2008, by = -1) - 1, "-",
         substr(as.character(seq(current_year, 2008, by = -1)), 3, 4))
)

# Default season selection: use previous year during off-season (May–Oct) so
# the first Load doesn't immediately 404 on endpoints that don't exist yet.
default_year <- if (is_offseason) current_year - 1L else current_year

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    img(src = "https://upload.wikimedia.org/wikipedia/commons/2/28/March_Madness_logo.svg",
        height = "30px", style = "margin-right:8px;"),
    "MCBB Advanced Stats"
  ),
  theme  = app_theme,
  id     = "main_nav",
  header = tagList(
    tags$head(tags$style(HTML("
      .metric-card { border-radius: 8px; padding: 16px; background: #f8f9fa; margin-bottom: 12px; }
      .metric-val  { font-size: 2rem; font-weight: 700; color: #1a3a5c; }
      .metric-lbl  { font-size: 0.85rem; color: #6c757d; }
      .nav-item .active { font-weight: 600; }
      .dataTables_wrapper .dataTables_filter input { border-radius: 6px; }
    "))),
    # Off-season notice (May–October) — some Torvik endpoints 404 during the off-season.
    # Historical data (select a year <= current_year - 1) always works.
    if (is_offseason) {
      div(
        class = "alert alert-warning fade show mb-0",
        role  = "alert",
        style = paste0("border-radius: 0; margin: 0; padding: 8px 16px; ",
                       "font-size: 0.9rem; text-align: center;"),
        tags$strong("⚠ Off-season (May–Oct):"),
        " Live slate & current-season files may not be available on barttorvik.com. ",
        "Historical data is fully accessible — select any season year from the dropdown. ",
        "Season returns in November."
      )
    }
  ),

  # ── Tab 1: T-Rank Ratings ──────────────────────────────────────────────────
  nav_panel(
    title = icon_text("trophy", "T-Rank Ratings"),
    value = "rankings",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        selectInput("rank_year", "Season", choices = year_choices, selected = default_year),
        selectInput("rank_conf", "Conference", choices = conf_choices),
        sliderInput("rank_n", "Show top N teams", min = 10, max = 363, value = 25, step = 5),
        hr(),
        actionButton("load_rankings", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      # Key summary cards
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("rank_card_1"),
        uiOutput("rank_card_2"),
        uiOutput("rank_card_3"),
        uiOutput("rank_card_4")
      ),
      br(),
      DTOutput("rankings_table"),
      br(),
      plotOutput("barthag_plot", height = "400px")
    )
  ),

  # ── Tab 2: Conference Overview ─────────────────────────────────────────────
  nav_panel(
    title = icon_text("chart-bar", "Conference"),
    value = "conference",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        selectInput("conf_year", "Season", choices = year_choices, selected = default_year),
        numericInput("conf_min_teams", "Min teams", value = 8, min = 4, max = 20),
        hr(),
        actionButton("load_conf", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      plotOutput("conf_barthag_plot", height = "450px"),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("conf_oe_de_plot", height = "380px"),
        plotOutput("conf_tempo_plot", height = "380px")
      ),
      br(),
      DTOutput("conf_table")
    )
  ),

  # ── Tab 3: Four Factors Explorer ──────────────────────────────────────────
  nav_panel(
    title = icon_text("sliders", "Four Factors"),
    value = "fourfactors",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("ff_year", "Season", choices = year_choices, selected = default_year),
        selectInput("ff_conf", "Conference", choices = conf_choices),
        hr(),
        selectInput("ff_x", "X axis",
          choices = list(
            "Offensive" = c("eFG% Off" = "o_efg", "TO% Off" = "o_to_pct",
                            "OReb%" = "o_reb_pct", "FT Rate Off" = "o_ftr"),
            "Defensive" = c("eFG% Def" = "d_efg", "TO% Def" = "d_to_pct",
                            "DReb%" = "d_reb_pct", "FT Rate Def" = "d_ftr")
          ),
          selected = "o_efg"
        ),
        selectInput("ff_y", "Y axis",
          choices = list(
            "Offensive" = c("eFG% Off" = "o_efg", "TO% Off" = "o_to_pct",
                            "OReb%" = "o_reb_pct", "FT Rate Off" = "o_ftr"),
            "Defensive" = c("eFG% Def" = "d_efg", "TO% Def" = "d_to_pct",
                            "DReb%" = "d_reb_pct", "FT Rate Def" = "d_ftr")
          ),
          selected = "d_efg"
        ),
        checkboxInput("ff_label", "Label points", value = FALSE),
        hr(),
        actionButton("load_ff", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      plotOutput("ff_scatter", height = "550px"),
      br(),
      DTOutput("ff_table")
    )
  ),

  # ── Tab 4: Shooting Splits ─────────────────────────────────────────────────
  nav_panel(
    title = icon_text("basketball-ball", "Shooting"),
    value = "shooting",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("sh_year", "Season", choices = year_choices, selected = default_year),
        selectInput("sh_conf", "Conference", choices = conf_choices),
        selectInput("sh_side", "Side",
                    choices = c("Offense" = "o", "Defense" = "d"),
                    selected = "o"),
        hr(),
        actionButton("load_sh", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("sh_rim_plot",   height = "380px"),
        plotOutput("sh_three_plot", height = "380px")
      ),
      br(),
      DTOutput("sh_table")
    )
  ),

  # ── Tab 5: Today's Games ──────────────────────────────────────────────────
  nav_panel(
    title = icon_text("calendar-day", "Today's Slate"),
    value = "today",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        p(strong("Date:"), format(Sys.Date(), "%B %d, %Y")),
        selectInput("today_year", "Season", choices = year_choices, selected = default_year),
        hr(),
        actionButton("load_today", "Refresh Slate", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      h4("Games on today's board:"),
      DTOutput("today_table"),
      br(),
      plotOutput("today_plot", height = "350px")
    )
  ),

  # ── Tab 6: Player Stats ────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("person", "Players"),
    value = "players",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("pl_year", "Season", choices = year_choices, selected = default_year),
        sliderInput("pl_gp",  "Min games played", min = 1, max = 35, value = 10),
        sliderInput("pl_usg", "Min usage %",      min = 0, max = 35, value = 15),
        hr(),
        actionButton("load_pl", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("pl_bpm_plot",  height = "380px"),
        plotOutput("pl_ortg_plot", height = "380px")
      ),
      br(),
      DTOutput("pl_table")
    )
  ),

  # ── Tab 7: Team Resume ─────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("clipboard-list", "Team Resume"),
    value = "resume",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("res_year", "Season", choices = year_choices, selected = default_year),
        uiOutput("res_team_ui"),
        selectInput("res_quad", "Quadrant filter",
                    choices = c("All" = "All", "Quad 1" = "1", "Quad 2" = "2",
                                "Quad 3" = "3", "Quad 4" = "4")),
        hr(),
        actionButton("load_res_teams", "Load Teams", class = "btn-secondary w-100",
                     icon = icon("download")),
        br(), br(),
        actionButton("load_res", "Load Games", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("res_card_w"),
        uiOutput("res_card_l"),
        uiOutput("res_card_adjoe"),
        uiOutput("res_card_adjde")
      ),
      br(),
      plotOutput("res_result_plot", height = "300px"),
      br(),
      DTOutput("res_table")
    )
  ),

  # ── Tab 8: Super Schedule ─────────────────────────────────────────────────
  nav_panel(
    title = icon_text("calendar-week", "Super Schedule"),
    value = "supersked",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("ss_year", "Season", choices = year_choices, selected = default_year),
        hr(),
        p("Shows all scheduled games with Torvik's predictions, win probabilities, and game quality scores.",
          style = "font-size:0.85em; color:#6c757d;"),
        actionButton("load_ss", "Load / Refresh", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("ss_ttq_plot",  height = "380px"),
        plotOutput("ss_line_plot", height = "380px")
      ),
      br(),
      DTOutput("ss_table")
    )
  ),

  # ── Tab 9: Time Machine ───────────────────────────────────────────────────
  nav_panel(
    title = icon_text("clock-rotate-left", "Time Machine"),
    value = "timemachine",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        dateInput("tm_date", "Snapshot date",
                  value = Sys.Date() - 1,
                  min   = as.Date("2014-11-13"),
                  max   = Sys.Date() - 1),
        selectInput("tm_conf", "Conference", choices = conf_choices),
        sliderInput("tm_n", "Show top N teams", min = 10, max = 363, value = 25, step = 5),
        hr(),
        p("Loads Torvik's ratings as they stood on the selected date. Useful for historical analysis.",
          style = "font-size:0.85em; color:#6c757d;"),
        actionButton("load_tm", "Load Snapshot", class = "btn-primary w-100",
                     icon = icon("rotate"))
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("tm_card_1"),
        uiOutput("tm_card_2"),
        uiOutput("tm_card_3"),
        uiOutput("tm_card_4")
      ),
      br(),
      plotOutput("tm_barthag_plot", height = "420px"),
      br(),
      DTOutput("tm_table")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # -- Data stores (reactive, loaded on button press) -------------------------
  rv <- reactiveValues(
    rankings   = NULL,
    conf       = NULL,
    ff         = NULL,
    shooting   = NULL,
    today      = NULL,
    players    = NULL,
    res_teams  = NULL,
    resume     = NULL,
    supersked  = NULL,
    timemachine = NULL
  )

  # ── Rankings ──────────────────────────────────────────────────────────────
  observeEvent(input$load_rankings, {
    withProgress(message = "Fetching T-Rank ratings...", {
      rv$rankings <- tryCatch(
        torvik_team_ratings(
          year = as.integer(input$rank_year),
          conf = if (input$rank_conf == "All") "All" else input$rank_conf
        ),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  rank_data <- reactive({
    req(rv$rankings)
    rv$rankings |>
      head(as.integer(input$rank_n)) |>
      select(rank, team, conf, record, adj_oe, adj_de, barthag, adj_tempo, wab)
  })

  output$rank_card_1 <- renderUI({
    d <- rv$rankings; req(d)
    div(class = "metric-card",
        div(class = "metric-val", d$team[1]),
        div(class = "metric-lbl", "#1 ranked team"))
  })
  output$rank_card_2 <- renderUI({
    d <- rv$rankings; req(d)
    div(class = "metric-card",
        div(class = "metric-val", sprintf("%.3f", max(d$barthag, na.rm = TRUE))),
        div(class = "metric-lbl", "Highest Barthag"))
  })
  output$rank_card_3 <- renderUI({
    d <- rv$rankings; req(d)
    div(class = "metric-card",
        div(class = "metric-val", sprintf("%.1f", max(d$adj_oe, na.rm = TRUE))),
        div(class = "metric-lbl", "Best Adj. OE"))
  })
  output$rank_card_4 <- renderUI({
    d <- rv$rankings; req(d)
    div(class = "metric-card",
        div(class = "metric-val", sprintf("%.1f", min(d$adj_de, na.rm = TRUE))),
        div(class = "metric-lbl", "Best Adj. DE"))
  })

  output$rankings_table <- renderDT({
    d <- rank_data()
    datatable(
      d,
      colnames = c("Rank", "Team", "Conf", "Record",
                   "Adj OE", "Adj DE", "Barthag", "Tempo", "WAB"),
      rownames = FALSE,
      options  = list(pageLength = 25, dom = "frtip",
                      order = list(list(0, "asc"))),
      class = "stripe hover compact"
    ) |>
      formatRound(c("adj_oe", "adj_de", "adj_tempo", "wab"), 1) |>
      formatRound("barthag", 3) |>
      formatStyle("barthag",
        background = styleColorBar(range(d$barthag, na.rm = TRUE), "#cce5ff"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  output$barthag_plot <- renderPlot({
    d <- rank_data()
    ggplot(d, aes(x = reorder(team, barthag), y = barthag, fill = barthag)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#cce5ff", high = "#1a3a5c") +
      coord_flip() +
      labs(title = "Barthag Power Rating (top teams)",
           x = NULL, y = "Barthag") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 8))
  })

  # ── Conference ────────────────────────────────────────────────────────────
  observeEvent(input$load_conf, {
    withProgress(message = "Fetching conference data...", {
      rv$conf <- tryCatch(
        torvik_conf_stats(year = as.integer(input$conf_year),
                          min_teams = as.integer(input$conf_min_teams)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$conf_barthag_plot <- renderPlot({
    d <- rv$conf; req(d)
    ggplot(d, aes(x = reorder(conf, mean_barthag), y = mean_barthag, fill = mean_barthag)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = top_team), hjust = -0.1, size = 3, color = "#333") +
      scale_fill_gradient(low = "#d4e6f1", high = "#1a3a5c") +
      coord_flip(clip = "off") +
      labs(title = "Conference Strength by Avg. Barthag",
           subtitle = "Label = best team in conference",
           x = NULL, y = "Avg. Barthag") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", plot.margin = margin(r = 80))
  })

  output$conf_oe_de_plot <- renderPlot({
    d <- rv$conf; req(d)
    ggplot(d, aes(x = mean_adj_oe, y = mean_adj_de, label = conf, color = mean_barthag)) +
      geom_point(size = 3.5) +
      geom_text(nudge_y = 0.08, size = 3) +
      scale_color_gradient(low = "#aed6f1", high = "#1a3a5c") +
      scale_y_reverse() +
      labs(title = "Adj. OE vs. Adj. DE by Conference",
           subtitle = "Right = better offense · Up = better defense",
           x = "Avg. Adj. Offensive Efficiency",
           y = "Avg. Adj. Defensive Efficiency (lower = better)",
           color = "Barthag") +
      theme_minimal(base_size = 11)
  })

  output$conf_tempo_plot <- renderPlot({
    d <- rv$conf; req(d)
    ggplot(d, aes(x = reorder(conf, mean_adj_tempo), y = mean_adj_tempo)) +
      geom_col(fill = "#e07b24", width = 0.65) +
      coord_flip() +
      labs(title = "Conference Pace (Avg. Adj. Tempo)",
           x = NULL, y = "Possessions / 40 min") +
      theme_minimal(base_size = 11)
  })

  output$conf_table <- renderDT({
    d <- rv$conf; req(d)
    cols_show <- c("conf", "n_teams", "mean_barthag", "mean_adj_oe",
                   "mean_adj_de", "mean_adj_tempo", "mean_wab", "total_wab",
                   "top_team", "top_barthag")
    datatable(
      d[, intersect(cols_show, names(d))],
      colnames = c("Conf", "Teams", "Avg Barthag", "Avg Adj OE",
                   "Avg Adj DE", "Avg Tempo", "Avg WAB", "Total WAB",
                   "Top Team", "Top Barthag")[seq_len(length(intersect(cols_show, names(d))))],
      rownames = FALSE,
      options  = list(pageLength = 20, dom = "frtip"),
      class    = "stripe hover compact"
    ) |>
      formatRound(c("mean_barthag", "top_barthag"), 3) |>
      formatRound(c("mean_adj_oe", "mean_adj_de", "mean_adj_tempo",
                    "mean_wab", "total_wab"), 1)
  })

  # ── Four Factors ──────────────────────────────────────────────────────────
  observeEvent(input$load_ff, {
    withProgress(message = "Fetching four factors...", {
      rv$ff <- tryCatch(
        torvik_four_factors(
          year = as.integer(input$ff_year),
          conf = if (input$ff_conf == "All") "All" else input$ff_conf
        ),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$ff_scatter <- renderPlot({
    d  <- rv$ff; req(d)
    xv <- input$ff_x; yv <- input$ff_y
    xlab <- names(which(unlist(list(
      "eFG% Off" = "o_efg", "TO% Off" = "o_to_pct",
      "OReb%" = "o_reb_pct", "FT Rate Off" = "o_ftr",
      "eFG% Def" = "d_efg", "TO% Def" = "d_to_pct",
      "DReb%" = "d_reb_pct", "FT Rate Def" = "d_ftr")) == xv))
    ylab <- names(which(unlist(list(
      "eFG% Off" = "o_efg", "TO% Off" = "o_to_pct",
      "OReb%" = "o_reb_pct", "FT Rate Off" = "o_ftr",
      "eFG% Def" = "d_efg", "TO% Def" = "d_to_pct",
      "DReb%" = "d_reb_pct", "FT Rate Def" = "d_ftr")) == yv))
    if (!xv %in% names(d) || !yv %in% names(d)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "Column not available for this season", size = 5) + theme_void())
    }
    p <- ggplot(d, aes_string(x = xv, y = yv, color = "barthag")) +
      geom_point(size = 2.8, alpha = 0.85) +
      scale_color_gradient(low = "#aed6f1", high = "#1a3a5c",
                           name = "Barthag") +
      geom_hline(yintercept = mean(d[[yv]], na.rm = TRUE),
                 linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = mean(d[[xv]], na.rm = TRUE),
                 linetype = "dashed", color = "grey50") +
      labs(title = paste(xlab, "vs.", ylab),
           x = xlab, y = ylab) +
      theme_minimal(base_size = 12)
    if (input$ff_label) {
      if (have_ggrepel) {
        p <- p + ggrepel::geom_text_repel(aes_string(label = "team"),
                                           size = 2.5, max.overlaps = 20)
      } else {
        p <- p + geom_text(aes_string(label = "team"), size = 2.2,
                           vjust = -0.5, check_overlap = TRUE)
      }
    }
    p
  })

  output$ff_table <- renderDT({
    d <- rv$ff; req(d)
    cols <- c("team", "conf", "gp", "adj_oe", "adj_de", "barthag",
              "o_efg", "d_efg", "o_to_pct", "d_to_pct",
              "o_reb_pct", "d_reb_pct", "o_ftr", "d_ftr", "adj_tempo", "wab")
    datatable(
      d[, intersect(cols, names(d))],
      colnames = c("Team", "Conf", "GP", "Adj OE", "Adj DE", "Barthag",
                   "O eFG%", "D eFG%", "O TO%", "D TO%",
                   "O Reb%", "D Reb%", "O FTR", "D FTR",
                   "Tempo", "WAB")[seq_len(length(intersect(cols, names(d))))],
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 25, dom = "frtip",
                      scrollX = TRUE),
      class    = "stripe hover compact"
    ) |>
      formatRound(c("adj_oe", "adj_de", "adj_tempo", "wab"), 1) |>
      formatRound("barthag", 3) |>
      formatRound(c("o_efg", "d_efg", "o_to_pct", "d_to_pct",
                    "o_reb_pct", "d_reb_pct", "o_ftr", "d_ftr"), 3)
  })

  # ── Shooting Splits ───────────────────────────────────────────────────────
  observeEvent(input$load_sh, {
    withProgress(message = "Fetching shooting splits...", {
      rv$shooting <- tryCatch(
        torvik_shooting(
          year = as.integer(input$sh_year),
          conf = if (input$sh_conf == "All") "All" else input$sh_conf
        ),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  sh_side_data <- reactive({
    d  <- rv$shooting; req(d)
    s  <- input$sh_side
    rim_pct  <- paste0(s, "_rim_pct")
    rim_rate <- paste0(s, "_rim_rate")
    thr_pct  <- paste0(s, "_three_pct")
    thr_rate <- paste0(s, "_three_rate")
    mid_pct  <- paste0(s, "_mid_pct")
    mid_rate <- paste0(s, "_mid_rate")
    req(all(c(rim_pct, rim_rate, thr_pct, thr_rate) %in% names(d)))
    list(d = d, s = s,
         rim_pct = rim_pct, rim_rate = rim_rate,
         thr_pct = thr_pct, thr_rate = thr_rate,
         mid_pct = mid_pct, mid_rate = mid_rate)
  })

  output$sh_rim_plot <- renderPlot({
    x <- sh_side_data()
    d <- x$d
    side_lbl <- if (x$s == "o") "Offensive" else "Defensive"
    top20 <- d |> arrange(desc(.data[[x$rim_pct]])) |> head(20)
    ggplot(top20, aes(x = reorder(team, .data[[x$rim_pct]]),
                      y = .data[[x$rim_pct]], fill = .data[[x$rim_rate]])) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#fdebd0", high = "#e07b24",
                          name = "Rim Rate") +
      coord_flip() +
      labs(title = paste("Top 20 —", side_lbl, "Rim Shooting %"),
           subtitle = "Color = % of FGA at rim",
           x = NULL, y = "Rim FG%") +
      theme_minimal(base_size = 10)
  })

  output$sh_three_plot <- renderPlot({
    x <- sh_side_data()
    d <- x$d
    side_lbl <- if (x$s == "o") "Offensive" else "Defensive"
    top20 <- d |> arrange(desc(.data[[x$thr_rate]])) |> head(20)
    ggplot(top20, aes(x = reorder(team, .data[[x$thr_rate]]),
                      y = .data[[x$thr_rate]], fill = .data[[x$thr_pct]])) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#d5e8d4", high = "#2d6a4f",
                          name = "3P%") +
      coord_flip() +
      labs(title = paste("Top 20 —", side_lbl, "Three-Point Rate"),
           subtitle = "Color = 3P%",
           x = NULL, y = "% of FGA from Three") +
      theme_minimal(base_size = 10)
  })

  output$sh_table <- renderDT({
    x  <- sh_side_data()
    d  <- x$d
    s  <- x$s
    keep <- c("team", "conf", "gp",
              paste0(s, c("_rim_made", "_rim_att", "_rim_pct", "_rim_rate",
                          "_mid_pct", "_mid_rate",
                          "_three_made", "_three_att", "_three_pct", "_three_rate")))
    keep <- intersect(keep, names(d))
    datatable(
      d[, keep],
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 25, dom = "frtip", scrollX = TRUE),
      class    = "stripe hover compact"
    ) |>
      formatRound(grep("_pct|_rate", keep, value = TRUE), 3)
  })

  # ── Today's Slate ─────────────────────────────────────────────────────────
  observeEvent(input$load_today, {
    withProgress(message = "Fetching today's games...", {
      rv$today <- tryCatch(
        dayCast(year = as.integer(input$today_year)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$today_table <- renderDT({
    d <- rv$today
    req(!is.null(d))
    if (is.character(d)) {
      return(datatable(data.frame(Message = d), rownames = FALSE,
                       options = list(dom = "t")))
    }
    datatable(
      d,
      rownames = FALSE,
      options  = list(pageLength = 30, dom = "frtip", order = list(list(4, "desc"))),
      class    = "stripe hover compact"
    ) |>
      formatStyle("WinProb",
        background = styleColorBar(c(50, 100), "#cce5ff"),
        backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  output$today_plot <- renderPlot({
    d <- rv$today
    req(!is.null(d), is.data.frame(d), nrow(d) > 0)
    ggplot(d, aes(x = reorder(Matchup, TTQ), y = TTQ, fill = WinProb)) +
      geom_col(width = 0.65) +
      scale_fill_gradient(low = "#aed6f1", high = "#1a3a5c",
                          name = "Win%", na.value = "grey70") +
      coord_flip() +
      labs(title = paste("Today's Games —", format(Sys.Date(), "%b %d, %Y")),
           subtitle = "Sorted by Torvik Tier Quality (TTQ)",
           x = NULL, y = "TTQ") +
      theme_minimal(base_size = 11)
  })

  # ── Player Stats ──────────────────────────────────────────────────────────
  observeEvent(input$load_pl, {
    withProgress(message = "Fetching player data (this may take ~10 s)...", {
      rv$players <- tryCatch(
        player_stats(year = as.integer(input$pl_year)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  pl_filtered <- reactive({
    d <- rv$players; req(d)
    d |>
      filter(as.numeric(GP) >= input$pl_gp,
             as.numeric(usg) >= input$pl_usg)
  })

  output$pl_bpm_plot <- renderPlot({
    d <- pl_filtered()
    req(nrow(d) > 0, "bpm" %in% names(d))
    top25 <- d |>
      mutate(bpm = as.numeric(bpm)) |>
      filter(!is.na(bpm)) |>
      arrange(desc(bpm)) |>
      head(25)
    ggplot(top25, aes(x = reorder(player_name, bpm), y = bpm, fill = bpm)) +
      geom_col(width = 0.7) +
      scale_fill_gradient2(low = "#c0392b", mid = "#ecf0f1",
                           high = "#1a5276", midpoint = 0) +
      coord_flip() +
      labs(title = "Top 25 Players by BPM",
           x = NULL, y = "Box Plus/Minus") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none")
  })

  output$pl_ortg_plot <- renderPlot({
    d <- pl_filtered()
    req(nrow(d) > 0, "ORtg" %in% names(d), "usg" %in% names(d))
    d2 <- d |>
      mutate(ORtg = as.numeric(ORtg), usg = as.numeric(usg),
             bpm  = as.numeric(bpm)) |>
      filter(!is.na(ORtg), !is.na(usg))
    ggplot(d2, aes(x = usg, y = ORtg, color = bpm)) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_gradient2(low = "#c0392b", mid = "#bdc3c7",
                            high = "#1a5276", midpoint = 0, na.value = "grey70",
                            name = "BPM") +
      geom_hline(yintercept = 100, linetype = "dashed", color = "grey60") +
      labs(title = "Usage vs. Offensive Rating",
           x = "Usage Rate (%)", y = "Offensive Rating") +
      theme_minimal(base_size = 11)
  })

  output$pl_table <- renderDT({
    d <- pl_filtered(); req(nrow(d) > 0)
    keep <- c("player_name", "team", "conf", "GP", "Min_pct",
              "ORtg", "usg", "eFG", "TS_pct",
              "bpm", "obpm", "dbpm",
              "ORB_pct", "DRB_pct", "AST_pct", "TO_pct",
              "stl", "blk", "pts")
    keep <- intersect(keep, names(d))
    datatable(
      d[, keep],
      colnames = c("Player", "Team", "Conf", "GP", "Min%",
                   "ORtg", "Usg", "eFG%", "TS%",
                   "BPM", "OBPM", "DBPM",
                   "OReb%", "DReb%", "Ast%", "TO%",
                   "STL", "BLK", "PTS")[seq_len(length(keep))],
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 25, dom = "frtip", scrollX = TRUE),
      class    = "stripe hover compact"
    )
  })
  # ── Team Resume ───────────────────────────────────────────────────────────

  observeEvent(input$load_res_teams, {
    withProgress(message = "Loading team list...", {
      d <- tryCatch(
        torvik_team_ratings(year = as.integer(input$res_year), conf = "All"),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
      rv$res_teams <- d
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$res_team_ui <- renderUI({
    d <- rv$res_teams
    if (is.null(d) || nrow(d) == 0) {
      selectInput("res_team", "Team (load teams first)", choices = character(0))
    } else {
      teams <- sort(unique(d$team))
      selectInput("res_team", "Team", choices = teams, selected = teams[1], selectize = TRUE)
    }
  })

  observeEvent(input$load_res, {
    req(input$res_team)
    withProgress(message = paste("Loading games for", input$res_team, "..."), {
      rv$resume <- tryCatch(
        get_games(
          team   = input$res_team,
          season = as.integer(input$res_year),
          quad   = if (input$res_quad == "All") "All" else as.integer(input$res_quad)
        ),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  res_data <- reactive({
    d <- rv$resume; req(d, nrow(d) > 0)
    d
  })

  output$res_card_w <- renderUI({
    d <- res_data()
    w <- sum(grepl("^W", d$Result), na.rm = TRUE)
    div(class = "metric-card",
        div(class = "metric-val", style = "color:#27ae60;", w),
        div(class = "metric-lbl", "Wins"))
  })

  output$res_card_l <- renderUI({
    d <- res_data()
    l <- sum(grepl("^L", d$Result), na.rm = TRUE)
    div(class = "metric-card",
        div(class = "metric-val", style = "color:#c0392b;", l),
        div(class = "metric-lbl", "Losses"))
  })

  output$res_card_adjoe <- renderUI({
    d <- res_data()
    oe_col <- intersect(c("Adj. O", "adj_oe", "AdjOE"), names(d))[1]
    val <- if (!is.na(oe_col)) round(mean(as.numeric(d[[oe_col]]), na.rm = TRUE), 1) else NA
    div(class = "metric-card",
        div(class = "metric-val", if (!is.na(val)) val else "—"),
        div(class = "metric-lbl", "Avg Adj. OE"))
  })

  output$res_card_adjde <- renderUI({
    d <- res_data()
    de_col <- intersect(c("Adj. D", "adj_de", "AdjDE"), names(d))[1]
    val <- if (!is.na(de_col)) round(mean(as.numeric(d[[de_col]]), na.rm = TRUE), 1) else NA
    div(class = "metric-card",
        div(class = "metric-val", if (!is.na(val)) val else "—"),
        div(class = "metric-lbl", "Avg Adj. DE"))
  })

  output$res_result_plot <- renderPlot({
    d <- res_data()
    result_col <- intersect(c("Result", "result"), names(d))[1]
    opp_col    <- intersect(c("Opp", "opponent", "Opponent"), names(d))[1]
    oe_col     <- intersect(c("Adj. O", "adj_oe", "AdjOE"), names(d))[1]
    req(!is.na(result_col), !is.na(oe_col))
    d$wl  <- ifelse(grepl("^W", d[[result_col]]), "W", "L")
    d$oe  <- as.numeric(d[[oe_col]])
    d$opp <- if (!is.na(opp_col)) d[[opp_col]] else seq_len(nrow(d))
    d <- d[!is.na(d$oe), ]
    ggplot(d, aes(x = reorder(opp, oe), y = oe, fill = wl)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c("W" = "#27ae60", "L" = "#c0392b"), name = NULL) +
      coord_flip() +
      labs(title = paste("Adj. Offensive Efficiency by Game —", input$res_team),
           x = NULL, y = "Adj. OE") +
      theme_minimal(base_size = 10) +
      theme(axis.text.y = element_text(size = 8))
  })

  output$res_table <- renderDT({
    d <- res_data()
    datatable(
      d,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 30, dom = "frtip", scrollX = TRUE),
      class    = "stripe hover compact"
    )
  })

  # ── Super Schedule ────────────────────────────────────────────────────────

  observeEvent(input$load_ss, {
    withProgress(message = "Fetching super schedule (may take ~15 s)...", {
      rv$supersked <- tryCatch(
        get_super_sked(year = as.integer(input$ss_year)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  ss_data <- reactive({
    d <- rv$supersked; req(d, nrow(d) > 0)
    d
  })

  output$ss_ttq_plot <- renderPlot({
    d <- ss_data()
    ttq_col <- intersect(c("ttq", "TTQ"), names(d))[1]
    mu_col  <- intersect(c("matchup", "Matchup", "muid"), names(d))[1]
    req(!is.na(ttq_col))
    d$ttq_val <- as.numeric(d[[ttq_col]])
    d$label   <- if (!is.na(mu_col)) d[[mu_col]] else seq_len(nrow(d))
    top25 <- d[order(-d$ttq_val), ][seq_len(min(25, nrow(d))), ]
    ggplot(top25, aes(x = reorder(label, ttq_val), y = ttq_val, fill = ttq_val)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#d4e6f1", high = "#1a3a5c", guide = "none") +
      coord_flip() +
      labs(title = "Top 25 Games by Game Quality (TTQ)",
           x = NULL, y = "Torvik Tier Quality") +
      theme_minimal(base_size = 10) +
      theme(axis.text.y = element_text(size = 8))
  })

  output$ss_line_plot <- renderPlot({
    d <- ss_data()
    line_col <- intersect(c("line", "Line", "predicted_score"), names(d))[1]
    wp_col   <- intersect(c("pctChanceWin", "t1wp", "WinProb"), names(d))[1]
    req(!is.na(line_col), !is.na(wp_col))
    d$line_val <- as.numeric(d[[line_col]])
    d$wp_val   <- as.numeric(d[[wp_col]])
    d2 <- d[!is.na(d$line_val) & !is.na(d$wp_val), ]
    ggplot(d2, aes(x = line_val, y = wp_val)) +
      geom_point(alpha = 0.5, size = 1.8, color = "#1a3a5c") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = 50, linetype = "dashed", color = "grey50") +
      labs(title = "Predicted Line vs. Win Probability",
           x = "Predicted Spread (negative = underdog)",
           y = "Win Probability (%)") +
      theme_minimal(base_size = 11)
  })

  output$ss_table <- renderDT({
    d <- ss_data()
    datatable(
      d,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 30, dom = "frtip", scrollX = TRUE,
                      order = list(list(
                        which(names(d) %in% c("ttq", "TTQ"))[1] - 1L, "desc"
                      ))),
      class    = "stripe hover compact"
    )
  })

  # ── Time Machine ──────────────────────────────────────────────────────────

  observeEvent(input$load_tm, {
    date_str <- format(as.Date(input$tm_date), "%Y%m%d")
    withProgress(message = paste("Loading ratings for", format(as.Date(input$tm_date), "%b %d, %Y"), "..."), {
      rv$timemachine <- tryCatch(
        timeMachine_ratings(date = date_str),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  tm_data <- reactive({
    d <- rv$timemachine; req(d, nrow(d) > 0)
    conf_sel <- input$tm_conf
    if (!is.null(conf_sel) && conf_sel != "All") {
      conf_col <- intersect(c("conf", "Conf"), names(d))[1]
      if (!is.na(conf_col)) d <- d[d[[conf_col]] == conf_sel, ]
    }
    head(d, as.integer(input$tm_n))
  })

  output$tm_card_1 <- renderUI({
    d <- rv$timemachine; req(d)
    team_col <- intersect(c("team", "Team"), names(d))[1]
    div(class = "metric-card",
        div(class = "metric-val", if (!is.na(team_col)) d[[team_col]][1] else "—"),
        div(class = "metric-lbl", paste("#1 on", format(as.Date(input$tm_date), "%b %d, %Y"))))
  })

  output$tm_card_2 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("barthag", "Barthag"), names(d))[1]
    val <- if (!is.na(col)) sprintf("%.3f", max(as.numeric(d[[col]]), na.rm = TRUE)) else "—"
    div(class = "metric-card",
        div(class = "metric-val", val),
        div(class = "metric-lbl", "Highest Barthag"))
  })

  output$tm_card_3 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("adjoe", "adj_oe", "AdjOE"), names(d))[1]
    val <- if (!is.na(col)) sprintf("%.1f", max(as.numeric(d[[col]]), na.rm = TRUE)) else "—"
    div(class = "metric-card",
        div(class = "metric-val", val),
        div(class = "metric-lbl", "Best Adj. OE"))
  })

  output$tm_card_4 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("adjde", "adj_de", "AdjDE"), names(d))[1]
    val <- if (!is.na(col)) sprintf("%.1f", min(as.numeric(d[[col]]), na.rm = TRUE)) else "—"
    div(class = "metric-card",
        div(class = "metric-val", val),
        div(class = "metric-lbl", "Best Adj. DE"))
  })

  output$tm_barthag_plot <- renderPlot({
    d <- tm_data()
    team_col    <- intersect(c("team", "Team"), names(d))[1]
    barthag_col <- intersect(c("barthag", "Barthag"), names(d))[1]
    req(!is.na(team_col), !is.na(barthag_col))
    d$team_lbl  <- d[[team_col]]
    d$bart_val  <- as.numeric(d[[barthag_col]])
    d <- d[!is.na(d$bart_val), ]
    ggplot(d, aes(x = reorder(team_lbl, bart_val), y = bart_val, fill = bart_val)) +
      geom_col(width = 0.7) +
      scale_fill_gradient(low = "#cce5ff", high = "#1a3a5c", guide = "none") +
      coord_flip() +
      labs(title = paste("Barthag Power Rating —", format(as.Date(input$tm_date), "%b %d, %Y")),
           x = NULL, y = "Barthag") +
      theme_minimal(base_size = 10) +
      theme(axis.text.y = element_text(size = 8))
  })

  output$tm_table <- renderDT({
    d <- tm_data()
    datatable(
      d,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 25, dom = "frtip", scrollX = TRUE),
      class    = "stripe hover compact"
    )
  })

}

shinyApp(ui, server)
