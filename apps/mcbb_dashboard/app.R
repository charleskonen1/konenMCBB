## ============================================================
## konenMCBB — MCBB Advanced Stats Dashboard
## ============================================================
## Run from the project root:
##   shiny::runApp("apps/mcbb_dashboard")
## ============================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

if (!requireNamespace("konenMCBB", quietly = TRUE)) {
  devtools::load_all(normalizePath(file.path("..", ".."), mustWork = TRUE))
} else {
  library(konenMCBB)
}

have_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

# ── Logo lookup (ESPN CDN) ─────────────────────────────────────────────────────
espn_teams_df <- tryCatch(espn_teams(), error = function(e) NULL)

get_logo_url <- function(team_name) {
  if (is.null(espn_teams_df) || is.na(team_name)) return(NA_character_)
  clean  <- function(x) tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", x)))
  target <- clean(team_name)
  locs   <- clean(espn_teams_df$location)
  idx    <- which(locs == target)
  if (length(idx) == 0) idx <- which(startsWith(locs, substr(target, 1, 8)))
  if (length(idx) == 0) return(NA_character_)
  espn_teams_df$logo_url[idx[1]]
}

logo_img <- function(url, size = 28) {
  if (is.na(url) || is.null(url)) return("")
  sprintf('<img src="%s" height="%dpx" style="vertical-align:middle;">', url, size)
}

# Helper: scale 0-1 values to 0-100 for display
pct_scale <- function(x) {
  if (all(is.na(x))) return(x)
  if (median(x, na.rm = TRUE) < 2) x * 100 else x
}

# ── Season / conf helpers ──────────────────────────────────────────────────────
.month <- as.integer(format(Sys.Date(), "%m"))
current_year <- if (.month >= 11L) as.integer(format(Sys.Date(), "%Y")) + 1L else
                                    as.integer(format(Sys.Date(), "%Y"))
is_offseason <- .month >= 5L && .month <= 10L
default_year <- if (is_offseason) current_year - 1L else current_year

year_choices <- setNames(
  seq(current_year, 2008, by = -1),
  paste0(seq(current_year, 2008, by = -1) - 1, "-",
         substr(as.character(seq(current_year, 2008, by = -1)), 3, 4))
)

conf_choices <- c(
  "All Conferences" = "All",
  "ACC", "B10", "B12", "BE", "P12", "SEC",
  "Amer", "A10", "MWC", "WCC", "CUSA", "MAC",
  "MVC", "SBC", "CAA", "Horz", "OVC", "BSky",
  "SC", "ASun", "Sum", "NEC", "Pat", "MEAC", "SWAC", "Ind"
)

# ── Theme ─────────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "cosmo",
  primary    = "#1a3a5c",
  secondary  = "#e07b24",
  base_font  = font_google("Inter"),
  code_font  = font_google("Fira Code")
)

icon_text <- function(icon_name, text) {
  tagList(tags$i(class = paste0("fas fa-", icon_name)), " ", text)
}

stat_card <- function(val, lbl, color = "#1a3a5c") {
  div(
    style = "border-radius:8px; padding:14px 10px; background:#f8f9fa; margin-bottom:10px; text-align:center;",
    div(style = paste0("font-size:1.8rem; font-weight:700; color:", color, ";"), val),
    div(style = "font-size:0.8rem; color:#6c757d; margin-top:2px;", lbl)
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    tags$i(class = "fas fa-basketball-ball", style = "color:#e07b24; margin-right:8px;"),
    "MCBB Advanced Stats"
  ),
  theme  = app_theme,
  id     = "main_nav",
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$style(HTML("
        .metric-card { border-radius:8px; padding:14px 10px; background:#f8f9fa;
                       margin-bottom:10px; text-align:center; }
        .metric-val  { font-size:1.8rem; font-weight:700; color:#1a3a5c; }
        .metric-lbl  { font-size:0.78rem; color:#6c757d; margin-top:2px; }
        .section-hdr { font-size:1rem; font-weight:600; color:#1a3a5c;
                       border-bottom:2px solid #e07b24; padding-bottom:4px;
                       margin:18px 0 10px; }
        .team-logo   { vertical-align:middle; margin-right:6px; }
        .nav-link    { font-size:0.9rem; }
      "))
    ),
    if (is_offseason) div(
      class = "alert alert-warning mb-0",
      style = "border-radius:0; padding:7px 16px; font-size:0.88rem; text-align:center;",
      tags$strong("Off-season (May–Oct):"),
      " Current-season Torvik data may not be available yet. Historical data works for any year."
    )
  ),

  # ── Tab 1: T-Rank ─────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("trophy", "T-Rank"),
    value = "rankings",
    layout_sidebar(
      sidebar = sidebar(width = 220,
        selectInput("rank_year", "Season", choices = year_choices, selected = default_year),
        selectInput("rank_conf", "Conference", choices = conf_choices),
        sliderInput("rank_n", "Top N teams", min = 10, max = 363, value = 25, step = 5),
        hr(),
        actionButton("load_rankings", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      layout_columns(col_widths = c(3,3,3,3),
        uiOutput("rank_card_1"), uiOutput("rank_card_2"),
        uiOutput("rank_card_3"), uiOutput("rank_card_4")
      ),
      br(),
      div(class = "section-hdr", "Rankings Table"),
      DTOutput("rankings_table"),
      br(),
      div(class = "section-hdr", "Offense vs. Defense Landscape"),
      plotOutput("rank_scatter", height = "480px")
    )
  ),

  # ── Tab 2: Conference ─────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("chart-bar", "Conference"),
    value = "conference",
    layout_sidebar(
      sidebar = sidebar(width = 220,
        selectInput("conf_year", "Season", choices = year_choices, selected = default_year),
        numericInput("conf_min_teams", "Min teams", value = 8, min = 4, max = 20),
        hr(),
        actionButton("load_conf", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      div(class = "section-hdr", "Conference Strength (Avg. Barthag)"),
      plotOutput("conf_barthag_plot", height = "420px"),
      br(),
      layout_columns(col_widths = c(6,6),
        div(
          div(class = "section-hdr", "Offense vs. Defense by Conference"),
          plotOutput("conf_oe_de_plot", height = "360px")
        ),
        div(
          div(class = "section-hdr", "Pace by Conference"),
          plotOutput("conf_tempo_plot", height = "360px")
        )
      ),
      br(),
      div(class = "section-hdr", "Conference Summary Table"),
      DTOutput("conf_table")
    )
  ),

  # ── Tab 3: Four Factors ───────────────────────────────────────────────────
  nav_panel(
    title = icon_text("sliders", "Four Factors"),
    value = "fourfactors",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("ff_year", "Season", choices = year_choices, selected = default_year),
        selectInput("ff_conf", "Conference", choices = conf_choices),
        hr(),
        selectInput("ff_x", "X axis",
          choices = list(
            "Offensive" = c("eFG% Off"="o_efg","TO% Off"="o_to_pct","OReb%"="o_reb_pct","FT Rate Off"="o_ftr"),
            "Defensive" = c("eFG% Def"="d_efg","TO% Def"="d_to_pct","DReb%"="d_reb_pct","FT Rate Def"="d_ftr")
          ), selected = "o_efg"),
        selectInput("ff_y", "Y axis",
          choices = list(
            "Offensive" = c("eFG% Off"="o_efg","TO% Off"="o_to_pct","OReb%"="o_reb_pct","FT Rate Off"="o_ftr"),
            "Defensive" = c("eFG% Def"="d_efg","TO% Def"="d_to_pct","DReb%"="d_reb_pct","FT Rate Def"="d_ftr")
          ), selected = "d_efg"),
        checkboxInput("ff_label", "Label points", value = FALSE),
        hr(),
        actionButton("load_ff", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      div(class = "section-hdr", "Four Factors Scatter"),
      plotOutput("ff_scatter", height = "520px"),
      br(),
      div(class = "section-hdr", "Four Factors Table (%)"),
      p("eFG%, TO%, Reb%, FT Rate all shown as percentages.", style="font-size:0.83em;color:#6c757d;"),
      DTOutput("ff_table")
    )
  ),

  # ── Tab 4: Shooting ───────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("bullseye", "Shooting"),
    value = "shooting",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("sh_year", "Season", choices = year_choices, selected = default_year),
        selectInput("sh_conf", "Conference", choices = conf_choices),
        selectInput("sh_side", "Side", choices = c("Offense"="o","Defense"="d"), selected = "o"),
        hr(),
        actionButton("load_sh", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      div(class = "section-hdr", "Shot Zone Distribution (avg % of FGA by zone)"),
      plotOutput("sh_zone_dist", height = "340px"),
      br(),
      layout_columns(col_widths = c(6,6),
        div(
          div(class = "section-hdr", "Top 20 — Rim FG%"),
          plotOutput("sh_rim_plot", height = "360px")
        ),
        div(
          div(class = "section-hdr", "Top 20 — Three-Point Rate"),
          plotOutput("sh_three_plot", height = "360px")
        )
      ),
      br(),
      div(class = "section-hdr", "Full Shooting Table (%)"),
      p("All pct and rate values shown as percentages (e.g. 64.2 = 64.2%).", style="font-size:0.83em;color:#6c757d;"),
      DTOutput("sh_table")
    )
  ),

  # ── Tab 5: Today's Slate ──────────────────────────────────────────────────
  nav_panel(
    title = icon_text("calendar-day", "Today's Slate"),
    value = "today",
    layout_sidebar(
      sidebar = sidebar(width = 220,
        p(strong("Date:"), format(Sys.Date(), "%B %d, %Y")),
        selectInput("today_year", "Season", choices = year_choices, selected = default_year),
        hr(),
        actionButton("load_today", "Refresh Slate", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      div(class = "section-hdr", "Games on Today's Board"),
      DTOutput("today_table"),
      br(),
      plotOutput("today_plot", height = "360px")
    )
  ),

  # ── Tab 6: Team Profile ────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("shirt", "Team Profile"),
    value = "team_profile",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("tp_year", "Season", choices = year_choices, selected = default_year),
        uiOutput("tp_team_ui"),
        hr(),
        actionButton("load_tp_list", "Load Teams", class = "btn-secondary w-100", icon = icon("download")),
        br(), br(),
        actionButton("load_tp", "Load Profile", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      uiOutput("tp_header"),
      br(),
      layout_columns(col_widths = c(3,3,3,3),
        uiOutput("tp_card_rank"), uiOutput("tp_card_barthag"),
        uiOutput("tp_card_oe"),   uiOutput("tp_card_de")
      ),
      br(),
      layout_columns(col_widths = c(6,6),
        div(
          div(class = "section-hdr", "Four Factors"),
          plotOutput("tp_ff_plot", height = "320px")
        ),
        div(
          div(class = "section-hdr", "Shot Zone Profile"),
          plotOutput("tp_shot_plot", height = "320px")
        )
      ),
      br(),
      div(class = "section-hdr", "Season Stats Summary"),
      tableOutput("tp_stats_table"),
      br(),
      div(class = "section-hdr", "Game Log"),
      DTOutput("tp_game_log")
    )
  ),

  # ── Tab 7: Players ────────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("person", "Players"),
    value = "players",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("pl_year", "Season", choices = year_choices, selected = default_year),
        sliderInput("pl_gp",  "Min games played", min = 1, max = 35, value = 10),
        sliderInput("pl_usg", "Min usage %", min = 0, max = 35, value = 15),
        hr(),
        p("Click any row to view a player's full stat line.", style="font-size:0.83em;color:#6c757d;"),
        actionButton("load_pl", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      uiOutput("player_profile_panel"),
      layout_columns(col_widths = c(6,6),
        div(
          div(class = "section-hdr", "Top 25 by BPM"),
          plotOutput("pl_bpm_plot", height = "380px")
        ),
        div(
          div(class = "section-hdr", "Usage vs. Offensive Rating"),
          plotOutput("pl_ortg_plot", height = "380px")
        )
      ),
      br(),
      div(class = "section-hdr", "Player Table — click a row to view full profile"),
      DTOutput("pl_table")
    )
  ),

  # ── Tab 8: Team Resume ────────────────────────────────────────────────────
  nav_panel(
    title = icon_text("clipboard-list", "Team Resume"),
    value = "resume",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("res_year", "Season", choices = year_choices, selected = default_year),
        uiOutput("res_team_ui"),
        selectInput("res_quad", "Quadrant filter",
                    choices = c("All"="All","Quad 1"="1","Quad 2"="2","Quad 3"="3","Quad 4"="4")),
        hr(),
        actionButton("load_res_teams", "Load Teams", class = "btn-secondary w-100", icon = icon("download")),
        br(), br(),
        actionButton("load_res", "Load Games", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      layout_columns(col_widths = c(3,3,3,3),
        uiOutput("res_card_w"), uiOutput("res_card_l"),
        uiOutput("res_card_adjoe"), uiOutput("res_card_adjde")
      ),
      br(),
      div(class = "section-hdr", "Adj. OE by Game"),
      plotOutput("res_result_plot", height = "300px"),
      br(),
      div(class = "section-hdr", "Game Log"),
      DTOutput("res_table")
    )
  ),

  # ── Tab 9: Super Schedule ─────────────────────────────────────────────────
  nav_panel(
    title = icon_text("calendar-week", "Super Schedule"),
    value = "supersked",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        selectInput("ss_year", "Season", choices = year_choices, selected = default_year),
        hr(),
        p("All scheduled games with Torvik predictions, win probabilities, and game quality (TTQ).",
          style="font-size:0.83em;color:#6c757d;"),
        actionButton("load_ss", "Load / Refresh", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      layout_columns(col_widths = c(6,6),
        div(div(class="section-hdr","Top Games by Quality (TTQ)"),
            plotOutput("ss_ttq_plot", height = "380px")),
        div(div(class="section-hdr","Spread vs. Win Probability"),
            plotOutput("ss_line_plot", height = "380px"))
      ),
      br(),
      div(class = "section-hdr", "Full Schedule"),
      DTOutput("ss_table")
    )
  ),

  # ── Tab 10: Time Machine ──────────────────────────────────────────────────
  nav_panel(
    title = icon_text("clock-rotate-left", "Time Machine"),
    value = "timemachine",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        dateInput("tm_date", "Snapshot date",
                  value = Sys.Date() - 1,
                  min   = as.Date("2014-11-13"),
                  max   = Sys.Date() - 1),
        selectInput("tm_conf", "Conference", choices = conf_choices),
        sliderInput("tm_n", "Top N teams", min = 10, max = 363, value = 25, step = 5),
        hr(),
        actionButton("load_tm", "Load Snapshot", class = "btn-primary w-100", icon = icon("rotate"))
      ),
      layout_columns(col_widths = c(3,3,3,3),
        uiOutput("tm_card_1"), uiOutput("tm_card_2"),
        uiOutput("tm_card_3"), uiOutput("tm_card_4")
      ),
      br(),
      div(class = "section-hdr", "Barthag Power Rating"),
      plotOutput("tm_barthag_plot", height = "420px"),
      br(),
      div(class = "section-hdr", "Full Snapshot Table"),
      DTOutput("tm_table")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    rankings    = NULL,
    conf        = NULL,
    ff          = NULL,
    shooting    = NULL,
    today       = NULL,
    players     = NULL,
    tp_teams    = NULL,
    tp_data     = NULL,
    tp_games    = NULL,
    tp_shooting = NULL,
    res_teams   = NULL,
    resume      = NULL,
    supersked   = NULL,
    timemachine = NULL
  )

  # ── T-Rank ────────────────────────────────────────────────────────────────
  observeEvent(input$load_rankings, {
    withProgress(message = "Fetching T-Rank ratings...", {
      rv$rankings <- tryCatch(
        torvik_team_ratings(year = as.integer(input$rank_year),
                            conf = if (input$rank_conf == "All") "All" else input$rank_conf),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  rank_data <- reactive({
    req(rv$rankings)
    rv$rankings |> head(as.integer(input$rank_n))
  })

  output$rank_card_1 <- renderUI({
    d <- rv$rankings; req(d)
    stat_card(d$team[1], "#1 Ranked Team")
  })
  output$rank_card_2 <- renderUI({
    d <- rv$rankings; req(d)
    stat_card(sprintf("%.3f", max(d$barthag, na.rm=TRUE)), "Best Barthag", "#27ae60")
  })
  output$rank_card_3 <- renderUI({
    d <- rv$rankings; req(d)
    stat_card(sprintf("%.1f", max(d$adj_oe, na.rm=TRUE)), "Best Adj. OE", "#2980b9")
  })
  output$rank_card_4 <- renderUI({
    d <- rv$rankings; req(d)
    stat_card(sprintf("%.1f", min(d$adj_de, na.rm=TRUE)), "Best Adj. DE (lowest)", "#8e44ad")
  })

  output$rankings_table <- renderDT({
    d <- rank_data()
    logo_col <- sapply(d$team, function(t) logo_img(get_logo_url(t), 24))
    d2 <- d |>
      select(rank, team, conf, record, adj_oe, adj_de, barthag, adj_tempo, wab,
             proj_w, proj_l, sos) |>
      mutate(
        Logo     = logo_col,
        barthag  = round(barthag * 100, 1),
        adj_oe   = round(adj_oe, 1),
        adj_de   = round(adj_de, 1),
        adj_tempo = round(adj_tempo, 1),
        wab      = round(wab, 1),
        sos      = round(sos, 3),
        proj_w   = round(proj_w, 1),
        proj_l   = round(proj_l, 1)
      ) |>
      select(rank, Logo, team, conf, record, adj_oe, adj_de, barthag,
             adj_tempo, wab, proj_w, proj_l, sos)

    datatable(
      d2,
      escape   = FALSE,
      rownames = FALSE,
      colnames = c("Rk","","Team","Conf","Record","Adj OE","Adj DE",
                   "Barthag %","Tempo","WAB","Proj W","Proj L","SOS"),
      options  = list(
        pageLength = 25, dom = "frtip",
        order      = list(list(0, "asc")),
        columnDefs = list(list(orderable=FALSE, targets=1))
      ),
      class = "stripe hover compact"
    ) |>
      formatStyle("adj_oe",  background = styleColorBar(range(d2$adj_oe,  na.rm=TRUE), "#cce5ff"),
                  backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") |>
      formatStyle("adj_de",  background = styleColorBar(range(d2$adj_de,  na.rm=TRUE), "#f5cba7"),
                  backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") |>
      formatStyle("barthag", background = styleColorBar(c(0, 100), "#d5f4e6"),
                  backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  output$rank_scatter <- renderPlot({
    d <- rank_data()
    p <- ggplot(d, aes(x=adj_oe, y=adj_de, size=barthag, color=barthag, label=team)) +
      geom_point(alpha=0.75) +
      scale_color_gradient(low="#aed6f1", high="#1a3a5c", name="Barthag") +
      scale_size_continuous(range=c(2,9), guide="none") +
      scale_y_reverse() +
      geom_hline(yintercept=mean(d$adj_de, na.rm=TRUE), linetype="dashed", color="grey50", linewidth=0.5) +
      geom_vline(xintercept=mean(d$adj_oe, na.rm=TRUE), linetype="dashed", color="grey50", linewidth=0.5) +
      annotate("text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.5, label="Elite Defense", color="grey50", size=3) +
      annotate("text", x=Inf,  y=Inf, hjust=1.1,  vjust=1.5, label="Two-Way",       color="grey50", size=3) +
      annotate("text", x=Inf,  y=-Inf,hjust=1.1,  vjust=-0.5,label="Elite Offense", color="grey50", size=3) +
      labs(title="Adjusted Offense vs. Defense — bubble = Barthag",
           subtitle="Upper-right = elite two-way team | Lower-right = elite offense | Upper-left = elite defense",
           x="Adj. Offensive Efficiency", y="Adj. Defensive Efficiency (lower = better)") +
      theme_minimal(base_size=12) +
      theme(legend.position="right")

    if (have_ggrepel) {
      p <- p + ggrepel::geom_text_repel(size=2.8, max.overlaps=15, color="#333")
    } else {
      p <- p + geom_text(size=2.5, vjust=-0.8, check_overlap=TRUE, color="#333")
    }
    p
  })

  # ── Conference ────────────────────────────────────────────────────────────
  observeEvent(input$load_conf, {
    withProgress(message = "Fetching conference data...", {
      rv$conf <- tryCatch(
        torvik_conf_stats(year=as.integer(input$conf_year), min_teams=as.integer(input$conf_min_teams)),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$conf_barthag_plot <- renderPlot({
    d <- rv$conf; req(d)
    d$mean_barthag_pct <- d$mean_barthag * 100
    ggplot(d, aes(x=reorder(conf, mean_barthag_pct), y=mean_barthag_pct, fill=mean_barthag_pct)) +
      geom_col(width=0.65) +
      geom_text(aes(label=paste0(top_team, "\n", sprintf("%.1f%%", top_barthag*100))),
                hjust=-0.05, size=2.8, color="#333") +
      scale_fill_gradient(low="#d4e6f1", high="#1a3a5c", guide="none") +
      coord_flip(clip="off") +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(title="Conference Strength — Avg. Barthag %", subtitle="Label = best team in conference",
           x=NULL, y="Avg. Barthag (%)") +
      theme_minimal(base_size=12) +
      theme(plot.margin=margin(r=90))
  })

  output$conf_oe_de_plot <- renderPlot({
    d <- rv$conf; req(d)
    ggplot(d, aes(x=mean_adj_oe, y=mean_adj_de, label=conf, color=mean_barthag)) +
      geom_point(size=3.5) +
      geom_text(nudge_y=0.1, size=3.2) +
      scale_color_gradient(low="#aed6f1", high="#1a3a5c", name="Avg\nBarthag") +
      scale_y_reverse() +
      geom_hline(yintercept=mean(d$mean_adj_de, na.rm=TRUE), linetype="dashed", color="grey60") +
      geom_vline(xintercept=mean(d$mean_adj_oe, na.rm=TRUE), linetype="dashed", color="grey60") +
      labs(title="Offense vs. Defense by Conference",
           x="Avg. Adj. OE", y="Avg. Adj. DE (lower = better)") +
      theme_minimal(base_size=11)
  })

  output$conf_tempo_plot <- renderPlot({
    d <- rv$conf; req(d)
    ggplot(d, aes(x=reorder(conf, mean_adj_tempo), y=mean_adj_tempo)) +
      geom_col(fill="#e07b24", width=0.65) +
      geom_text(aes(label=round(mean_adj_tempo,1)), hjust=-0.2, size=3) +
      coord_flip(clip="off") +
      labs(title="Pace by Conference", x=NULL, y="Avg. Adj. Tempo (poss/40 min)") +
      theme_minimal(base_size=11)
  })

  output$conf_table <- renderDT({
    d <- rv$conf; req(d)
    d2 <- d |>
      mutate(
        mean_barthag  = round(mean_barthag * 100, 1),
        top_barthag   = round(top_barthag  * 100, 1),
        mean_adj_oe   = round(mean_adj_oe, 1),
        mean_adj_de   = round(mean_adj_de, 1),
        mean_adj_tempo = round(mean_adj_tempo, 1),
        mean_wab      = round(mean_wab, 1),
        total_wab     = round(total_wab, 1)
      )
    cols_show <- c("conf","n_teams","mean_barthag","mean_adj_oe","mean_adj_de",
                   "mean_adj_tempo","mean_wab","total_wab","top_team","top_barthag")
    datatable(
      d2[, intersect(cols_show, names(d2))],
      colnames = c("Conf","Teams","Avg Barthag %","Avg Adj OE","Avg Adj DE",
                   "Avg Tempo","Avg WAB","Total WAB","Top Team","Top Barthag %"),
      rownames = FALSE,
      options  = list(pageLength=20, dom="frtip"),
      class    = "stripe hover compact"
    )
  })

  # ── Four Factors ──────────────────────────────────────────────────────────
  observeEvent(input$load_ff, {
    withProgress(message = "Fetching four factors...", {
      rv$ff <- tryCatch(
        torvik_four_factors(year=as.integer(input$ff_year),
                            conf=if(input$ff_conf=="All")"All" else input$ff_conf),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  ff_display <- reactive({
    d <- rv$ff; req(d)
    pct_cols <- c("o_efg","o_to_pct","o_reb_pct","o_ftr","d_efg","d_to_pct","d_reb_pct","d_ftr")
    for (col in intersect(pct_cols, names(d))) {
      d[[col]] <- round(pct_scale(d[[col]]), 1)
    }
    d
  })

  output$ff_scatter <- renderPlot({
    d  <- ff_display()
    xv <- input$ff_x; yv <- input$ff_y
    lbl_map <- c(
      o_efg="eFG% Off", o_to_pct="TO% Off", o_reb_pct="OReb%", o_ftr="FT Rate Off",
      d_efg="eFG% Def", d_to_pct="TO% Def", d_reb_pct="DReb%", d_ftr="FT Rate Def"
    )
    if (!xv %in% names(d) || !yv %in% names(d)) {
      return(ggplot() + annotate("text",x=.5,y=.5,label="Column not available",size=5) + theme_void())
    }
    p <- ggplot(d, aes_string(x=xv, y=yv, color="barthag")) +
      geom_point(size=2.8, alpha=0.82) +
      scale_color_gradient(low="#aed6f1", high="#1a3a5c", name="Barthag") +
      geom_hline(yintercept=mean(d[[yv]], na.rm=TRUE), linetype="dashed", color="grey50") +
      geom_vline(xintercept=mean(d[[xv]], na.rm=TRUE), linetype="dashed", color="grey50") +
      labs(title=paste(lbl_map[xv], "vs.", lbl_map[yv]),
           x=paste0(lbl_map[xv]," (%)"), y=paste0(lbl_map[yv]," (%)")) +
      theme_minimal(base_size=12)
    if (input$ff_label) {
      fn <- if (have_ggrepel) function(p) p + ggrepel::geom_text_repel(aes_string(label="team"), size=2.5, max.overlaps=20)
            else              function(p) p + geom_text(aes_string(label="team"), size=2.2, vjust=-0.6, check_overlap=TRUE)
      p <- fn(p)
    }
    p
  })

  output$ff_table <- renderDT({
    d <- ff_display()
    cols <- c("team","conf","gp","adj_oe","adj_de","barthag",
              "o_efg","d_efg","o_to_pct","d_to_pct",
              "o_reb_pct","d_reb_pct","o_ftr","d_ftr","adj_tempo","wab")
    d2 <- d[, intersect(cols, names(d))]
    if ("barthag" %in% names(d2)) d2$barthag <- round(d2$barthag * 100, 1)
    datatable(
      d2,
      colnames = c("Team","Conf","GP","Adj OE","Adj DE","Barthag %",
                   "O eFG%","D eFG%","O TO%","D TO%",
                   "O Reb%","D Reb%","O FTR%","D FTR%",
                   "Tempo","WAB")[seq_len(ncol(d2))],
      rownames = FALSE, filter="top",
      options  = list(pageLength=25, dom="frtip", scrollX=TRUE),
      class    = "stripe hover compact"
    ) |>
      formatStyle("o_efg", background=styleColorBar(range(d2$o_efg, na.rm=TRUE),"#d5f4e6"),
                  backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") |>
      formatStyle("d_efg", background=styleColorBar(range(d2$d_efg, na.rm=TRUE),"#f5cba7"),
                  backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  # ── Shooting ──────────────────────────────────────────────────────────────
  observeEvent(input$load_sh, {
    withProgress(message = "Fetching shooting splits...", {
      rv$shooting <- tryCatch(
        torvik_shooting(year=as.integer(input$sh_year),
                        conf=if(input$sh_conf=="All")"All" else input$sh_conf),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  sh_display <- reactive({
    d <- rv$shooting; req(d)
    pct_rate_cols <- grep("_pct|_rate", names(d), value=TRUE)
    for (col in pct_rate_cols) {
      d[[col]] <- round(pct_scale(d[[col]]), 1)
    }
    d
  })

  output$sh_zone_dist <- renderPlot({
    d <- sh_display()
    s <- input$sh_side
    rim_r   <- paste0(s,"_rim_rate");   mid_r  <- paste0(s,"_mid_rate")
    three_r <- paste0(s,"_three_rate")
    req(all(c(rim_r, mid_r, three_r) %in% names(d)))
    avgs <- tibble(
      Zone  = c("Rim","Mid-Range","Three"),
      Rate  = c(mean(d[[rim_r]], na.rm=TRUE),
                mean(d[[mid_r]], na.rm=TRUE),
                mean(d[[three_r]], na.rm=TRUE))
    )
    avgs$Zone <- factor(avgs$Zone, levels=c("Three","Mid-Range","Rim"))
    side_lbl <- if (s=="o") "Offensive" else "Defensive"
    ggplot(avgs, aes(x="Average", y=Rate, fill=Zone)) +
      geom_col(width=0.55) +
      geom_text(aes(label=paste0(round(Rate,1),"%")), position=position_stack(vjust=0.5),
                color="white", fontface="bold", size=4.5) +
      scale_fill_manual(values=c("Rim"="#1a3a5c","Mid-Range"="#e07b24","Three"="#27ae60")) +
      coord_flip() +
      labs(title=paste(side_lbl, "— Average Shot Zone Distribution"),
           x=NULL, y="% of FGA") +
      theme_minimal(base_size=13) +
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  })

  output$sh_rim_plot <- renderPlot({
    d <- sh_display()
    s <- input$sh_side
    rim_pct  <- paste0(s,"_rim_pct");  rim_rate <- paste0(s,"_rim_rate")
    req(all(c(rim_pct, rim_rate) %in% names(d)))
    side_lbl <- if (s=="o") "Offensive" else "Defensive"
    top20 <- d |> arrange(desc(.data[[rim_pct]])) |> head(20)
    ggplot(top20, aes(x=reorder(team, .data[[rim_pct]]), y=.data[[rim_pct]], fill=.data[[rim_rate]])) +
      geom_col(width=0.7) +
      scale_fill_gradient(low="#fdebd0", high="#e07b24", name="Rim\nRate %") +
      geom_text(aes(label=paste0(.data[[rim_pct]],"%")), hjust=-0.1, size=3) +
      coord_flip(clip="off") +
      labs(title=paste("Top 20 —", side_lbl, "Rim FG%"), x=NULL, y="Rim FG%") +
      theme_minimal(base_size=10) +
      theme(plot.margin=margin(r=30))
  })

  output$sh_three_plot <- renderPlot({
    d <- sh_display()
    s <- input$sh_side
    thr_rate <- paste0(s,"_three_rate"); thr_pct <- paste0(s,"_three_pct")
    req(all(c(thr_rate, thr_pct) %in% names(d)))
    side_lbl <- if (s=="o") "Offensive" else "Defensive"
    top20 <- d |> arrange(desc(.data[[thr_rate]])) |> head(20)
    ggplot(top20, aes(x=reorder(team, .data[[thr_rate]]), y=.data[[thr_rate]], fill=.data[[thr_pct]])) +
      geom_col(width=0.7) +
      scale_fill_gradient(low="#d5e8d4", high="#2d6a4f", name="3P%") +
      geom_text(aes(label=paste0(.data[[thr_rate]],"%")), hjust=-0.1, size=3) +
      coord_flip(clip="off") +
      labs(title=paste("Top 20 —", side_lbl, "Three-Point Rate"), x=NULL, y="% of FGA from Three") +
      theme_minimal(base_size=10) +
      theme(plot.margin=margin(r=30))
  })

  output$sh_table <- renderDT({
    d <- sh_display()
    s <- input$sh_side
    keep <- c("team","conf","gp",
              paste0(s, c("_rim_pct","_rim_rate","_mid_pct","_mid_rate",
                          "_three_pct","_three_rate","_dunk_pct")))
    keep <- intersect(keep, names(d))
    d2 <- d[, keep]
    col_lbls <- c("Team","Conf","GP","Rim FG%","Rim Rate%","Mid FG%","Mid Rate%",
                  "3P FG%","3P Rate%","Dunk%")[seq_len(length(keep))]
    datatable(
      d2, colnames=col_lbls, rownames=FALSE, filter="top",
      options = list(pageLength=25, dom="frtip", scrollX=TRUE),
      class   = "stripe hover compact"
    ) |>
      formatStyle(intersect(c(paste0(s,"_rim_pct"), paste0(s,"_three_pct")), keep),
        background=styleColorBar(c(0,100), "#cce5ff"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  # ── Today's Slate ─────────────────────────────────────────────────────────
  observeEvent(input$load_today, {
    withProgress(message = "Fetching today's games...", {
      rv$today <- tryCatch(
        dayCast(year=as.integer(input$today_year)),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$today_table <- renderDT({
    d <- rv$today; req(!is.null(d))
    if (is.character(d)) {
      return(datatable(data.frame(Message=d), rownames=FALSE, options=list(dom="t")))
    }
    datatable(d, rownames=FALSE,
              options=list(pageLength=30, dom="frtip", order=list(list(4,"desc"))),
              class="stripe hover compact") |>
      formatStyle("WinProb",
        background=styleColorBar(c(50,100),"#cce5ff"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  output$today_plot <- renderPlot({
    d <- rv$today; req(!is.null(d), is.data.frame(d), nrow(d)>0)
    ggplot(d, aes(x=reorder(Matchup, TTQ), y=TTQ, fill=WinProb)) +
      geom_col(width=0.65) +
      scale_fill_gradient(low="#aed6f1", high="#1a3a5c", name="Win%", na.value="grey70") +
      coord_flip() +
      labs(title=paste("Today's Games —", format(Sys.Date(),"%b %d, %Y")),
           subtitle="Sorted by Torvik Tier Quality (TTQ)", x=NULL, y="TTQ") +
      theme_minimal(base_size=11)
  })

  # ── Team Profile ──────────────────────────────────────────────────────────
  observeEvent(input$load_tp_list, {
    withProgress(message="Loading team list...", {
      rv$tp_teams <- tryCatch(
        torvik_team_ratings(year=as.integer(input$tp_year), conf="All"),
        error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  output$tp_team_ui <- renderUI({
    d <- rv$tp_teams
    if (is.null(d) || nrow(d)==0) {
      selectInput("tp_team", "Team (load teams first)", choices=character(0))
    } else {
      selectInput("tp_team", "Team", choices=sort(unique(d$team)), selectize=TRUE)
    }
  })

  observeEvent(input$load_tp, {
    req(input$tp_team)
    yr <- as.integer(input$tp_year)
    withProgress(message=paste("Loading profile for", input$tp_team, "..."), {
      rv$tp_data <- tryCatch(
        torvik_team_ratings(year=yr, conf="All") |> filter(team == input$tp_team),
        error=function(e) NULL
      )
      rv$tp_games <- tryCatch(
        get_games(team=input$tp_team, season=yr, quad="All"),
        error=function(e) NULL
      )
      rv$tp_shooting <- tryCatch(
        torvik_shooting(year=yr, conf="All") |> filter(team == input$tp_team),
        error=function(e) NULL
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  output$tp_header <- renderUI({
    d <- rv$tp_data; req(d, nrow(d)>0)
    logo_url <- get_logo_url(d$team[1])
    div(style="display:flex; align-items:center; gap:16px; padding:8px 0;",
      if (!is.na(logo_url)) tags$img(src=logo_url, height="60px"),
      div(
        h3(d$team[1], style="margin:0; color:#1a3a5c; font-weight:700;"),
        p(paste0(d$conf[1], " · ", d$record[1], " · Rank #", d$rank[1]),
          style="margin:0; color:#6c757d;")
      )
    )
  })

  output$tp_card_rank    <- renderUI({ d <- rv$tp_data; req(d,nrow(d)>0); stat_card(paste0("#",d$rank[1]),"T-Rank") })
  output$tp_card_barthag <- renderUI({ d <- rv$tp_data; req(d,nrow(d)>0); stat_card(paste0(round(d$barthag[1]*100,1),"%"),"Barthag","#27ae60") })
  output$tp_card_oe      <- renderUI({ d <- rv$tp_data; req(d,nrow(d)>0); stat_card(round(d$adj_oe[1],1),"Adj. OE","#2980b9") })
  output$tp_card_de      <- renderUI({ d <- rv$tp_data; req(d,nrow(d)>0); stat_card(round(d$adj_de[1],1),"Adj. DE","#8e44ad") })

  output$tp_ff_plot <- renderPlot({
    ff_raw <- tryCatch(
      torvik_four_factors(year=as.integer(input$tp_year), conf="All") |>
        filter(team == input$tp_team),
      error=function(e) NULL
    )
    req(!is.null(ff_raw), nrow(ff_raw)>0)
    factors <- tibble(
      Factor = c("eFG% Off","eFG% Def","TO% Off","TO% Def","OReb%","DReb%","FTR Off","FTR Def"),
      Value  = c(pct_scale(ff_raw$o_efg)[1], pct_scale(ff_raw$d_efg)[1],
                 pct_scale(ff_raw$o_to_pct)[1], pct_scale(ff_raw$d_to_pct)[1],
                 pct_scale(ff_raw$o_reb_pct)[1], pct_scale(ff_raw$d_reb_pct)[1],
                 pct_scale(ff_raw$o_ftr)[1], pct_scale(ff_raw$d_ftr)[1]),
      Side   = c("Off","Def","Off","Def","Off","Def","Off","Def")
    )
    ggplot(factors, aes(x=reorder(Factor, Value), y=Value, fill=Side)) +
      geom_col(width=0.7) +
      geom_text(aes(label=paste0(round(Value,1),"%")), hjust=-0.1, size=3.2) +
      scale_fill_manual(values=c("Off"="#1a3a5c","Def"="#e07b24")) +
      coord_flip(clip="off") +
      labs(x=NULL, y="%") +
      theme_minimal(base_size=11) +
      theme(plot.margin=margin(r=30), legend.position="top")
  })

  output$tp_shot_plot <- renderPlot({
    sh <- rv$tp_shooting; req(!is.null(sh), nrow(sh)>0)
    sh_d <- sh
    pct_cols <- grep("_pct|_rate", names(sh_d), value=TRUE)
    for (col in pct_cols) sh_d[[col]] <- round(pct_scale(sh_d[[col]]), 1)

    zones <- tibble(
      Zone = rep(c("Rim","Mid","Three"), 2),
      Rate = c(sh_d$o_rim_rate[1], sh_d$o_mid_rate[1], sh_d$o_three_rate[1],
               sh_d$d_rim_rate[1], sh_d$d_mid_rate[1], sh_d$d_three_rate[1]),
      Pct  = c(sh_d$o_rim_pct[1], sh_d$o_mid_pct[1], sh_d$o_three_pct[1],
               sh_d$d_rim_pct[1], sh_d$d_mid_pct[1], sh_d$d_three_pct[1]),
      Side = c(rep("Offense",3), rep("Defense",3))
    )
    zones$Zone <- factor(zones$Zone, levels=c("Rim","Mid","Three"))
    ggplot(zones, aes(x=Zone, y=Rate, fill=Side)) +
      geom_col(position="dodge", width=0.65) +
      geom_text(aes(label=paste0(round(Rate,1),"%")),
                position=position_dodge(width=0.65), vjust=-0.4, size=3) +
      scale_fill_manual(values=c("Offense"="#1a3a5c","Defense"="#e07b24")) +
      labs(x="Shot Zone", y="% of FGA", title=NULL) +
      theme_minimal(base_size=11) +
      theme(legend.position="top")
  })

  output$tp_stats_table <- renderTable({
    d <- rv$tp_data; req(d, nrow(d)>0)
    tibble(
      Metric = c("T-Rank","Record","Adj. OE","Adj. DE","Barthag %","Adj. Tempo",
                 "WAB","Proj W","Proj L","SOS"),
      Value  = c(as.character(d$rank[1]), d$record[1],
                 sprintf("%.1f", d$adj_oe[1]), sprintf("%.1f", d$adj_de[1]),
                 sprintf("%.1f%%", d$barthag[1]*100), sprintf("%.1f", d$adj_tempo[1]),
                 sprintf("%.1f", d$wab[1]),
                 sprintf("%.1f", d$proj_w[1]), sprintf("%.1f", d$proj_l[1]),
                 sprintf("%.3f", d$sos[1]))
    )
  }, striped=TRUE, hover=TRUE)

  output$tp_game_log <- renderDT({
    d <- rv$tp_games; req(!is.null(d), nrow(d)>0)
    datatable(d, rownames=FALSE, filter="top",
              options=list(pageLength=20, dom="frtip", scrollX=TRUE),
              class="stripe hover compact")
  })

  # ── Players ───────────────────────────────────────────────────────────────
  observeEvent(input$load_pl, {
    withProgress(message="Fetching player data (~10 s)...", {
      rv$players <- tryCatch(
        player_stats(year=as.integer(input$pl_year)),
        error=function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  pl_filtered <- reactive({
    d <- rv$players; req(d)
    d |> filter(as.numeric(GP) >= input$pl_gp, as.numeric(usg) >= input$pl_usg)
  })

  selected_player <- reactiveVal(NULL)

  observeEvent(input$pl_table_rows_selected, {
    d <- pl_filtered()
    idx <- input$pl_table_rows_selected
    if (!is.null(idx) && length(idx)>0 && idx <= nrow(d)) {
      selected_player(d[idx, ])
    } else {
      selected_player(NULL)
    }
  })

  output$player_profile_panel <- renderUI({
    p <- selected_player()
    if (is.null(p)) return(NULL)
    logo_url <- get_logo_url(p$team[1])
    div(
      style="background:#f8f9fa; border-radius:10px; padding:16px; margin-bottom:18px; border-left:4px solid #1a3a5c;",
      div(style="display:flex; align-items:center; gap:14px; margin-bottom:12px;",
        if (!is.na(logo_url)) tags$img(src=logo_url, height="44px"),
        div(
          h4(p$player_name[1], style="margin:0; color:#1a3a5c; font-weight:700;"),
          p(paste0(p$team[1]," · ",p$conf[1]," · ",p$yr[1]," · Ht: ",p$ht[1]),
            style="margin:0; color:#6c757d; font-size:0.88em;")
        )
      ),
      layout_columns(col_widths=c(2,2,2,2,2,2),
        stat_card(p$GP[1],    "GP"),
        stat_card(paste0(round(as.numeric(p$ORtg[1]),0)), "ORtg",    "#2980b9"),
        stat_card(paste0(round(as.numeric(p$usg[1]),1),"%"), "Usage", "#e07b24"),
        stat_card(paste0(round(as.numeric(p$eFG[1]),1),"%"), "eFG%",  "#27ae60"),
        stat_card(round(as.numeric(p$gbpm[1]),1), "BPM 2.0", "#8e44ad"),
        stat_card(p$pts[1],   "PTS",     "#c0392b")
      ),
      layout_columns(col_widths=c(3,3,3,3),
        stat_card(paste0(round(as.numeric(p$ORB_pct[1]),1),"%"), "OReb%"),
        stat_card(paste0(round(as.numeric(p$AST_pct[1]),1),"%"), "Ast%"),
        stat_card(paste0(round(as.numeric(p$TO_pct[1]),1),"%"),  "TO%"),
        stat_card(paste0(round(as.numeric(p$stl_pct[1]),1),"%"), "Stl%")
      )
    )
  })

  output$pl_bpm_plot <- renderPlot({
    d <- pl_filtered()
    req(nrow(d)>0, "bpm" %in% names(d))
    top25 <- d |> mutate(bpm=as.numeric(bpm)) |> filter(!is.na(bpm)) |>
      arrange(desc(bpm)) |> head(25)
    ggplot(top25, aes(x=reorder(player_name, bpm), y=bpm, fill=bpm)) +
      geom_col(width=0.7) +
      geom_text(aes(label=round(bpm,1)), hjust=ifelse(top25$bpm>=0,-0.15,1.1), size=3) +
      scale_fill_gradient2(low="#c0392b", mid="#ecf0f1", high="#1a5276", midpoint=0) +
      coord_flip(clip="off") +
      labs(title="Top 25 Players by BPM", x=NULL, y="Box Plus/Minus") +
      theme_minimal(base_size=10) +
      theme(legend.position="none", plot.margin=margin(r=20))
  })

  output$pl_ortg_plot <- renderPlot({
    d <- pl_filtered()
    req(nrow(d)>0, "ORtg" %in% names(d))
    d2 <- d |> mutate(ORtg=as.numeric(ORtg), usg=as.numeric(usg), bpm=as.numeric(bpm)) |>
      filter(!is.na(ORtg), !is.na(usg))
    ggplot(d2, aes(x=usg, y=ORtg, color=bpm)) +
      geom_point(alpha=0.65, size=2) +
      scale_color_gradient2(low="#c0392b", mid="#bdc3c7", high="#1a5276", midpoint=0,
                            na.value="grey70", name="BPM") +
      geom_hline(yintercept=100, linetype="dashed", color="grey60") +
      labs(title="Usage vs. Offensive Rating", x="Usage Rate (%)", y="Offensive Rating") +
      theme_minimal(base_size=11)
  })

  output$pl_table <- renderDT({
    d <- pl_filtered(); req(nrow(d)>0)
    # Column definitions per barttorvik data dictionary:
    # Min_pct  = % of available team minutes (team_min / 5)
    # gbpm     = BPM 2.0 (revised box plus-minus, better for small samples)
    # bpm      = original BPM formula
    # obpm/dbpm = offensive/defensive BPM
    # rimmade  = rim shots made; midmade = mid-range two-pointers made
    # rim_pct / mid_pct = shooting % from rim / mid-range
    keep <- c("player_name","team","conf","GP","yr","ht","Min_pct",
              "ORtg","usg","eFG","TS_pct",
              "gbpm","bpm","obpm","dbpm",
              "ORB_pct","DRB_pct","AST_pct","TO_pct","stl_pct","blk_pct",
              "rim_pct","mid_pct","TP_pct",
              "pts","ast","treb","stl","blk","Recruit_TRank")
    keep <- intersect(keep, names(d))
    lbl_map <- c(
      player_name="Player", team="Team", conf="Conf", GP="GP", yr="Yr", ht="Ht",
      Min_pct="Min%", ORtg="ORtg", usg="Usg%", eFG="eFG%", TS_pct="TS%",
      gbpm="BPM 2.0", bpm="BPM", obpm="OBPM", dbpm="DBPM",
      ORB_pct="OReb%", DRB_pct="DReb%", AST_pct="Ast%", TO_pct="TO%",
      stl_pct="Stl%", blk_pct="Blk%",
      rim_pct="Rim%", mid_pct="Mid%", TP_pct="3P%",
      pts="PTS", ast="AST", treb="REB", stl="STL", blk="BLK",
      Recruit_TRank="Recruit Rk"
    )
    datatable(
      d[, keep], colnames=lbl_map[keep], rownames=FALSE, filter="top",
      selection="single",
      options=list(pageLength=25, dom="frtip", scrollX=TRUE,
                   columnDefs=list(list(
                     render=JS("function(data, type, row, meta) {
                       if (type === 'display' && data !== null && !isNaN(parseFloat(data))) {
                         return parseFloat(data).toFixed(1);
                       } return data;
                     }"),
                     targets=which(keep %in% c("gbpm","bpm","obpm","dbpm","ORtg","usg",
                                               "eFG","TS_pct","ORB_pct","DRB_pct",
                                               "AST_pct","TO_pct","stl_pct","blk_pct",
                                               "rim_pct","mid_pct","TP_pct","Min_pct")) - 1L
                   ))),
      class="stripe hover compact"
    ) |>
      formatStyle("gbpm",
        background=styleColorBar(c(-5,10),"#d5f4e6"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") |>
      formatStyle("ORtg",
        background=styleColorBar(c(80,130),"#cce5ff"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  # ── Team Resume ───────────────────────────────────────────────────────────
  observeEvent(input$load_res_teams, {
    withProgress(message="Loading team list...", {
      rv$res_teams <- tryCatch(
        torvik_team_ratings(year=as.integer(input$res_year), conf="All"),
        error=function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  output$res_team_ui <- renderUI({
    d <- rv$res_teams
    if (is.null(d)||nrow(d)==0)
      selectInput("res_team","Team (load teams first)",choices=character(0))
    else
      selectInput("res_team","Team",choices=sort(unique(d$team)),selectize=TRUE)
  })

  observeEvent(input$load_res, {
    req(input$res_team)
    withProgress(message=paste("Loading games for", input$res_team,"..."), {
      rv$resume <- tryCatch(
        get_games(team=input$res_team, season=as.integer(input$res_year),
                  quad=if(input$res_quad=="All")"All" else as.integer(input$res_quad)),
        error=function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  res_data <- reactive({ d <- rv$resume; req(d,nrow(d)>0); d })

  output$res_card_w <- renderUI({
    d <- res_data(); w <- sum(grepl("^W",d$Result),na.rm=TRUE)
    stat_card(w,"Wins","#27ae60")
  })
  output$res_card_l <- renderUI({
    d <- res_data(); l <- sum(grepl("^L",d$Result),na.rm=TRUE)
    stat_card(l,"Losses","#c0392b")
  })
  output$res_card_adjoe <- renderUI({
    d <- res_data()
    oe_col <- intersect(c("Adj. O","adj_oe","AdjOE"),names(d))[1]
    val <- if(!is.na(oe_col)) round(mean(as.numeric(d[[oe_col]]),na.rm=TRUE),1) else "—"
    stat_card(val,"Avg Adj. OE","#2980b9")
  })
  output$res_card_adjde <- renderUI({
    d <- res_data()
    de_col <- intersect(c("Adj. D","adj_de","AdjDE"),names(d))[1]
    val <- if(!is.na(de_col)) round(mean(as.numeric(d[[de_col]]),na.rm=TRUE),1) else "—"
    stat_card(val,"Avg Adj. DE","#8e44ad")
  })

  output$res_result_plot <- renderPlot({
    d <- res_data()
    result_col <- intersect(c("Result","result"),names(d))[1]
    opp_col    <- intersect(c("Opp","opponent","Opponent"),names(d))[1]
    oe_col     <- intersect(c("Adj. O","adj_oe","AdjOE"),names(d))[1]
    req(!is.na(result_col),!is.na(oe_col))
    d$wl  <- ifelse(grepl("^W",d[[result_col]]),"W","L")
    d$oe  <- as.numeric(d[[oe_col]])
    d$opp <- if(!is.na(opp_col)) d[[opp_col]] else seq_len(nrow(d))
    d <- d[!is.na(d$oe),]
    ggplot(d, aes(x=reorder(opp,oe), y=oe, fill=wl)) +
      geom_col(width=0.7) +
      scale_fill_manual(values=c("W"="#27ae60","L"="#c0392b"),name=NULL) +
      coord_flip() +
      labs(title=paste("Adj. OE by Game —",input$res_team),x=NULL,y="Adj. OE") +
      theme_minimal(base_size=10) +
      theme(axis.text.y=element_text(size=8))
  })

  output$res_table <- renderDT({
    datatable(res_data(), rownames=FALSE, filter="top",
              options=list(pageLength=30,dom="frtip",scrollX=TRUE),
              class="stripe hover compact")
  })

  # ── Super Schedule ────────────────────────────────────────────────────────
  observeEvent(input$load_ss, {
    withProgress(message="Fetching super schedule (~15 s)...", {
      rv$supersked <- tryCatch(
        get_super_sked(year=as.integer(input$ss_year)),
        error=function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  ss_data <- reactive({ d <- rv$supersked; req(d,nrow(d)>0); d })

  output$ss_ttq_plot <- renderPlot({
    d <- ss_data()
    req("ttq" %in% names(d), "matchup" %in% names(d))
    d$ttq_val <- as.numeric(d$ttq)
    d2 <- d[!is.na(d$ttq_val) & d$ttq_val > 0, ]
    top25 <- d2[order(-d2$ttq_val), ][seq_len(min(25, nrow(d2))), ]
    top25$label <- if (all(c("t1rk","t2rk") %in% names(top25))) {
      paste0(top25$matchup, "  (#", top25$t1rk, " vs #", top25$t2rk, ")")
    } else top25$matchup
    ggplot(top25, aes(x=reorder(label, ttq_val), y=ttq_val, fill=ttq_val)) +
      geom_col(width=0.7) +
      geom_text(aes(label=round(ttq_val,1)), hjust=-0.1, size=3) +
      scale_fill_gradient(low="#d4e6f1", high="#1a3a5c", guide="none") +
      coord_flip(clip="off") +
      labs(title="Top 25 Games by Torvik Thrill Quotient (TTQ)",
           subtitle="Higher = better quality matchup", x=NULL, y="TTQ") +
      theme_minimal(base_size=10) +
      theme(axis.text.y=element_text(size=8), plot.margin=margin(r=30))
  })

  output$ss_line_plot <- renderPlot({
    d <- ss_data()
    # t1wp is the direct win probability column per the data dictionary
    req("t1wp" %in% names(d))
    d$wp  <- suppressWarnings(as.numeric(d$t1wp))
    d$gv  <- if ("gamevalue" %in% names(d)) suppressWarnings(as.numeric(d$gamevalue)) else NA_real_
    d$ttq_num <- suppressWarnings(as.numeric(d$ttq))
    d2 <- d[!is.na(d$wp), ]
    x_col <- if (!all(is.na(d2$gv))) "gv" else "ttq_num"
    x_lbl <- if (x_col == "gv") "Game Value" else "TTQ"
    ggplot(d2, aes_string(x=x_col, y="wp", color="ttq_num")) +
      geom_point(alpha=0.5, size=1.8) +
      scale_color_gradient(low="#aed6f1", high="#1a3a5c", name="TTQ", na.value="grey70") +
      geom_hline(yintercept=50, linetype="dashed", color="grey50") +
      labs(title="Game Value vs. Team 1 Win Probability",
           subtitle="t1wp = Team 1 win probability (%) · higher gamevalue = more meaningful game",
           x=x_lbl, y="Team 1 Win Prob (%)") +
      theme_minimal(base_size=11)
  })

  output$ss_table <- renderDT({
    d <- ss_data()
    key_cols <- c("Date","matchup","ttq","team1","t1rk","t1oe","t1de","t1wp",
                  "team2","t2rk","t2oe","t2de","t2wp",
                  "t1pts","t2pts","result","confmatch","venue",
                  "gamevalue","mismatch","blowout","tempo","overtimes")
    show <- intersect(key_cols, names(d))
    col_labels <- c(
      Date="Date", matchup="Matchup", ttq="TTQ",
      team1="Team 1", t1rk="T1 Rk", t1oe="T1 AdjO", t1de="T1 AdjD", t1wp="T1 Win%",
      team2="Team 2", t2rk="T2 Rk", t2oe="T2 AdjO", t2de="T2 AdjD", t2wp="T2 Win%",
      t1pts="T1 Pts", t2pts="T2 Pts", result="Result", confmatch="Conf?",
      venue="Venue", gamevalue="Game Val", mismatch="Mismatch",
      blowout="Blowout", tempo="Tempo", overtimes="OT"
    )
    d2 <- d[, show]
    num_cols <- intersect(c("ttq","t1rk","t2rk","t1oe","t1de","t1wp",
                            "t2oe","t2de","t2wp","t1pts","t2pts",
                            "gamevalue","mismatch","blowout","tempo","overtimes"), show)
    for (col in num_cols) d2[[col]] <- suppressWarnings(as.numeric(d2[[col]]))
    ttq_idx <- which(show == "ttq") - 1L
    datatable(
      d2, colnames=col_labels[show], rownames=FALSE, filter="top",
      options=list(pageLength=30, dom="frtip", scrollX=TRUE,
                   order=list(list(ttq_idx, "desc"))),
      class="stripe hover compact"
    ) |>
      formatRound(intersect(c("ttq","t1oe","t1de","t2oe","t2de","gamevalue","tempo"), show), 1) |>
      formatStyle("t1wp",
        background=styleColorBar(c(0,100), "#cce5ff"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") |>
      formatStyle("t2wp",
        background=styleColorBar(c(0,100), "#fdebd0"),
        backgroundSize="100% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  # ── Time Machine ──────────────────────────────────────────────────────────
  observeEvent(input$load_tm, {
    date_str <- format(as.Date(input$tm_date),"%Y%m%d")
    withProgress(message=paste("Loading ratings for",format(as.Date(input$tm_date),"%b %d, %Y"),"..."), {
      rv$timemachine <- tryCatch(
        timeMachine_ratings(date=date_str),
        error=function(e) { showNotification(paste("Error:", e$message), type="error"); NULL }
      )
    })
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  tm_data <- reactive({
    d <- rv$timemachine; req(d,nrow(d)>0)
    conf_sel <- input$tm_conf
    if (!is.null(conf_sel) && conf_sel!="All") {
      conf_col <- intersect(c("conf","Conf"),names(d))[1]
      if (!is.na(conf_col)) d <- d[d[[conf_col]]==conf_sel,]
    }
    head(d, as.integer(input$tm_n))
  })

  output$tm_card_1 <- renderUI({
    d <- rv$timemachine; req(d)
    team_col <- intersect(c("team","Team"),names(d))[1]
    stat_card(if(!is.na(team_col)) d[[team_col]][1] else "—",
              paste("#1 on",format(as.Date(input$tm_date),"%b %d, %Y")))
  })
  output$tm_card_2 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("barthag","Barthag"),names(d))[1]
    val <- if(!is.na(col)) sprintf("%.1f%%", max(as.numeric(d[[col]]),na.rm=TRUE)*100) else "—"
    stat_card(val,"Highest Barthag","#27ae60")
  })
  output$tm_card_3 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("adjoe","adj_oe","AdjOE"),names(d))[1]
    val <- if(!is.na(col)) sprintf("%.1f",max(as.numeric(d[[col]]),na.rm=TRUE)) else "—"
    stat_card(val,"Best Adj. OE","#2980b9")
  })
  output$tm_card_4 <- renderUI({
    d <- rv$timemachine; req(d)
    col <- intersect(c("adjde","adj_de","AdjDE"),names(d))[1]
    val <- if(!is.na(col)) sprintf("%.1f",min(as.numeric(d[[col]]),na.rm=TRUE)) else "—"
    stat_card(val,"Best Adj. DE","#8e44ad")
  })

  output$tm_barthag_plot <- renderPlot({
    d <- tm_data()
    team_col    <- intersect(c("team","Team"),names(d))[1]
    barthag_col <- intersect(c("barthag","Barthag"),names(d))[1]
    req(!is.na(team_col),!is.na(barthag_col))
    d$team_lbl <- d[[team_col]]
    d$bart_val <- as.numeric(d[[barthag_col]])
    d <- d[!is.na(d$bart_val),]
    d$bart_pct <- d$bart_val * 100
    ggplot(d, aes(x=reorder(team_lbl,bart_pct),y=bart_pct,fill=bart_pct)) +
      geom_col(width=0.7) +
      geom_text(aes(label=paste0(round(bart_pct,1),"%")),hjust=-0.1,size=3) +
      scale_fill_gradient(low="#cce5ff",high="#1a3a5c",guide="none") +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      coord_flip(clip="off") +
      labs(title=paste("Barthag % —",format(as.Date(input$tm_date),"%b %d, %Y")),x=NULL,y="Barthag %") +
      theme_minimal(base_size=10) +
      theme(axis.text.y=element_text(size=8), plot.margin=margin(r=30))
  })

  output$tm_table <- renderDT({
    datatable(tm_data(), rownames=FALSE, filter="top",
              options=list(pageLength=25,dom="frtip",scrollX=TRUE),
              class="stripe hover compact")
  })
}

shinyApp(ui, server)
