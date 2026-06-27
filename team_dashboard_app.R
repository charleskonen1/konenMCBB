## NCAA MBB Team Dashboard (ESPN data)
##
## Usage (from project root after building data):
##   source("team_dashboard_app.R")
##
## Expects data/espn_db populated for 2024-25 (e.g. via scripts/espn_build_2024_11_11_20.R).

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' must be installed to run the dashboard.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' must be installed to run the dashboard.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' must be installed to run the dashboard.")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Package 'tidyr' must be installed to run the dashboard.")
}

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Optional: bslib for theme, DT for styled tables
has_bslib <- requireNamespace("bslib", quietly = TRUE)
has_dt    <- requireNamespace("DT", quietly = TRUE)
if (has_bslib) library(bslib)
if (has_dt) library(DT)

# Use whatever base path the session is configured with; fallback to project-level
base_path <- getOption("konenMCBB.espn_db_path", "data/espn_db")

## Load team- and player-game data
tg_raw <- konenMCBB:::espn_team_games("2024-25", base_path = base_path)
pg_raw <- konenMCBB:::espn_player_games("2024-25", base_path = base_path)

team_games <- tg_raw %>%
  filter(
    game_date >= as.Date("2024-11-11"),
    game_date <= as.Date("2024-11-20")
  )

if (nrow(team_games) == 0L) {
  stop("No team-game rows found for 2024-11-11 to 2024-11-20. ",
       "Run scripts/espn_build_2024_11_11_20.R first.")
}

## Build team lookup (id -> display name, abbreviation) from game.rds when available
game_dirs <- konenMCBB:::.espn_list_game_dirs(base_path, "2024-25")
team_lookup <- tibble(
  team_id = character(),
  team_name = character(),
  team_short = character(),
  team_abbrev = character()
)
for (gdir in game_dirs) {
  gpath <- file.path(gdir, "game.rds")
  if (!file.exists(gpath)) next
  gobj <- tryCatch(readRDS(gpath), error = function(e) NULL)
  if (is.null(gobj)) next
  comps <- gobj$header$competitions
  if (is.null(comps) || length(comps) == 0L) next
  competitors <- comps[[1]]$competitors
  if (is.null(competitors)) next
  for (c in competitors) {
    tid <- c$team$id
    if (is.null(tid)) next
    tid <- as.character(tid)
    team_lookup <- bind_rows(
      team_lookup,
      tibble(
        team_id = tid,
        team_name = as.character(
          if (!is.null(c$team$displayName)) c$team$displayName
          else if (!is.null(c$team$shortDisplayName)) c$team$shortDisplayName
          else tid
        ),
        team_short = as.character(if (!is.null(c$team$shortDisplayName)) c$team$shortDisplayName else NA_character_),
        team_abbrev = as.character(if (!is.null(c$team$abbreviation)) c$team$abbreviation else NA_character_)
      )
    )
  }
}
if (nrow(team_lookup) > 0L) {
  team_lookup <- team_lookup %>%
    group_by(team_id) %>%
    summarise(
      team_name = first(na.omit(team_name)),
      team_short = first(na.omit(team_short)),
      team_abbrev = first(na.omit(team_abbrev)),
      .groups = "drop"
    )
}

# Fallback when no game.rds: use team_id as name
all_team_ids <- sort(unique(team_games$team_id))
if (nrow(team_lookup) == 0L) {
  team_lookup <- tibble(
    team_id = all_team_ids,
    team_name = all_team_ids,
    team_short = all_team_ids,
    team_abbrev = NA_character_
  )
} else {
  missing <- setdiff(all_team_ids, team_lookup$team_id)
  if (length(missing) > 0L) {
    team_lookup <- bind_rows(
      team_lookup,
      tibble(team_id = missing, team_name = missing, team_short = missing, team_abbrev = NA_character_)
    )
  }
}

team_games <- team_games %>%
  left_join(team_lookup %>% select(team_id, team_name, team_abbrev), by = "team_id") %>%
  left_join(
    team_lookup %>% select(opponent_team_id = team_id, opponent_name = team_name, opponent_abbrev = team_abbrev),
    by = "opponent_team_id"
  )

player_games <- pg_raw

# For selector: show team name, value is team_id
team_choices <- setNames(team_games$team_id, team_games$team_name)
team_choices <- team_choices[!duplicated(team_choices)]
team_choices <- team_choices[order(names(team_choices))]

# Palette for charts
pal_factors <- c("#2ecc71", "#e74c3c", "#3498db", "#9b59b6")
pal_wl <- c("W" = "#27ae60", "L" = "#c0392b")

ui <- fluidPage(
  theme = if (has_bslib) bslib::bs_theme(bootswatch = "flatly", primary = "#2980b9") else NULL,
  titlePanel(
    div(
      span("NCAA MBB Team Dashboard", style = "font-weight: 700;"),
      span(" | ESPN · Nov 11–20, 2024", style = "color: #7f8c8d; font-size: 0.9em;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "team",
        "Team",
        choices = team_choices,
        selectize = TRUE
      ),
      dateRangeInput(
        "date_range",
        "Date range",
        start = as.Date("2024-11-11"),
        end   = as.Date("2024-11-20"),
        min   = min(team_games$game_date),
        max   = max(team_games$game_date)
      ),
      hr(),
      p("Summary and game-level stats from ESPN game data.", style = "font-size: 0.85em; color: #7f8c8d;")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(3, uiOutput("value_wins")),
        column(3, uiOutput("value_losses")),
        column(3, uiOutput("value_margin")),
        column(3, uiOutput("value_eff"))
      ),
      hr(),
      h4("Season (window) summary", style = "margin-top: 1em;"),
      tableOutput("team_summary"),
      h4("Shooting & rebounding", style = "margin-top: 1.5em;"),
      tableOutput("team_shooting"),
      h4("Game-by-game", style = "margin-top: 1.5em;"),
      if (has_dt) DT::dataTableOutput("game_table_dt") else tableOutput("game_table_base"),
      h4("Offensive efficiency over time", style = "margin-top: 1.5em;"),
      plotOutput("eff_trend", height = "280px"),
      h4("Four factors (window average)", style = "margin-top: 1.5em;"),
      plotOutput("four_factors", height = "300px"),
      h4("Shot distance profile", style = "margin-top: 1.5em;"),
      plotOutput("shot_profile", height = "300px"),
      h4("Player summary (window)", style = "margin-top: 1.5em;"),
      tableOutput("player_table")
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    req(input$team, input$date_range)
    team_games %>%
      filter(
        team_id == input$team,
        game_date >= input$date_range[1],
        game_date <= input$date_range[2]
      )
  })

  selected_name <- reactive({
    req(input$team)
    sub("/.*", "", names(team_choices)[match(input$team, team_choices)])
  })

  output$value_wins <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    w <- sum(df$won, na.rm = TRUE)
    if (has_bslib) {
      value_box(
        title = "Wins",
        value = w,
        theme = "success",
        showcase = NULL
      )
    } else {
      div(
        style = "background: #d5f4e6; padding: 12px; border-radius: 6px; text-align: center;",
        p(style = "margin: 0; font-size: 0.9em; color: #1e8449;", "Wins"),
        p(style = "margin: 0; font-size: 1.8em; font-weight: bold; color: #1e8449;", w)
      )
    }
  })

  output$value_losses <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    g <- nrow(df)
    w <- sum(df$won, na.rm = TRUE)
    L <- g - w
    if (has_bslib) {
      value_box(
        title = "Losses",
        value = L,
        theme = "danger",
        showcase = NULL
      )
    } else {
      div(
        style = "background: #fadbd8; padding: 12px; border-radius: 6px; text-align: center;",
        p(style = "margin: 0; font-size: 0.9em; color: #922b21;", "Losses"),
        p(style = "margin: 0; font-size: 1.8em; font-weight: bold; color: #922b21;", L)
      )
    }
  })

  output$value_margin <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    m <- round(mean(df$margin, na.rm = TRUE), 1)
    clr <- if (m >= 0) "#1e8449" else "#922b21"
    if (has_bslib) {
      value_box(
        title = "Avg margin",
        value = sprintf("%+.1f", m),
        theme = if (m >= 0) "success" else "danger",
        showcase = NULL
      )
    } else {
      div(
        style = paste0("background: #e8f6f3; padding: 12px; border-radius: 6px; text-align: center; border-left: 4px solid ", clr, ";"),
        p(style = "margin: 0; font-size: 0.9em; color: #1a5276;", "Avg margin"),
        p(style = paste0("margin: 0; font-size: 1.8em; font-weight: bold; color: ", clr, ";"), sprintf("%+.1f", m))
      )
    }
  })

  output$value_eff <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    e <- round(mean(df$eff, na.rm = TRUE), 1)
    if (has_bslib) {
      value_box(
        title = "Off eff (pts/100 poss)",
        value = e,
        theme = "primary",
        showcase = NULL
      )
    } else {
      div(
        style = "background: #ebf5fb; padding: 12px; border-radius: 6px; text-align: center;",
        p(style = "margin: 0; font-size: 0.9em; color: #1a5276;", "Off eff"),
        p(style = "margin: 0; font-size: 1.8em; font-weight: bold; color: #2980b9;", e)
      )
    }
  })

  output$team_summary <- renderTable({
    df <- filtered()
    req(nrow(df) > 0)
    df %>%
      summarise(
        Games = n(),
        Wins = sum(won, na.rm = TRUE),
        Losses = Games - Wins,
        `Avg margin` = round(mean(margin, na.rm = TRUE), 1),
        `Off eff` = round(mean(eff, na.rm = TRUE), 1),
        `eFG%` = round(100 * mean(effective_field_goal_pct, na.rm = TRUE), 1),
        `TS%` = round(100 * mean(true_shooting_pct, na.rm = TRUE), 1),
        TOV = round(mean(turnovers, na.rm = TRUE), 1),
        FTR = round(mean(free_throw_rate, na.rm = TRUE), 2),
        `3PAr` = round(mean(three_point_attempt_rate, na.rm = TRUE), 2)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Metric", values_to = "Value")
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$team_shooting <- renderTable({
    df <- filtered()
    req(nrow(df) > 0)
    orb_col <- if ("offensiveRebounds" %in% names(df)) "offensiveRebounds" else "offensive_rebounds"
    df %>%
      summarise(
        `FG (avg)` = paste0(round(mean(field_goals_made, na.rm = TRUE), 1), "/", round(mean(field_goals_attempted, na.rm = TRUE), 1)),
        `3P (avg)` = paste0(round(mean(three_point_field_goals_made, na.rm = TRUE), 1), "/", round(mean(three_point_field_goals_attempted, na.rm = TRUE), 1)),
        `FT (avg)` = paste0(round(mean(free_throws_made, na.rm = TRUE), 1), "/", round(mean(free_throws_attempted, na.rm = TRUE), 1)),
        `ORB (avg)` = as.character(if (orb_col %in% names(df)) round(mean(df[[orb_col]], na.rm = TRUE), 1) else NA_real_),
        `TRB (avg)` = as.character(if ("rebounds" %in% names(df)) round(mean(rebounds, na.rm = TRUE), 1) else NA_real_),
        `AST (avg)` = as.character(if ("assists" %in% names(df)) round(mean(assists, na.rm = TRUE), 1) else NA_real_)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Stat", values_to = "Avg")
  }, striped = TRUE, hover = TRUE, width = "100%")

  game_table_df <- reactive({
    df <- filtered() %>%
      arrange(game_date) %>%
      mutate(
        Date = format(game_date, "%Y-%m-%d"),
        Opponent = coalesce(opponent_name, opponent_team_id),
        Result = if_else(won, "W", "L"),
        Pts = as.integer(points),
        Opp = as.integer(points_allowed),
        Marg = as.integer(margin),
        eFG = round(100 * effective_field_goal_pct, 1),
        TS = round(100 * true_shooting_pct, 1),
        Poss = round(estimated_possessions, 0),
        Eff = round(eff, 1)
      )
    df %>%
      select(Date, Opponent, Result, Pts, Opp, Marg, eFG, TS, Poss, Eff)
  })

  if (has_dt) {
    output$game_table_dt <- DT::renderDataTable({
      df <- game_table_df()
      req(nrow(df) > 0)
      DT::datatable(
        df,
        options = list(pageLength = 15, dom = "tip"),
        rownames = FALSE
      ) %>%
        DT::formatStyle("Result", target = "cell", backgroundColor = DT::styleEqual(c("W", "L"), c("#d5f4e6", "#fadbd8")))
    })
  }
  output$game_table_base <- renderTable({
    df <- game_table_df()
    req(nrow(df) > 0)
    df
  }, striped = TRUE, hover = TRUE)

  output$eff_trend <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    df <- df %>% mutate(Result = if_else(won, "W", "L"))
    p <- ggplot(df, aes(x = game_date, y = eff, color = Result)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      scale_color_manual(values = pal_wl) +
      labs(x = "Date", y = "Offensive efficiency (pts per 100 poss)", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
    print(p)
  })

  output$four_factors <- renderPlot({
    df <- filtered()
    req(nrow(df) > 0)
    orb_vals <- if ("offensiveRebounds" %in% names(df)) df$offensiveRebounds else if ("offensive_rebounds" %in% names(df)) df$offensive_rebounds else NA_real_
    long <- df %>%
      summarise(
        eFG = mean(effective_field_goal_pct, na.rm = TRUE),
        TOV = mean(turnovers, na.rm = TRUE),
        ORB = mean(orb_vals, na.rm = TRUE),
        FTR = mean(free_throw_rate, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "factor", values_to = "value")
    long$factor <- factor(long$factor, levels = c("eFG", "TOV", "ORB", "FTR"))
    ggplot(long, aes(x = factor, y = value, fill = factor)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = setNames(pal_factors, c("eFG", "TOV", "ORB", "FTR")), guide = "none") +
      labs(x = NULL, y = "Value", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
  })

  # Shot distance profile for selected team over selected window
  shot_profile_df <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    dirs <- file.path(
      base_path,
      df$season,
      format(df$game_date, "%Y-%m-%d"),
      df$game_id
    )
    dirs <- unique(dirs)
    plays_list <- lapply(dirs, function(gd) {
      p_path <- file.path(gd, "plays.rds")
      if (!file.exists(p_path)) return(NULL)
      p <- tryCatch(readRDS(p_path), error = function(e) NULL)
      if (is.null(p) || nrow(p) == 0L) return(NULL)
      # Only keep columns needed for shot-distance analytics to avoid
      # type mismatches across games (e.g. points sometimes numeric vs character).
      keep <- intersect(
        c("team_id", "shooting_play", "shot_distance", "shotDistance", "x_coordinate", "y_coordinate"),
        names(p)
      )
      p <- p[, keep, drop = FALSE]
      # Normalize likely types
      if ("team_id" %in% names(p)) p$team_id <- as.character(p$team_id)
      if ("shooting_play" %in% names(p)) p$shooting_play <- as.logical(p$shooting_play)
      if ("shot_distance" %in% names(p)) p$shot_distance <- suppressWarnings(as.numeric(p$shot_distance))
      if ("shotDistance" %in% names(p)) p$shotDistance <- suppressWarnings(as.numeric(p$shotDistance))
      if ("x_coordinate" %in% names(p)) p$x_coordinate <- suppressWarnings(as.numeric(p$x_coordinate))
      if ("y_coordinate" %in% names(p)) p$y_coordinate <- suppressWarnings(as.numeric(p$y_coordinate))
      p
    })
    plays <- dplyr::bind_rows(plays_list)
    if (is.null(plays) || nrow(plays) == 0L) {
      return(tibble::tibble(zone = character(), pct = numeric()))
    }
    dist <- if ("shot_distance" %in% names(plays)) {
      suppressWarnings(as.numeric(plays$shot_distance))
    } else if ("shotDistance" %in% names(plays)) {
      suppressWarnings(as.numeric(plays$shotDistance))
    } else if (all(c("x_coordinate", "y_coordinate") %in% names(plays))) {
      x <- suppressWarnings(as.numeric(plays$x_coordinate))
      y <- suppressWarnings(as.numeric(plays$y_coordinate))
      ifelse(!is.na(x) & !is.na(y), sqrt((x - 25)^2 + y^2), NA_real_)
    } else {
      rep(NA_real_, nrow(plays))
    }
    is_shot <- if ("shooting_play" %in% names(plays)) {
      as.logical(plays$shooting_play)
    } else {
      rep(FALSE, nrow(plays))
    }
    plays2 <- tibble::tibble(
      team_id = as.character(plays$team_id),
      is_shot = is_shot,
      dist = dist
    ) %>%
      dplyr::filter(team_id == input$team, is_shot %in% TRUE, !is.na(dist))
    if (nrow(plays2) == 0L) {
      return(tibble::tibble(zone = character(), pct = numeric()))
    }
    plays2 <- plays2 %>%
      dplyr::mutate(
        zone = dplyr::case_when(
          dist <= 4 ~ "At rim (<=4 ft)",
          dist <= 14 ~ "Paint/short mid (4-14 ft)",
          dist <= 22 ~ "Long mid (14-22 ft)",
          TRUE ~ "Three+ (>=22 ft)"
        )
      )
    plays2 %>%
      dplyr::count(zone, name = "attempts") %>%
      dplyr::mutate(pct = attempts / sum(attempts)) %>%
      dplyr::arrange(match(zone, c("At rim (<=4 ft)", "Paint/short mid (4-14 ft)", "Long mid (14-22 ft)", "Three+ (>=22 ft)")))
  })

  output$shot_profile <- renderPlot({
    df <- shot_profile_df()
    req(nrow(df) > 0)
    ggplot(df, aes(x = zone, y = pct, fill = zone)) +
      geom_col(width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Attempt share", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")
  })

  # Player-level summary over selected window
  player_df <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    pg <- player_games %>%
      dplyr::filter(game_id %in% df$game_id, team_id == input$team)
    if (nrow(pg) == 0L) {
      return(tibble::tibble())
    }
    pg %>%
      dplyr::group_by(player_id, display_name) %>%
      dplyr::summarise(
        GP = dplyr::n(),
        Min = round(sum(minutes_numeric, na.rm = TRUE), 1),
        PTS = round(sum(points, na.rm = TRUE), 1),
        Eff = round(mean(points_per_estimated_possession, na.rm = TRUE) * 100, 1),
        PlusMinus = round(sum(plus_minus, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Min))
  })

  output$player_table <- renderTable({
    df <- player_df()
    req(nrow(df) > 0)
    df
  }, striped = TRUE, hover = TRUE, width = "100%")
}

shinyApp(ui, server)
