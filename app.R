# =============================================================================
# STAN - STock ANalyser
# app.R
# =============================================================================

# =============================================================================
# 0. INSTALLATION / CHARGEMENT DES PACKAGES
# -----------------------------------------------------------------------------
# Cette partie permet de lancer l'application même si certains packages
# ne sont pas encore installés sur l'ordinateur.
# =============================================================================

required_packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "dplyr",
  "htmltools",
  "scales",
  "readr",
  "rmarkdown"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# 1. CHARGEMENT DE global.R SI NECESSAIRE
# -----------------------------------------------------------------------------
# Cela évite d'avoir à faire source("global.R") à la main.
# =============================================================================

if (!exists("ticker_ref")) {
  source("global.R")
}

# =============================================================================
# 2. UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  uiOutput("main_ui")
)

# =============================================================================
# 3. SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # 3.1 OUTILS D'AFFICHAGE
  # ---------------------------------------------------------------------------
  # Ces fonctions servent à mieux faire rentrer les valeurs dans les valueBox.
  # Si un chiffre est long, il sera affiché avec une taille un peu réduite.
  # ===========================================================================
  fit_valuebox_text <- function(x, large = "28px", small = "20px") {
    txt <- as.character(x)
    chosen_size <- if (nchar(txt) > 12) small else large
    HTML(
      paste0(
        "<div style='white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
        " font-size:", chosen_size, "; line-height:1.1;'>",
        txt,
        "</div>"
      )
    )
  }
  
  # ===========================================================================
  # 3.2 ETAT DE NAVIGATION
  # ===========================================================================
  current_view <- reactiveVal("landing")
  
  observeEvent(input$go_dashboard, {
    current_view("dashboard")
  })
  
  observeEvent(input$back_home, {
    current_view("landing")
  })
  
  # ===========================================================================
  # 3.3 LISTE DES TICKERS DISPONIBLES
  # ---------------------------------------------------------------------------
  # On fusionne les tickers du référentiel et ceux déjà présents localement.
  # ===========================================================================
  ticker_choices <- reactive({
    base_choices <- ticker_ref$ticker
    local_choices <- available_local_tickers()
    sort(unique(c(base_choices, local_choices)))
  })
  
  # ===========================================================================
  # 3.4 UI PRINCIPALE
  # ===========================================================================
  output$main_ui <- renderUI({
    if (current_view() == "landing") {
      
      # -----------------------------------------------------------------------
      # PAGE D'ACCUEIL
      # -----------------------------------------------------------------------
      div(
        class = "landing-page",
        div(
          class = "landing-overlay",
          div(
            class = "landing-box",
            
            div(
              style = "display:flex; align-items:center; justify-content:center; gap:16px; margin-bottom:10px;",
              icon("chart-line", class = "landing-main-icon"),
              h1("STAN", class = "landing-title", style = "margin:0;")
            ),
            
            h3("STock ANalyser", class = "landing-subtitle"),
            
            p(
              "Application Shiny d'analyse boursière : indicateurs financiers, performances, régression log-prix, visualisation interactive et gestion des données.",
              class = "landing-text"
            ),
            
            div(
              class = "landing-features",
              div(class = "feature-card", strong("Indicateurs"), br(), "Prix, volatilité, CAGR"),
              div(class = "feature-card", strong("Performance"), br(), "1M, 6M, 1A, 3A, 5A"),
              div(class = "feature-card", strong("Graphique"), br(), "Plotly + bandes sigma"),
              div(class = "feature-card", strong("Qualité"), br(), "Tests et contrôle des données")
            ),
            
            actionButton("go_dashboard", "Commencer votre analyse", class = "start-btn")
          )
        )
      )
      
    } else {
      
      # -----------------------------------------------------------------------
      # DASHBOARD
      # -----------------------------------------------------------------------
      dashboardPage(
        skin = "black",
        
        dashboardHeader(
          title = tags$span(
            style = "display:flex; align-items:center; gap:10px; font-weight:700;",
            icon("chart-line"),
            span("STAN")
          ),
          tags$li(
            class = "dropdown",
            a(
              href = "#",
              onclick = "return false;",
              actionLink("back_home", label = NULL, icon = icon("house")),
              style = "padding: 15px 12px; display: block; color: white;"
            )
          )
        ),
        
        dashboardSidebar(
          width = 270,
          sidebarMenu(
            id = "tabs",
            menuItem("Réglages", tabName = "settings", icon = icon("sliders")),
            menuItem("Vue générale", tabName = "overview", icon = icon("chart-line")),
            menuItem("Indicateurs", tabName = "indicators", icon = icon("gauge-high")),
            menuItem("Performance", tabName = "performance", icon = icon("percent")),
            menuItem("Graphique", tabName = "graph", icon = icon("chart-area")),
            menuItem("Gestion des données", tabName = "data_mgmt", icon = icon("database")),
            menuItem("Qualité des données", tabName = "quality", icon = icon("shield-halved"))
          )
        ),
        
        dashboardBody(
          tabItems(
            
            # ===============================================================
            # ONGLET 1 - VUE GENERALE
            # ===============================================================
            tabItem(
              tabName = "overview",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("circle-info"), " Résumé de l'analyse"),
                  uiOutput("overview_text")
                )
              ),
              fluidRow(
                valueBoxOutput("overview_price", width = 3),
                valueBoxOutput("overview_perf", width = 3),
                valueBoxOutput("overview_sigma", width = 3),
                valueBoxOutput("overview_rows", width = 3)
              )
            ),
            
            # ===============================================================
            # ONGLET 2 - REGLAGES
            # ===============================================================
            tabItem(
              tabName = "settings",
              fluidRow(
                box(
                  width = 6, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("sliders"), " Paramètres"),
                  selectInput(
                    "stock_ticker",
                    "Choix du ticker",
                    choices = ticker_choices(),
                    selected = ticker_choices()[1]
                  ),
                  dateInput(
                    "start_date",
                    "Date de début",
                    value = Sys.Date() - 3650,
                    format = "dd/mm/yyyy"
                  ),
                  dateInput(
                    "end_date",
                    "Date de fin",
                    value = Sys.Date(),
                    format = "dd/mm/yyyy"
                  )
                ),
                
                box(
                  width = 6, status = "warning", solidHeader = TRUE,
                  title = tagList(icon("circle-info"), " Aide rapide"),
                  tags$p(tags$b("Ticker : "), "action sélectionnée pour l'analyse."),
                  tags$p(tags$b("Date de début / fin : "), "période utilisée pour les indicateurs, les performances et le graphique."),
                  tags$p(tags$em("Conseil : une période trop courte peut empêcher la régression d'être calculée correctement."))
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tagList(icon("table"), " Informations sur la série"),
                  DTOutput("settings_info_table")
                )
              )
            ),
            
            # ===============================================================
            # ONGLET 3 - INDICATEURS
            # ===============================================================
            tabItem(
              tabName = "indicators",
              fluidRow(
                valueBoxOutput("last_price_box", width = 3),
                valueBoxOutput("last_update_box", width = 3),
                valueBoxOutput("volatility_box", width = 3),
                valueBoxOutput("cagr_box", width = 3)
              ),
              
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("circle-question"), " Comprendre les indicateurs"),
                  div(
                    class = "stan-card-blue",
                    strong("Qu'est-ce que le CAGR ?"), br(),
                    "Le CAGR correspond au taux de croissance annuel composé.", br(), br(),
                    HTML("Formule : <b>(Prix final / Prix initial)<sup>1/n</sup> - 1</b>"),
                    br(),
                    em("où n est le nombre d'années de la période étudiée.")
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tagList(icon("info-circle"), " Détail des définitions"),
                  tags$ul(
                    tags$li(tags$b("Dernier prix : "), "clôture la plus récente disponible."),
                    tags$li(tags$b("Dernière mise à jour : "), "date du dernier prix affiché."),
                    tags$li(tags$b("Volatilité : "), "écart-type des rendements log journaliers. Plus elle est élevée, plus le titre est risqué."),
                    tags$li(tags$b("CAGR : "), "croissance annuelle moyenne composée sur la période sélectionnée.")
                  )
                )
              )
            ),
            
            # ===============================================================
            # ONGLET 4 - PERFORMANCE
            # ===============================================================
            tabItem(
              tabName = "performance",
              
              fluidRow(
                box(
                  title = span(icon("percent"), " Performance par période"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("perf_1m", width = 2),
                    valueBoxOutput("perf_6m", width = 2),
                    valueBoxOutput("perf_1a", width = 2),
                    valueBoxOutput("perf_3a", width = 3),
                    valueBoxOutput("perf_5a", width = 3)
                  ),
                  helpText("Performance calculée à partir du prix Adjusted Close, ou Close si Adjusted est indisponible.")
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "warning", solidHeader = TRUE,
                  title = tagList(icon("lightbulb"), " Lecture rapide"),
                  fluidRow(
                    column(
                      4,
                      div(
                        class = "stan-card-blue",
                        strong("Beta - La pente"), br(),
                        "Croissance journalière selon la tendance long terme.", br(),
                        em("x 365 = croissance annuelle estimée.")
                      )
                    ),
                    column(
                      4,
                      div(
                        class = "stan-card-gold",
                        strong("Sigma - La dispersion"), br(),
                        "Écart-type des résidus de la régression.", br(),
                        em("Mesure l'écart du prix par rapport à sa tendance.")
                      )
                    ),
                    column(
                      4,
                      div(
                        class = "stan-card-purple",
                        strong("Position (sigma)"), br(),
                        "+1 sigma = prix au-dessus de la tendance.", br(),
                        em("Zone normale : entre -1 et +1 sigma.")
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 8, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("calculator"), " Régression log-linéaire"),
                  DTOutput("regression_metrics_table"),
                  br(),
                  tags$p(tags$em("Modèle : log(Prix) = alpha + beta * t + epsilon"))
                ),
                
                box(
                  width = 4, status = "success", solidHeader = TRUE,
                  title = tagList(icon("book-open"), " Lecture"),
                  tags$p(HTML("&rarr; <b>Beta > 0</b> &rarr; tendance haussière.")),
                  tags$hr(),
                  tags$p(HTML("&rarr; <b>Sigma faible</b> &rarr; prix plus régulier.")),
                  tags$hr(),
                  tags$p(HTML("&rarr; <b>Position > +2</b> &rarr; prix très au-dessus de la tendance.")),
                  tags$hr(),
                  tags$p(HTML("&rarr; <b>Position < -2</b> &rarr; prix très en dessous de la tendance.")),
                  tags$hr(),
                  tags$p(tags$em("Indication pédagogique, pas un conseil d'investissement."))
                )
              )
            ),
            
            # ===============================================================
            # ONGLET 5 - GRAPHIQUE
            # ===============================================================
            tabItem(
              tabName = "graph",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("chart-area"), " Prix, tendance et bandes sigma"),
                  plotlyOutput("price_plot", height = "520px"),
                  tags$hr(),
                  fluidRow(
                    column(
                      4,
                      div(
                        class = "stan-card-red",
                        strong("Ligne rouge pointillée"), br(),
                        "Droite de régression : tendance long terme."
                      )
                    ),
                    column(
                      4,
                      div(
                        class = "stan-card-blue",
                        strong("Zone bleue foncée (±1σ)"), br(),
                        "Zone normale : le prix y passe le plus souvent."
                      )
                    ),
                    column(
                      4,
                      div(
                        class = "stan-card-green",
                        strong("Zone bleue claire (±2σ)"), br(),
                        "Zone plus extrême : écart inhabituel autour de la tendance."
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tagList(icon("circle-info"), " Comment lire le graphique ?"),
                  tags$ul(
                    tags$li(tags$b("Prix observé : "), "cours historique réellement observé."),
                    tags$li(tags$b("Tendance théorique : "), "droite estimée par la régression sur le log-prix."),
                    tags$li(tags$b("Bande ±1 sigma : "), "zone habituelle autour de la tendance."),
                    tags$li(tags$b("Bande ±2 sigma : "), "zone plus rare, correspondant à un écart plus important.")
                  )
                )
              )
            ),
            
            # ===============================================================
            # ONGLET 6 - GESTION DES DONNEES
            # ===============================================================
            tabItem(
              tabName = "data_mgmt",
              fluidRow(
                box(
                  width = 4, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("arrows-rotate"), " Mettre à jour un ticker"),
                  
                  # ------------------------------------------------------------
                  # La liste déroulante contient :
                  # - "Tous les tickers"
                  # - puis tous les tickers individuels
                  # ------------------------------------------------------------
                  selectInput(
                    "update_ticker",
                    "Ticker à mettre à jour",
                    choices = c("Tous les tickers" = "ALL_TICKERS_UPDATE", ticker_choices()),
                    selected = ticker_choices()[1]
                  ),
                  
                  actionButton(
                    "update_btn",
                    "Mettre à jour depuis Yahoo",
                    icon = icon("download"),
                    class = "action-btn"
                  ),
                  br(), br(),
                  textOutput("update_status"),
                  br(),
                  tags$small("Un ticker précis = mise à jour de ce ticker. Tous les tickers = mise à jour seulement des tickers déjà présents localement dans data/.")
                ),
                
                box(
                  width = 4, status = "success", solidHeader = TRUE,
                  title = tagList(icon("plus"), " Ajouter un ticker Yahoo"),
                  textInput("new_ticker", "Nouveau ticker", placeholder = "Exemple : ADBE ou AC.PA"),
                  actionButton("add_ticker_btn", "Télécharger et ajouter", icon = icon("plus-circle"), class = "action-btn"),
                  br(), br(),
                  textOutput("add_ticker_status"),
                  br(),
                  tags$small("Ajoute un nouveau ticker à la base locale à partir de Yahoo Finance.")
                ),
                
                box(
                  width = 4, status = "warning", solidHeader = TRUE,
                  title = tagList(icon("file-import"), " Importer un CSV"),
                  textInput("import_ticker_name", "Nom du ticker importé", value = "IMPORT_USER"),
                  fileInput("csv_file", "Choisir un fichier CSV", accept = c(".csv")),
                  actionButton("import_btn", "Importer le CSV", icon = icon("upload"), class = "action-btn"),
                  br(), br(),
                  textOutput("import_status"),
                  br(),
                  tags$small("Le fichier doit contenir au minimum une date et un prix exploitable.")
                )
              ),
              
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = tagList(icon("database"), " Résumé de la base locale"),
                  DTOutput("data_summary_table")
                )
              )
            ),
            
            # ===============================================================
            # ONGLET 7 - QUALITE DES DONNEES
            # ===============================================================
            tabItem(
              tabName = "quality",
              fluidRow(
                box(
                  width = 6, status = "primary", solidHeader = TRUE,
                  title = tagList(icon("check-double"), " Tests qualité"),
                  DTOutput("quality_checks_table")
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = tagList(icon("clipboard-list"), " Résumé qualité"),
                  DTOutput("quality_summary_table")
                )
              )
            )
          )
        )
      )
    }
  })
  
  # ===========================================================================
  # 3.5 DONNEES REACTIVES
  # ===========================================================================
  raw_data <- reactive({
    req(input$stock_ticker)
    load_or_download_ticker(input$stock_ticker)
  })
  
  filtered_data <- reactive({
    df <- raw_data()
    if (is.null(df)) return(NULL)
    filter_by_period(df, input$start_date, input$end_date)
  })
  
  regression_obj <- reactive({
    df <- filtered_data()
    if (is.null(df) || nrow(df) < 10) return(NULL)
    fit_log_regression(df)
  })
  
  plot_data <- reactive({
    df <- filtered_data()
    reg <- regression_obj()
    if (is.null(df) || is.null(reg)) return(NULL)
    prepare_plot_data(df, reg)
  })
  
  # ===========================================================================
  # 3.6 VUE GENERALE
  # ===========================================================================
  output$overview_text <- renderUI({
    df <- filtered_data()
    reg <- regression_obj()
    
    if (is.null(df) || nrow(df) < 2) {
      return(div(class = "stan-alert", "Pas assez de données pour construire un résumé."))
    }
    
    last_price <- tail(df$Price, 1)
    perf_6m <- calc_performance_months(df, 6)
    pos_sigma <- calc_current_position_sigma(reg)
    
    trend_text <- if (is.na(perf_6m)) {
      "La tendance récente n'est pas disponible."
    } else if (perf_6m >= 0) {
      "Le titre présente une dynamique récente positive."
    } else {
      "Le titre présente une dynamique récente négative."
    }
    
    sigma_text <- if (is.na(pos_sigma)) {
      "La position par rapport à la tendance n'est pas disponible."
    } else if (pos_sigma > 1) {
      "Le prix est au-dessus de sa tendance théorique."
    } else if (pos_sigma < -1) {
      "Le prix est en dessous de sa tendance théorique."
    } else {
      "Le prix se situe dans une zone proche de sa tendance."
    }
    
    HTML(paste0(
      "<b>Ticker sélectionné :</b> ", input$stock_ticker, "<br>",
      "<b>Période :</b> ", fmt_date_fr(input$start_date), " au ", fmt_date_fr(input$end_date), "<br><br>",
      trend_text, "<br>",
      sigma_text, "<br><br>",
      "<b>Dernier prix observé :</b> ", fmt_number(last_price), "<br>",
      "<b>Nombre d'observations utilisées :</b> ", nrow(df)
    ))
  })
  
  output$overview_price <- renderValueBox({
    df <- filtered_data()
    val <- if (is.null(df) || nrow(df) == 0) "N/A" else fmt_number(tail(df$Price, 1))
    valueBox(fit_valuebox_text(val), "Dernier prix", icon = icon("coins"), color = "light-blue")
  })
  
  output$overview_perf <- renderValueBox({
    df <- filtered_data()
    val <- calc_performance_months(df, 6)
    color <- if (is.na(val)) "gray" else if (val >= 0) "green" else "red"
    icon_name <- if (is.na(val)) "minus" else if (val >= 0) "arrow-up" else "arrow-down"
    valueBox(fit_valuebox_text(fmt_percent(val)), "Performance 6M", icon = icon(icon_name), color = color)
  })
  
  output$overview_sigma <- renderValueBox({
    reg <- regression_obj()
    val <- calc_current_position_sigma(reg)
    valueBox(fit_valuebox_text(fmt_number(val, digits = 2)), "Position σ", icon = icon("bullseye"), color = "purple")
  })
  
  output$overview_rows <- renderValueBox({
    df <- filtered_data()
    val <- if (is.null(df)) "0" else as.character(nrow(df))
    valueBox(fit_valuebox_text(val), "Observations", icon = icon("table"), color = "yellow")
  })
  
  # ===========================================================================
  # 3.7 INDICATEURS
  # ===========================================================================
  output$last_price_box <- renderValueBox({
    df <- filtered_data()
    val <- if (is.null(df) || nrow(df) == 0) "N/A" else fmt_number(tail(df$Price, 1))
    valueBox(fit_valuebox_text(val), "Dernier prix", icon = icon("euro-sign"), color = "light-blue")
  })
  
  output$last_update_box <- renderValueBox({
    df <- filtered_data()
    val <- if (is.null(df) || nrow(df) == 0) "N/A" else fmt_date_fr(tail(df$Date, 1))
    valueBox(fit_valuebox_text(val, large = "24px", small = "18px"), "Dernière mise à jour", icon = icon("calendar-days"), color = "aqua")
  })
  
  output$volatility_box <- renderValueBox({
    df <- filtered_data()
    val <- calc_volatility(df)
    valueBox(fit_valuebox_text(fmt_percent(val)), "Volatilité", icon = icon("wave-square"), color = "orange")
  })
  
  output$cagr_box <- renderValueBox({
    df <- filtered_data()
    val <- calc_cagr(df)
    color <- if (is.na(val)) "gray" else if (val >= 0) "green" else "red"
    icon_name <- if (is.na(val)) "minus" else "chart-line"
    valueBox(fit_valuebox_text(fmt_percent(val)), "CAGR", icon = icon(icon_name), color = color)
  })
  
  # ===========================================================================
  # 3.8 REGLAGES
  # ===========================================================================
  output$settings_info_table <- renderDT({
    df <- filtered_data()
    
    info_tbl <- data.frame(
      Indicateur = c("Ticker", "Date minimale", "Date maximale", "Nombre de lignes"),
      Valeur = c(
        input$stock_ticker,
        ifelse(is.null(df) || nrow(df) == 0, "N/A", as.character(min(df$Date))),
        ifelse(is.null(df) || nrow(df) == 0, "N/A", as.character(max(df$Date))),
        ifelse(is.null(df), 0, nrow(df))
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(info_tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })
  
  # ===========================================================================
  # 3.9 PERFORMANCE
  # ===========================================================================
  render_perf_box <- function(df_reactive, months_n, label, output_name) {
    output[[output_name]] <- renderValueBox({
      df <- df_reactive()
      val <- calc_performance_months(df, months_n)
      
      if (is.na(val)) {
        valueBox(fit_valuebox_text("N/A"), label, icon = icon("minus"), color = "gray")
      } else {
        couleur <- if (val >= 0) "green" else "red"
        signe <- if (val >= 0) "+" else ""
        valueBox(
          fit_valuebox_text(paste0(signe, sprintf("%.2f%%", val))),
          label,
          icon = icon(if (val >= 0) "arrow-up" else "arrow-down"),
          color = couleur
        )
      }
    })
  }
  
  render_perf_box(filtered_data, 1, "1 Mois", "perf_1m")
  render_perf_box(filtered_data, 6, "6 Mois", "perf_6m")
  render_perf_box(filtered_data, 12, "1 An", "perf_1a")
  render_perf_box(filtered_data, 36, "3 Ans", "perf_3a")
  render_perf_box(filtered_data, 60, "5 Ans", "perf_5a")
  
  output$regression_metrics_table <- renderDT({
    df <- filtered_data()
    reg <- regression_obj()
    
    if (is.null(df) || is.null(reg)) {
      tbl <- data.frame(
        Mesure = c("Beta", "Sigma", "Position actuelle (σ)", "Valeur théorique actuelle", "Projection 1 an", "Projection 5 ans"),
        Valeur = rep("N/A", 6)
      )
      return(datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE)))
    }
    
    beta <- reg$beta
    sigma <- reg$sigma
    pos_sigma <- calc_current_position_sigma(reg)
    theo_now <- calc_theoretical_current_price(reg)
    proj_1y <- project_theoretical_price(reg, 1)
    proj_5y <- project_theoretical_price(reg, 5)
    
    tbl <- data.frame(
      Mesure = c(
        "Beta (pente)",
        "Sigma (écart-type des résidus)",
        "Position actuelle (σ)",
        "Valeur théorique actuelle",
        "Valeur théorique dans 1 an",
        "Valeur théorique dans 5 ans"
      ),
      Valeur = c(
        fmt_number(beta, 6),
        fmt_number(sigma, 6),
        fmt_number(pos_sigma, 3),
        fmt_number(theo_now, 2),
        fmt_number(proj_1y, 2),
        fmt_number(proj_5y, 2)
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })
  
  # ===========================================================================
  # 3.10 GRAPHIQUE
  # ===========================================================================
  output$price_plot <- renderPlotly({
    df <- filtered_data()
    plot_df <- plot_data()
    
    validate(
      need(!is.null(df) && nrow(df) >= 10, "Pas assez de données pour afficher le graphique."),
      need(!is.null(plot_df), "Régression indisponible sur cette période.")
    )
    
    plot_ly(data = plot_df, x = ~Date) %>%
      add_ribbons(
        ymin = ~lower_2, ymax = ~upper_2,
        name = "±2σ",
        fillcolor = "rgba(52, 152, 219, 0.10)",
        line = list(color = "transparent"),
        hoverinfo = "skip"
      ) %>%
      add_ribbons(
        ymin = ~lower_1, ymax = ~upper_1,
        name = "±1σ",
        fillcolor = "rgba(52, 152, 219, 0.22)",
        line = list(color = "transparent"),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~Price,
        name = "Prix observé",
        line = list(width = 2),
        hovertemplate = paste(
          "<b>Date :</b> %{x}<br>",
          "<b>Prix :</b> %{y:.2f}<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~fitted_price,
        name = "Tendance théorique",
        line = list(color = "red", dash = "dash", width = 2),
        hovertemplate = paste(
          "<b>Date :</b> %{x}<br>",
          "<b>Tendance :</b> %{y:.2f}<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = paste("Analyse de", input$stock_ticker),
          x = 0.5
        ),
        yaxis = list(title = "Prix", type = "log"),
        xaxis = list(title = "Date"),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        ),
        margin = list(t = 80, b = 80),
        paper_bgcolor = "#111827",
        plot_bgcolor = "#111827",
        font = list(color = "#e5e7eb")
      )
  })
  
  # ===========================================================================
  # 3.11 GESTION DES DONNEES
  # ---------------------------------------------------------------------------
  # Règle de mise à jour :
  # - si l'utilisateur choisit un ticker précis -> mise à jour de ce ticker
  # - si l'utilisateur choisit "Tous les tickers" -> mise à jour seulement
  # des tickers déjà présents localement dans data/
  # ===========================================================================
  update_status_text <- reactiveVal("")
  add_status_text <- reactiveVal("")
  import_status_text <- reactiveVal("")
  
  observeEvent(input$update_btn, {
    req(input$update_ticker)
    
    # ------------------------------------------------------------------------
    # CAS 1 : TOUS LES TICKERS
    # ------------------------------------------------------------------------
    if (input$update_ticker == "ALL_TICKERS_UPDATE") {
      
      # Ici on met à jour UNIQUEMENT les tickers déjà présents localement
      tickers_to_update <- available_local_tickers()
      
      if (length(tickers_to_update) == 0) {
        update_status_text("Aucun ticker local à mettre à jour.")
      } else {
        results <- sapply(tickers_to_update, function(tk) {
          tryCatch(
            update_ticker_data(tk, from = "2015-01-01"),
            error = function(e) FALSE
          )
        })
        
        n_ok <- sum(results, na.rm = TRUE)
        n_total <- length(results)
        n_fail <- n_total - n_ok
        
        update_status_text(
          paste0(
            "Mise à jour terminée : ",
            n_ok, " ticker(s) réussi(s), ",
            n_fail, " échec(s)."
          )
        )
      }
      
    } else {
      
      # ----------------------------------------------------------------------
      # CAS 2 : UN SEUL TICKER
      # ----------------------------------------------------------------------
      ok <- update_ticker_data(input$update_ticker, from = "2015-01-01")
      
      if (ok) {
        update_status_text(paste("Mise à jour réussie pour", input$update_ticker))
      } else {
        update_status_text(paste("Échec de la mise à jour pour", input$update_ticker))
      }
    }
  })
  
  observeEvent(input$add_ticker_btn, {
    req(input$new_ticker)
    new_ticker <- toupper(trimws(input$new_ticker))
    
    if (nchar(new_ticker) == 0) {
      add_status_text("Veuillez saisir un ticker valide.")
    } else {
      ok <- update_ticker_data(new_ticker, from = "2015-01-01")
      if (ok) {
        add_status_text(paste("Ticker ajouté avec succès :", new_ticker))
      } else {
        add_status_text(paste("Impossible de télécharger le ticker :", new_ticker))
      }
    }
  })
  
  observeEvent(input$import_btn, {
    req(input$csv_file)
    ticker_name <- trimws(input$import_ticker_name)
    if (nchar(ticker_name) == 0) ticker_name <- "IMPORT_USER"
    
    imported <- import_user_csv(input$csv_file$datapath, ticker_name = ticker_name)
    
    if (is.null(imported)) {
      import_status_text("Import échoué : fichier invalide ou colonnes insuffisantes.")
    } else {
      import_status_text(paste("Import réussi pour le ticker :", ticker_name))
    }
  })
  
  output$update_status <- renderText({ update_status_text() })
  output$add_ticker_status <- renderText({ add_status_text() })
  output$import_status <- renderText({ import_status_text() })
  
  output$data_summary_table <- renderDT({
    files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = TRUE)
    
    if (length(files) == 0) {
      tbl <- data.frame(Message = "Aucune donnée locale disponible.")
      return(datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE)))
    }
    
    summary_list <- lapply(files, function(f) {
      df <- tryCatch(readr::read_csv(f, show_col_types = FALSE), error = function(e) NULL)
      df_clean <- clean_stock_data(df, ticker = gsub("\\.csv$", "", basename(f)))
      
      data.frame(
        Ticker = gsub("\\.csv$", "", basename(f)),
        Lignes = ifelse(is.null(df_clean), 0, nrow(df_clean)),
        Date_min = ifelse(is.null(df_clean) || nrow(df_clean) == 0, NA, as.character(min(df_clean$Date))),
        Date_max = ifelse(is.null(df_clean) || nrow(df_clean) == 0, NA, as.character(max(df_clean$Date))),
        stringsAsFactors = FALSE
      )
    })
    
    tbl <- bind_rows(summary_list)
    datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ===========================================================================
  # 3.12 QUALITE DES DONNEES
  # ===========================================================================
  output$quality_checks_table <- renderDT({
    df <- filtered_data()
    qc <- quality_checks(df)
    
    datatable(qc, rownames = FALSE, options = list(dom = "t", paging = FALSE)) %>%
      formatStyle(
        "Statut",
        target = "row",
        backgroundColor = styleEqual(
          c("OK", "Alerte", "Erreur"),
          c("#123524", "#4a3b12", "#4b1f1f")
        ),
        color = "white"
      )
  })
  
  output$quality_summary_table <- renderDT({
    df_raw <- raw_data()
    df_clean <- filtered_data()
    qs <- quality_summary(df_raw, df_clean)
    
    datatable(qs, rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })
}

# =============================================================================
# 4. LANCEMENT DE L'APPLICATION
# =============================================================================
shinyApp(ui, server)