# =============================================================================
# STAN - STock ANalyser
# global.R
# Chargement des données, nettoyage, fonctions métiers et tests qualité
# =============================================================================

# =========================
# 1. PACKAGES
# =========================
required_packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "dplyr",
  "readr",
  "lubridate",
  "quantmod",
  "zoo",
  "htmltools",
  "scales"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

options(stringsAsFactors = FALSE)
options(scipen = 999)

# =========================
# 2. DOSSIERS
# =========================
DATA_DIR <- "data"
IMPORT_DIR <- "imports"
WWW_DIR <- "www"

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!dir.exists(IMPORT_DIR)) dir.create(IMPORT_DIR, recursive = TRUE)
if (!dir.exists(WWW_DIR)) dir.create(WWW_DIR, recursive = TRUE)

# =========================
# 3. REFERENTIEL DES TICKERS
# =========================
ticker_ref <- data.frame(
  ticker = c(
    "AI.PA", "AIR.PA", "ACA.PA", "BN.PA", "BNP.PA", "CA.PA", "CAP.PA",
    "CS.PA", "DG.PA", "DSY.PA", "EL.PA", "ENGI.PA", "EN.PA", "GLE.PA",
    "HO.PA", "KER.PA", "LR.PA", "MC.PA", "ML.PA", "ORA.PA", "PUB.PA",
    "RI.PA", "RMS.PA", "RNO.PA", "SAF.PA", "SAN.PA", "SGO.PA",
    "STLAM.MI", "SU.PA", "TEP.PA", "TTE.PA", "VIE.PA", "VIV.PA",
    "VK.PA", "WLN.PA", "ALO.PA", "MT.AS", "SLB", "AAPL", "MSFT",
    "NVDA", "AMZN", "META", "GOOGL", "TSLA"
  ),
  company = c(
    "Air Liquide", "Airbus", "Crédit Agricole", "Danone", "BNP Paribas",
    "Carrefour", "Capgemini", "AXA", "Vinci", "Dassault Systèmes",
    "EssilorLuxottica", "Engie", "Bouygues", "Société Générale",
    "Thales", "Kering", "Legrand", "LVMH", "Michelin", "Orange",
    "Publicis", "Pernod Ricard", "Hermès", "Renault", "Safran", "Sanofi",
    "Saint-Gobain", "Stellantis", "Schneider Electric", "Teleperformance",
    "TotalEnergies", "Veolia", "Vivendi", "Vallourec", "Worldline",
    "Alstom", "ArcelorMittal", "Schlumberger", "Apple", "Microsoft",
    "NVIDIA", "Amazon", "Meta", "Alphabet", "Tesla"
  ),
  stringsAsFactors = FALSE
)

# =========================
# 4. FONCTIONS UTILITAIRES
# =========================

sanitize_ticker_filename <- function(ticker) {
  gsub("[^A-Za-z0-9_\\-\\.]", "_", ticker)
}

get_csv_path <- function(ticker) {
  file.path(DATA_DIR, paste0(sanitize_ticker_filename(ticker), ".csv"))
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

fmt_percent <- function(x, digits = 2) {
  if (is.na(x)) return("N/A")
  paste0(ifelse(x > 0, "+", ""), sprintf(paste0("%.", digits, "f%%"), x))
}

fmt_number <- function(x, digits = 2, suffix = "") {
  if (is.na(x)) return("N/A")
  paste0(format(round(x, digits), nsmall = digits, big.mark = " "), suffix)
}

fmt_date_fr <- function(x) {
  if (is.na(x)) return("N/A")
  format(as.Date(x), "%d/%m/%Y")
}

# =========================
# 5. TELECHARGEMENT YAHOO
# =========================
download_ticker_data <- function(ticker, from = "2015-01-01") {
  tryCatch({
    raw_xts <- suppressWarnings(
      quantmod::getSymbols(
        Symbols = ticker,
        src = "yahoo",
        from = from,
        auto.assign = FALSE,
        warnings = FALSE
      )
    )
    
    if (NROW(raw_xts) == 0) return(NULL)
    
    df <- data.frame(
      Date = as.Date(index(raw_xts)),
      coredata(raw_xts),
      row.names = NULL
    )
    
    colnames(df) <- gsub(paste0("^", ticker, "\\."), "", colnames(df))
    colnames(df) <- gsub("Adjusted", "Adjusted", colnames(df))
    
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df$Ticker <- ticker
    
    return(df)
  }, error = function(e) {
    message("Erreur téléchargement pour ", ticker, " : ", e$message)
    return(NULL)
  })
}

save_ticker_data <- function(df, ticker) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  readr::write_csv(df, get_csv_path(ticker))
  TRUE
}

# =========================
# 6. STANDARDISATION / NETTOYAGE
# =========================
standardize_columns <- function(df, ticker = NA) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  names(df) <- trimws(names(df))
  
  # Harmonisation des noms courants possibles
  rename_map <- c(
    "date" = "Date",
    "open" = "Open",
    "high" = "High",
    "low" = "Low",
    "close" = "Close",
    "adj.close" = "Adjusted",
    "adjusted" = "Adjusted",
    "adj_close" = "Adjusted",
    "adjclose" = "Adjusted",
    "volume" = "Volume",
    "ticker" = "Ticker",
    "symbol" = "Ticker"
  )
  
  lower_names <- tolower(names(df))
  for (i in seq_along(lower_names)) {
    if (lower_names[i] %in% names(rename_map)) {
      names(df)[i] <- rename_map[[lower_names[i]]]
    }
  }
  
  if (!"Date" %in% names(df)) return(NULL)
  
  if (!"Adjusted" %in% names(df) && "Close" %in% names(df)) {
    df$Adjusted <- df$Close
  }
  
  if (!"Close" %in% names(df) && "Adjusted" %in% names(df)) {
    df$Close <- df$Adjusted
  }
  
  needed_cols <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  for (col in needed_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  
  if (!"Ticker" %in% names(df)) {
    df$Ticker <- ticker
  }
  
  df <- df[, c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "Ticker")]
  
  return(df)
}

clean_stock_data <- function(df, ticker = NA) {
  df <- standardize_columns(df, ticker = ticker)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Conversion de date
  if (!inherits(df$Date, "Date")) {
    parsed_date <- suppressWarnings(lubridate::ymd(df$Date))
    if (all(is.na(parsed_date))) {
      parsed_date <- suppressWarnings(lubridate::dmy(df$Date))
    }
    if (all(is.na(parsed_date))) {
      parsed_date <- as.Date(df$Date)
    }
    df$Date <- parsed_date
  }
  
  # Conversion numérique
  num_cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  for (col in num_cols) df[[col]] <- safe_numeric(df[[col]])
  
  # Prix de référence
  df$Price <- dplyr::coalesce(df$Adjusted, df$Close)
  
  # Nettoyage
  df <- df %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::arrange(Date) %>%
    dplyr::distinct(Date, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(Price), Price > 0)
  
  if (nrow(df) == 0) return(NULL)
  
  return(df)
}

read_local_ticker <- function(ticker) {
  path <- get_csv_path(ticker)
  if (!file.exists(path)) return(NULL)
  
  df <- tryCatch({
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  }, error = function(e) NULL)
  
  clean_stock_data(df, ticker = ticker)
}

load_or_download_ticker <- function(ticker, from = "2015-01-01") {
  df <- read_local_ticker(ticker)
  
  if (is.null(df) || nrow(df) == 0) {
    df <- download_ticker_data(ticker, from = from)
    df <- clean_stock_data(df, ticker = ticker)
    if (!is.null(df)) save_ticker_data(df, ticker)
  }
  
  return(df)
}

update_ticker_data <- function(ticker, from = "2015-01-01") {
  df <- download_ticker_data(ticker, from = from)
  df <- clean_stock_data(df, ticker = ticker)
  if (!is.null(df) && nrow(df) > 0) {
    save_ticker_data(df, ticker)
    return(TRUE)
  }
  FALSE
}

# =========================
# 7. FILTRAGE PERIODE
# =========================
filter_by_period <- function(df, start_date = NULL, end_date = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  if (!is.null(start_date)) {
    df <- df %>% dplyr::filter(Date >= as.Date(start_date))
  }
  
  if (!is.null(end_date)) {
    df <- df %>% dplyr::filter(Date <= as.Date(end_date))
  }
  
  if (nrow(df) == 0) return(NULL)
  df
}

# =========================
# 8. CALCULS FINANCIERS
# =========================
compute_returns <- function(df) {
  if (is.null(df) || nrow(df) < 2) return(rep(NA_real_, nrow(df)))
  c(NA, diff(log(df$Price)))
}

calc_volatility <- function(df) {
  if (is.null(df) || nrow(df) < 3) return(NA_real_)
  rets <- compute_returns(df)
  sd(rets, na.rm = TRUE) * 100
}

calc_cagr <- function(df) {
  if (is.null(df) || nrow(df) < 2) return(NA_real_)
  
  p0 <- df$Price[1]
  p1 <- df$Price[nrow(df)]
  days <- as.numeric(max(df$Date) - min(df$Date))
  
  if (is.na(p0) || is.na(p1) || p0 <= 0 || p1 <= 0 || days <= 0) return(NA_real_)
  
  years <- days / 365.25
  if (years <= 0) return(NA_real_)
  
  ((p1 / p0)^(1 / years) - 1) * 100
}

calc_performance_months <- function(df, months_back = 1) {
  if (is.null(df) || nrow(df) < 2) return(NA_real_)
  
  last_date <- max(df$Date, na.rm = TRUE)
  target_date <- last_date %m-% months(months_back)
  
  ref_row <- which.min(abs(as.numeric(df$Date - target_date)))
  last_row <- nrow(df)
  
  if (length(ref_row) == 0 || ref_row >= last_row) return(NA_real_)
  
  p0 <- df$Price[ref_row]
  p1 <- df$Price[last_row]
  
  if (is.na(p0) || is.na(p1) || p0 <= 0) return(NA_real_)
  
  ((p1 / p0) - 1) * 100
}

fit_log_regression <- function(df) {
  if (is.null(df) || nrow(df) < 10) return(NULL)
  
  df <- df %>%
    dplyr::filter(!is.na(Price), Price > 0) %>%
    dplyr::arrange(Date)
  
  if (nrow(df) < 10) return(NULL)
  
  df$t <- as.numeric(df$Date - min(df$Date))
  df$log_price <- log(df$Price)
  
  model <- tryCatch(
    lm(log_price ~ t, data = df),
    error = function(e) NULL
  )
  
  if (is.null(model)) return(NULL)
  
  preds <- predict(model, newdata = df)
  resid <- df$log_price - preds
  sigma <- sd(resid, na.rm = TRUE)
  
  list(
    model = model,
    data = df,
    fitted_log = preds,
    residuals_log = resid,
    beta = unname(coef(model)[["t"]]),
    intercept = unname(coef(model)[["(Intercept)"]]),
    sigma = sigma
  )
}

calc_current_position_sigma <- function(reg_obj) {
  if (is.null(reg_obj) || is.na(reg_obj$sigma) || reg_obj$sigma == 0) return(NA_real_)
  tail(reg_obj$residuals_log, 1) / reg_obj$sigma
}

calc_theoretical_current_price <- function(reg_obj) {
  if (is.null(reg_obj)) return(NA_real_)
  exp(tail(reg_obj$fitted_log, 1))
}

project_theoretical_price <- function(reg_obj, years_ahead = 1) {
  if (is.null(reg_obj)) return(NA_real_)
  
  last_t <- max(reg_obj$data$t, na.rm = TRUE)
  future_t <- last_t + round(365.25 * years_ahead)
  
  pred_log <- predict(reg_obj$model, newdata = data.frame(t = future_t))
  exp(pred_log)
}

prepare_plot_data <- function(df, reg_obj) {
  if (is.null(df) || is.null(reg_obj)) return(NULL)
  
  plot_df <- reg_obj$data
  plot_df$fitted_price <- exp(reg_obj$fitted_log)
  plot_df$upper_1 <- exp(reg_obj$fitted_log + reg_obj$sigma)
  plot_df$lower_1 <- exp(reg_obj$fitted_log - reg_obj$sigma)
  plot_df$upper_2 <- exp(reg_obj$fitted_log + 2 * reg_obj$sigma)
  plot_df$lower_2 <- exp(reg_obj$fitted_log - 2 * reg_obj$sigma)
  
  plot_df
}

# =========================
# 9. QUALITE DES DONNEES
# =========================
quality_checks <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Test = c("Colonnes obligatoires", "Dates valides", "Doublons", "Prix manquants", "Série suffisante", "Ordre chronologique"),
      Statut = c("Erreur", "Erreur", "Erreur", "Erreur", "Erreur", "Erreur"),
      Detail = c("Aucune donnée", "Aucune donnée", "Aucune donnée", "Aucune donnée", "Aucune donnée", "Aucune donnée")
    ))
  }
  
  has_required <- all(c("Date", "Price") %in% names(df))
  valid_dates <- all(!is.na(df$Date))
  duplicates_n <- sum(duplicated(df$Date))
  missing_price_n <- sum(is.na(df$Price))
  enough_series <- nrow(df) >= 30
  ordered <- isTRUE(all(diff(df$Date) >= 0))
  
  data.frame(
    Test = c(
      "Colonnes obligatoires",
      "Dates valides",
      "Doublons",
      "Prix manquants",
      "Série suffisante",
      "Ordre chronologique"
    ),
    Statut = c(
      ifelse(has_required, "OK", "Erreur"),
      ifelse(valid_dates, "OK", "Erreur"),
      ifelse(duplicates_n == 0, "OK", "Alerte"),
      ifelse(missing_price_n == 0, "OK", "Alerte"),
      ifelse(enough_series, "OK", "Alerte"),
      ifelse(ordered, "OK", "Erreur")
    ),
    Detail = c(
      ifelse(has_required, "Colonnes nécessaires présentes", "Colonnes manquantes"),
      ifelse(valid_dates, "Toutes les dates sont valides", "Dates invalides détectées"),
      paste("Nombre de doublons :", duplicates_n),
      paste("Nombre de prix manquants :", missing_price_n),
      paste("Nombre de lignes :", nrow(df)),
      ifelse(ordered, "Données triées chronologiquement", "Tri chronologique incorrect")
    ),
    stringsAsFactors = FALSE
  )
}

quality_summary <- function(df_raw, df_clean) {
  n_raw <- if (is.null(df_raw)) 0 else nrow(df_raw)
  n_clean <- if (is.null(df_clean)) 0 else nrow(df_clean)
  
  duplicates_removed <- if (is.null(df_raw) || is.null(df_clean)) {
    NA
  } else {
    max(0, n_raw - n_clean)
  }
  
  data.frame(
    Indicateur = c(
      "Nombre de lignes initiales",
      "Nombre de lignes après nettoyage",
      "Lignes supprimées / écart",
      "Date minimum",
      "Date maximum",
      "Statut final"
    ),
    Valeur = c(
      n_raw,
      n_clean,
      duplicates_removed,
      ifelse(is.null(df_clean) || n_clean == 0, "N/A", as.character(min(df_clean$Date))),
      ifelse(is.null(df_clean) || n_clean == 0, "N/A", as.character(max(df_clean$Date))),
      ifelse(!is.null(df_clean) && n_clean >= 30, "Exploitable", "Insuffisant / partiel")
    ),
    stringsAsFactors = FALSE
  )
}

# =========================
# 10. IMPORT CSV
# =========================
import_user_csv <- function(file_path, ticker_name = "IMPORT_USER") {
  if (is.null(file_path) || !file.exists(file_path)) return(NULL)
  
  df <- tryCatch({
    readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE)
  }, error = function(e) {
    tryCatch(
      read.csv(file_path, stringsAsFactors = FALSE),
      error = function(e2) NULL
    )
  })
  
  df_clean <- clean_stock_data(df, ticker = ticker_name)
  if (is.null(df_clean) || nrow(df_clean) == 0) return(NULL)
  
  out_path <- get_csv_path(ticker_name)
  readr::write_csv(df_clean, out_path)
  
  df_clean
}

# =========================
# 11. PRECHARGEMENT LEGER
# =========================
available_local_tickers <- function() {
  files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = FALSE)
  if (length(files) == 0) return(character(0))
  gsub("\\.csv$", "", files)
}

message("global.R chargé avec succès.")
message("Tickers référencés : ", nrow(ticker_ref))
message("Tickers locaux disponibles : ", length(available_local_tickers()))
