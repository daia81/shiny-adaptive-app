# app.R ---------------------------------------------------------------
library(shiny)
library(shinyjs)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)
library(tidyr)
library(rmarkdown)

# ============================================================
# CONFIG: file di input
# ============================================================
RULES_XLSX <- "ml_m5p_results_MONO_all_scales_tuned.xlsx"

# Dizionario lungo: Item / Testo / Scala
DICT_XLSX  <- "Dic_all_scales_clean_long (1).xlsx"
DICT_SHEET <- "Dictionary"

# ============================================================
# LETTURA DICTIONARY (globale)
# ============================================================
read_dictionary_global <- function(path = DICT_XLSX, sheet = DICT_SHEET) {
  dic <- read_xlsx(path, sheet = sheet)
  if ("Testo" %in% names(dic) && !"Text" %in% names(dic)) {
    dic <- dplyr::rename(dic, Text = Testo)
  }
  if ("Scala" %in% names(dic) && !"Scale" %in% names(dic)) {
    dic <- dplyr::rename(dic, Scale = Scala)
  }
  req_cols <- c("Item","Text","Scale")
  if (!all(req_cols %in% names(dic))) {
    stop("Nel dictionary mancano colonne: ",
         paste(setdiff(req_cols, names(dic)), collapse = ", "))
  }
  dic$Item  <- trimws(dic$Item)
  dic$Scale <- trimws(dic$Scale)
  dic
}

DICT_GLOBAL <- read_dictionary_global()
SCALES_AVAILABLE <- sort(unique(DICT_GLOBAL$Scale))

# ============================================================
# MAPPATURA SCALE_FS -> ID FOGLIO RULES (rules_<id>)
# ============================================================
FS_TO_RULE_ID <- list(
  AFFneg_FS        = "AFFneg",
  AFFpos_FS        = "AFFpos",
  COP_CB_FS        = "COP_CB",
  COP_PS_FS        = "COP_PS",
  COP_SS_FS        = "COP_SS",
  DIST_FS          = "DIST",
  DM_FS            = "DM",
  EQUIT_FS         = "EQUIT",
  FAS_FS           = "FAS",
  ICAWS_FS         = "ICAWS",
  JCQ_Control_FS   = "JCQ_Control",
  JCQ_Demand_FS    = "JCQ_Demand",
  JCQ_Support_FS   = "JCQ_Support",
  JDI_FS           = "JDI",
  OCS_FS           = "OCS",
  QWI_FS           = "QWI",
  RCA_Ambig_FS     = "RCA_Ambig",
  RCA_Conflict_FS  = "RCA_Conflict",
  SELF_EffAss_FS   = "SELF_EffAss",
  SELF_EffEmneg_FS = "SELF_EffEmneg",
  SELF_EffEmpa_FS  = "SELF_EffEmpa",
  SELF_EffLav_FS   = "SELF_EffLav"
)

FS_TO_RULE_ID <- FS_TO_RULE_ID[names(FS_TO_RULE_ID) %in% SCALES_AVAILABLE]

# ============================================================
# UNITÀ SELEZIONABILI (blocchi) E MACRO-AREE
# ============================================================
UNITS <- list(
  # Caratteristiche personali
  coping = list(
    label = "Coping",
    area  = "Caratteristiche personali",
    group = "Coping",
    scales = c("COP_CB_FS", "COP_PS_FS", "COP_SS_FS")
  ),
  autoeff = list(
    label = "Autoefficacia",
    area  = "Caratteristiche personali",
    group = "Autoefficacia",
    scales = c("SELF_EffLav_FS", "SELF_EffAss_FS", "SELF_EffEmneg_FS", "SELF_EffEmpa_FS")
  ),
  dm = list(
    label = "Disimpegno morale",
    area  = "Caratteristiche personali",
    group = "Disimpegno morale",
    scales = c("DM_FS")
  ),
  # Benessere
  wellbeing = list(
    label = "Well-being (AFFetti)",
    area  = "Benessere",
    group = "Well-being",
    scales = c("AFFpos_FS", "AFFneg_FS")
  ),
  dist = list(
    label = "Disturbi psicofisici",
    area  = "Benessere",
    group = "Disturbi",
    scales = c("DIST_FS")
  ),
  jdi = list(
    label = "Soddisfazione lavorativa",
    area  = "Benessere",
    group = "Soddisfazione",
    scales = c("JDI_FS")
  ),
  # Risorse organizzative
  fas = list(
    label = "Autonomia",
    area  = "Risorse organizzative",
    group = "Risorse organizzative",
    scales = c("FAS_FS")
  ),
  jcq_ctrl = list(
    label = "Controllo sul lavoro",
    area  = "Risorse organizzative",
    group = "Risorse organizzative",
    scales = c("JCQ_Control_FS")
  ),
  jcq_sup = list(
    label = "Supporto sociale (lavoro)",
    area  = "Risorse organizzative",
    group = "Risorse organizzative",
    scales = c("JCQ_Support_FS")
  ),
  equit = list(
    label = "Equità organizzativa",
    area  = "Risorse organizzative",
    group = "Risorse organizzative",
    scales = c("EQUIT_FS")
  ),
  # Domande organizzative
  qwi = list(
    label = "Carico di lavoro",
    area  = "Domande organizzative",
    group = "Domande organizzative",
    scales = c("QWI_FS")
  ),
  ocs = list(
    label = "Costrittività organizzative",
    area  = "Domande organizzative",
    group = "Domande organizzative",
    scales = c("OCS_FS")
  ),
  rca_conf = list(
    label = "Conflitto di ruolo",
    area  = "Domande organizzative",
    group = "Domande organizzative",
    scales = c("RCA_Conflict_FS")
  ),
  jcq_dem = list(
    label = "Domanda lavorativa",
    area  = "Domande organizzative",
    group = "Domande organizzative",
    scales = c("JCQ_Demand_FS")
  ),
  icaws = list(
    label = "Conflitti interpersonali",
    area  = "Domande organizzative",
    group = "Domande organizzative",
    scales = c("ICAWS_FS")
  )
)

AREA_UNITS <- list(
  "Caratteristiche personali" = c("coping","autoeff","dm"),
  "Benessere"                 = c("wellbeing","dist","jdi"),
  "Risorse organizzative"     = c("fas","jcq_ctrl","jcq_sup","equit"),
  "Domande organizzative"     = c("qwi","ocs","rca_conf","jcq_dem","icaws")
)

# mappa scala -> area / gruppo
all_scales <- unique(unlist(lapply(UNITS, function(u) u$scales)))
FS_TO_AREA  <- setNames(rep(NA_character_, length(all_scales)), all_scales)
FS_TO_GROUP <- setNames(rep(NA_character_, length(all_scales)), all_scales)
for (id in names(UNITS)) {
  u <- UNITS[[id]]
  FS_TO_AREA[u$scales]  <- u$area
  FS_TO_GROUP[u$scales] <- u$group
}

# ============================================================
# GRUPPI PER LO "STACCO"
# ============================================================
SCALE_GROUP <- c(
  COP_CB_FS        = "coping",
  COP_PS_FS        = "coping",
  COP_SS_FS        = "coping",
  SELF_EffLav_FS   = "autoeff",
  SELF_EffAss_FS   = "autoeff",
  SELF_EffEmneg_FS = "autoeff",
  SELF_EffEmpa_FS  = "autoeff",
  AFFpos_FS        = "wellbeing",
  AFFneg_FS        = "wellbeing"
)

get_group_id <- function(scala_fs) {
  if (scala_fs %in% names(SCALE_GROUP)) {
    SCALE_GROUP[[scala_fs]]
  } else {
    scala_fs
  }
}

# ============================================================
# NOMI "BELLI" PER SCALE (report)
# ============================================================
SCALE_REPORT_LABEL <- c(
  AFFneg_FS        = "Affettività negativa",
  AFFpos_FS        = "Affettività positiva",
  COP_CB_FS        = "Comportamenti di compensazione",
  COP_PS_FS        = "Coping di controllo",
  COP_SS_FS        = "Coping di supporto sociale",
  DIST_FS          = "Disturbi psicofisici",
  DM_FS            = "Disimpegno morale",
  EQUIT_FS         = "Equità organizzativa",
  FAS_FS           = "Autonomia",
  ICAWS_FS         = "Conflitti interpersonali",
  JCQ_Control_FS   = "Controllo sul lavoro",
  JCQ_Demand_FS    = "Domanda lavorativa",
  JCQ_Support_FS   = "Supporto sociale (lavoro)",
  JDI_FS           = "Soddisfazione lavorativa",
  OCS_FS           = "Costrittività organizzative",
  QWI_FS           = "Carico di lavoro",
  RCA_Ambig_FS     = "Ambiguità di ruolo",
  RCA_Conflict_FS  = "Conflitto di ruolo",
  SELF_EffAss_FS   = "Autoefficacia assertiva",
  SELF_EffEmneg_FS = "Autoefficacia emotiva",
  SELF_EffEmpa_FS  = "Autoefficacia empatica",
  SELF_EffLav_FS   = "Autoefficacia lavorativa"
)

# ============================================================
# CONFIG RISPOSTE / ISTRUZIONI
# ============================================================
DEFAULT_CFG <- list(
  label        = NULL,
  values       = 1:5,
  labels       = c("Per niente", "Poco", "Abbastanza", "Molto", "Moltissimo"),
  instructions = "Per ciascuna affermazione indichi quanto la rappresenta rispetto al suo lavoro."
)

SELF_LABELS_7 <- c(
  "Per nulla capace",
  "Poco capace",
  "Piuttosto poco capace",
  "Mediamente capace",
  "Abbastanza capace",
  "Molto capace",
  "Del tutto capace"
)

OVERRIDE_CFG <- list(
  ICAWS_FS = list(
    label        = "Aggressioni verbali (ICAWS)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Consideri alcuni eventi che possono accadere nel lavoro. Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?"
  ),
  QWI_FS = list(
    label        = "Carico di lavoro (QWI)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Consideri alcuni eventi che possono accadere nel lavoro. Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?"
  ),
  OCS_FS = list(
    label        = "Stressors organizzativi (OCS)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Quante volte trova difficile o impossibile fare il suo lavoro a causa degli eventi indicati di seguito?"
  ),
  EQUIT_FS = list(
    label        = "Equità organizzativa (EQUIT)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Relativamente alle situazioni di seguito descritte indichi con quale frequenza si verificano nella sua organizzazione."
  ),
  FAS_FS = list(
    label        = "Autonomia (FAS)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni. Legga attentamente ciascuna frase e indichi con quale frequenza deve chiedere l’autorizzazione per svolgere le seguenti attività."
  ),
  COP_CB_FS = list(
    label        = "Comportamenti di compensazione (COP_CB)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni. Indichi la risposta che meglio esprime il suo parere."
  ),
  COP_PS_FS = list(
    label        = "Coping di controllo (COP_PS)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni. Indichi la risposta che meglio esprime il suo parere."
  ),
  COP_SS_FS = list(
    label        = "Coping di supporto sociale (COP_SS)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni. Indichi la risposta che meglio esprime il suo parere."
  ),
  AFFpos_FS = list(
    label        = "Affettività positiva (AFFpos)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Di seguito sono presentate emozioni che il lavoro può indurre. Indichi quanto gli aspetti del suo attuale lavoro le hanno fatto provare queste emozioni negli ultimi 30 giorni."
  ),
  AFFneg_FS = list(
    label        = "Affettività negativa (AFFneg)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Di seguito sono presentate emozioni che il lavoro può indurre. Indichi quanto gli aspetti del suo attuale lavoro le hanno fatto provare queste emozioni negli ultimi 30 giorni."
  ),
  RCA_Ambig_FS = list(
    label        = "Ambiguità di ruolo (RCA Ambiguità)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni nel lavoro. Per ciascuna di esse indichi l'opzione che meglio descrive la sua esperienza nel suo attuale lavoro."
  ),
  RCA_Conflict_FS = list(
    label        = "Conflitti di ruolo (RCA Conflitto)",
    values       = 1:5,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta",
                     "Abbastanza spesso", "Molto spesso o sempre"),
    instructions = "Le affermazioni di seguito presentate descrivono alcune situazioni comuni nel lavoro. Per ciascuna di esse indichi l'opzione che meglio descrive la sua esperienza nel suo attuale lavoro."
  ),
  DIST_FS = list(
    label        = "Disturbi psicofisici (DIST)",
    values       = 1:4,
    labels       = c("Mai", "Raramente", "A volte", "Spesso"),
    instructions = "Negli ultimi 6 mesi, con quale frequenza le è capitato di avvertire ciascuno dei seguenti disturbi?"
  ),
  DM_FS = list(
    label        = "Disimpegno morale (DM)",
    values       = 1:5,
    labels       = c("Per nulla d'accordo",
                     "Poco d'accordo",
                     "Mediamente d'accordo",
                     "Molto d'accordo",
                     "Del tutto d'accordo"),
    instructions = "Esprima il suo grado di accordo con le affermazioni riportate, pensando alla sua esperienza lavorativa."
  ),
  JCQ_Control_FS = list(
    label        = "Controllo sul lavoro (JCQ Control)",
    values       = 1:4,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta", "Spesso"),
    instructions = "Le chiediamo di esprimere il suo parere su vari aspetti della sua vita lavorativa indicando quanto spesso le capitano le situazioni che sono riportate nelle righe seguenti."
  ),
  JCQ_Demand_FS = list(
    label        = "Domanda lavorativa (JCQ Demand)",
    values       = 1:4,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta", "Spesso"),
    instructions = "Le chiediamo di esprimere il suo parere su vari aspetti della sua vita lavorativa indicando quanto spesso le capitano le situazioni che sono riportate nelle righe seguenti."
  ),
  JCQ_Support_FS = list(
    label        = "Supporto sociale (JCQ Support)",
    values       = 1:4,
    labels       = c("Mai o quasi mai", "Raramente", "Qualche volta", "Spesso"),
    instructions = "Indichi quanto spesso riceve il supporto descritto nelle affermazioni seguenti."
  ),
  SELF_EffLav_FS = list(
    label        = "Autoefficacia lavorativa (SELF EffLav)",
    values       = 1:7,
    labels       = SELF_LABELS_7,
    instructions = "Le chiediamo di esprimere il suo parere su vari aspetti della sua vita lavorativa indicando quanto spesso le capitano le situazioni che sono riportate nelle righe seguenti."
  ),
  SELF_EffAss_FS = list(
    label        = "Autoefficacia assertiva (SELF EffAss)",
    values       = 1:7,
    labels       = SELF_LABELS_7,
    instructions = "Le seguenti affermazioni descrivono comportamenti riferiti all’attività lavorativa. Indichi, per ognuna, quanto si sente capace di mettere in atto il comportamento descritto."
  ),
  SELF_EffEmneg_FS = list(
    label        = "Autoefficacia emotiva (SELF EffEmneg)",
    values       = 1:7,
    labels       = SELF_LABELS_7,
    instructions = "Le seguenti affermazioni descrivono comportamenti riferiti all’attività lavorativa. Indichi, per ognuna, quanto si sente capace di mettere in atto il comportamento descritto."
  ),
  SELF_EffEmpa_FS = list(
    label        = "Autoefficacia empatica (SELF EffEmpa)",
    values       = 1:7,
    labels       = SELF_LABELS_7,
    instructions = "Le seguenti affermazioni descrivono comportamenti riferiti all’attività lavorativa. Indichi, per ognuna, quanto si sente capace di mettere in atto il comportamento descritto."
  ),
  JDI_FS = list(
    label        = "Job Descriptive Index (JDI)",
    values       = c(0, 1),
    labels       = c("No", "Sì"),
    instructions = "Pensi al suo lavoro in generale. Indichi \"Sì\" se la parola o frase descrive il suo lavoro nella maggior parte dei casi, \"No\" se non lo descrive."
  )
)

get_scale_cfg <- function(scala_fs) {
  cfg <- OVERRIDE_CFG[[scala_fs]]
  if (is.null(cfg)) cfg <- DEFAULT_CFG
  if (is.null(cfg$label)) cfg$label <- scala_fs
  cfg
}
# ============================================================
# LETTURA REGOLE M5P (versione nuova: usa pred_const_hat)
# ============================================================
read_leaves_m5p <- function(rule_id) {
  sheet_name <- paste0("rules_", rule_id)
  df <- read_xlsx(RULES_XLSX, sheet = sheet_name)
  
  # Controllo colonne minime
  req_cols <- c("leaf_id", "rule")
  if (!all(req_cols %in% names(df))) {
    stop("Nel foglio ", sheet_name, " mancano colonne obbligatorie: ",
         paste(setdiff(req_cols, names(df)), collapse = ", "))
  }
  
  # Scegliamo quale colonna usare come predizione di foglia
  # priorità: pred_const_hat (APP-style), altrimenti pred (vecchia versione)
  pred_col <- NULL
  if ("pred_const_hat" %in% names(df)) {
    pred_col <- "pred_const_hat"
  } else if ("pred" %in% names(df)) {
    pred_col <- "pred"
  } else {
    stop("Nel foglio ", sheet_name,
         " non trovo nessuna colonna di predizione (né pred_const_hat né pred).")
  }
  
  # pulizia righe di meta-info
  df <- df[df$rule != "Number of Rules", , drop = FALSE]
  df <- df[!is.na(df[[pred_col]]), , drop = FALSE]
  
  if (nrow(df) == 0) {
    stop("Per la scala ", rule_id,
         " non ci sono foglie con ", pred_col, " non-NA.")
  }
  
  # colonne extra, se presenti (depth, n_unique_vars, nobs_train, ecc.)
  depth_col        <- if ("depth"        %in% names(df)) df$depth        else NA_real_
  n_unique_vars_col<- if ("n_unique_vars"%in% names(df)) df$n_unique_vars else NA_real_
  nobs_train_col   <- if ("nobs_train"   %in% names(df)) df$nobs_train   else NA_real_
  pred_const_y_col <- if ("pred_const_y" %in% names(df)) df$pred_const_y else NA_real_
  
  tibble(
    node_id        = df$leaf_id,
    label          = paste0(rule_id, "_leaf_", df$leaf_id),
    path           = df$rule,
    out_score      = as.numeric(df[[pred_col]]),  # questo è quello che usa l’app
    depth          = as.numeric(depth_col),
    n_unique_vars  = as.numeric(n_unique_vars_col),
    nobs_train     = as.numeric(nobs_train_col),
    pred_const_hat = if ("pred_const_hat" %in% names(df)) as.numeric(df$pred_const_hat) else NA_real_,
    pred_const_y   = as.numeric(pred_const_y_col)
  )
}

# ============================================================
# PARSING REGOLE
# ============================================================
parse_path <- function(path_str) {
  if (is.null(path_str)) path_str <- ""
  parts <- str_split(path_str, "&")[[1]]
  parts <- str_trim(parts)
  # accetta anche "=" oltre a "=="
  rx <- "^(\\w+)\\s*(<=|>=|<|>|==|=)\\s*([-+]?[0-9]*\\.?[0-9]+)$"
  mats <- str_match(parts, rx)
  out <- tibble(
    var = mats[, 2],
    op  = mats[, 3],
    cut = suppressWarnings(as.numeric(mats[, 4]))
  )
  out <- out[!is.na(out$var), , drop = FALSE]
  out
}

compact_constraints <- function(df_cons) {
  if (nrow(df_cons) == 0) return(df_cons)
  df_cons %>%
    group_by(var) %>%
    summarize(
      lower = {
        low_ops <- op %in% c(">", ">=")
        if (any(low_ops)) max(cut[low_ops]) else -Inf
      },
      upper = {
        eq_vals <- cut[op %in% c("==", "=")]
        if (length(eq_vals) > 0) {
          min(eq_vals)
        } else if (any(op %in% c("<", "<="))) {
          min(cut[op %in% c("<", "<=")])
        } else {
          Inf
        }
      },
      .groups = "drop"
    )
}

compatible_leaf <- function(path_str, answers_named) {
  cons <- compact_constraints(parse_path(path_str))
  if (nrow(cons) == 0) return(TRUE)
  for (i in seq_len(nrow(cons))) {
    v  <- cons$var[i]
    lo <- cons$lower[i]
    up <- cons$upper[i]
    if (!v %in% names(answers_named)) next
    ans <- answers_named[[v]]
    if (!is.finite(ans)) return(FALSE)
    if (ans <= lo || ans > up) return(FALSE)
  }
  TRUE
}

filter_leaves <- function(Leaves, answers_named) {
  idx <- vapply(
    Leaves$path,
    compatible_leaf,
    logical(1),
    answers_named = answers_named
  )
  Leaves[idx, , drop = FALSE]
}

choose_next_item <- function(Leaves_sub, asked_vars) {
  if (nrow(Leaves_sub) == 0) return(NA_character_)
  all_vars <- unique(unlist(
    lapply(Leaves_sub$path, function(p) unique(parse_path(p)$var))
  ))
  vars <- setdiff(all_vars, asked_vars)
  if (length(vars) == 0) return(NA_character_)
  var_score <- sapply(vars, function(v) {
    sum(grepl(paste0("\\b", v, "\\b"), Leaves_sub$path))
  })
  names(var_score)[which.max(var_score)]
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .btn-choice { margin:6px; padding:10px 16px; font-size:16px; }
      .btn-next   { margin-top:12px; }
      .panel-instructions { background:#f8f9fa; }
      .results-box { background:#fafafa; padding:12px; border-radius:8px; border:1px solid #e5e5e5; }
      .centered { text-align:center; }
      .instr-bar { background:#f5f7fb; padding:10px 16px; margin-bottom:16px;
                   font-size:18px; color:#003366; border-radius:8px;
                   font-weight:500; }
      .scale-intro-box { background:#eef2f7; padding:16px; border-radius:8px; margin-bottom:16px; }
    "))
  ),
  
  # PAGINA SETUP
  div(
    id = "page_setup",
    fluidRow(
      column(
        12,
        wellPanel(
          class = "panel-instructions",
          h4("Impostazioni iniziali"),
          radioButtons(
            "mode",
            "Modalità di somministrazione:",
            choices = c("Somministrazione singola (con report)" = "single",
                        "Raccolta massiva (solo salvataggio punteggi)" = "massive"),
            selected = "single"
          ),
          textInput("subject_id", "ID partecipante (opzionale):", ""),
          tags$hr(),
          h4("Seleziona una o più aree e blocchi da somministrare:"),
          fluidRow(
            column(
              6,
              h5("Caratteristiche personali"),
              checkboxGroupInput(
                "sel_cp",
                label = NULL,
                choices = setNames(
                  AREA_UNITS[["Caratteristiche personali"]],
                  sapply(AREA_UNITS[["Caratteristiche personali"]],
                         function(id) UNITS[[id]]$label)
                )
              ),
              h5("Benessere / Well-being"),
              checkboxGroupInput(
                "sel_ben",
                label = NULL,
                choices = setNames(
                  AREA_UNITS[["Benessere"]],
                  sapply(AREA_UNITS[["Benessere"]],
                         function(id) UNITS[[id]]$label)
                )
              )
            ),
            column(
              6,
              h5("Risorse organizzative"),
              checkboxGroupInput(
                "sel_ris",
                label = NULL,
                choices = setNames(
                  AREA_UNITS[["Risorse organizzative"]],
                  sapply(AREA_UNITS[["Risorse organizzative"]],
                         function(id) UNITS[[id]]$label)
                )
              ),
              h5("Domande organizzative"),
              checkboxGroupInput(
                "sel_dom",
                label = NULL,
                choices = setNames(
                  AREA_UNITS[["Domande organizzative"]],
                  sapply(AREA_UNITS[["Domande organizzative"]],
                         function(id) UNITS[[id]]$label)
                )
              )
            )
          ),
          tags$hr(),
          p("Di seguito troverà alcune affermazioni che riguardano il lavoro, le emozioni e il benessere."),
          p("Per ogni blocco selezionato le verranno presentate alcune domande, con modalità adattiva, fino a stimare i punteggi delle scale corrispondenti."),
          div(
            class = "centered",
            actionButton("start_btn", "Inizia", class = "btn btn-success")
          )
        )
      )
    )
  ),
  
  # PAGINA DOMANDE
  hidden(
    div(
      id = "page_questions",
      fluidRow(
        column(12, uiOutput("scale_intro"))
      ),
      fluidRow(
        column(12, uiOutput("instructions_bar"))
      ),
      fluidRow(
        column(
          12, align = "center",
          uiOutput("question"),
          br(),
          uiOutput("buttons"),
          br(),
          actionButton("next_btn", "Prosegui", class = "btn btn-success btn-next"),
          actionButton(
            "exit_or_reset_mid",
            "Esci o ricomincia",
            class = "btn btn-secondary",
            style = "margin-left:8px;"
          )
        )
      )
    )
  ),
  
  # PAGINA RISULTATI
  hidden(
    div(
      id = "page_results",
      fluidRow(
        column(
          12,
          h3("Somministrazione completata"),
          uiOutput("results_text")
        )
      ),
      fluidRow(
        column(
          6,
          h4("Risultati per scala"),
          div(
            class = "results-box",
            tableOutput("mini_table")
          ),
          br(),
          uiOutput("download_ui"),
          uiOutput("return_link"),
          actionButton(
            "exit_or_reset_bottom",
            "Esci o ricomincia",
            class = "btn btn-secondary"
          )
        ),
        column(
          6,
          h4("Profilo sintetico"),
          plotOutput("hist_plot", height = "260px")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  Dict_all <- reactiveVal(DICT_GLOBAL)
  
  selected_scales     <- reactiveVal(character())
  current_scale_index <- reactiveVal(NA_integer_)
  last_group          <- reactiveVal(NULL)
  in_intro            <- reactiveVal(TRUE)
  
  # integrazione con Qualtrics
  return_url <- reactiveVal(NULL)
  
  observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query[["id"]]) && nzchar(query[["id"]])) {
      updateTextInput(session, "subject_id", value = query[["id"]])
    }
    if (!is.null(query[["return_url"]]) && nzchar(query[["return_url"]])) {
      return_url(query[["return_url"]])
    }
  })
  
  current_scale_id <- reactive({
    sc  <- selected_scales()
    idx <- current_scale_index()
    if (is.na(idx) || length(sc) == 0 || idx < 1 || idx > length(sc)) return(NULL)
    sc[idx]
  })
  
  Leaves_current <- reactiveVal(NULL)
  answers        <- reactiveVal(list())
  asked          <- reactiveVal(character())
  pending_choice <- reactiveVal(NULL)
  
  results_scores <- reactiveVal(
    data.frame(Scala = character(), Punteggio = numeric(), stringsAsFactors = FALSE)
  )
  
  cfg_reactive <- reactive({
    sc <- current_scale_id()
    if (is.null(sc)) return(DEFAULT_CFG)
    get_scale_cfg(sc)
  })
  
  # Intro scala
  output$scale_intro <- renderUI({
    if (!isTRUE(in_intro())) return(NULL)
    sc <- current_scale_id()
    if (is.null(sc)) return(NULL)
    cfg <- cfg_reactive()
    
    vals   <- cfg$values
    labels <- cfg$labels
    
    if (!is.null(labels) && length(labels) >= length(vals)) {
      anchors <- labels[seq_along(vals)]
    } else if (!is.null(labels) && length(labels) > 0) {
      anchors <- labels
    } else {
      anchors <- as.character(vals)
    }
    anchors <- unique(anchors)
    
    div(
      class = "scale-intro-box",
      p(cfg$instructions),
      strong("Scala di risposta:"),
      tags$ul(lapply(anchors, tags$li)),
      br(),
      div(
        class = "centered",
        actionButton("begin_scale", "Inizia questa sezione", class = "btn btn-primary")
      )
    )
  })
  
  observeEvent(input$begin_scale, {
    in_intro(FALSE)
  })
  
  # Barra istruzioni sopra item
  output$instructions_bar <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    sc <- current_scale_id()
    if (is.null(sc)) return(NULL)
    cfg <- cfg_reactive()
    div(class = "instr-bar", cfg$instructions)
  })
  
  # Inizializza scala
  init_scale <- function(scala_fs) {
    if (is.null(scala_fs) || is.na(scala_fs)) return(invisible(NULL))
    
    rule_id <- FS_TO_RULE_ID[[scala_fs]]
    if (is.null(rule_id)) {
      showModal(modalDialog(
        title = "Errore configurazione",
        paste("Non è stata trovata la mappatura RULE_ID per la scala", scala_fs),
        easyClose = TRUE
      ))
      return(invisible(NULL))
    }
    
    Leaves_current(read_leaves_m5p(rule_id))
    answers(list())
    asked(character())
    pending_choice(NULL)
    disable("next_btn")
    
    g_cur  <- get_group_id(scala_fs)
    g_prev <- last_group()
    
    if (!is.null(g_prev) && g_prev == g_cur) {
      in_intro(FALSE)
    } else {
      in_intro(TRUE)
    }
    last_group(g_cur)
    
    invisible(NULL)
  }
  
  # Start
  observeEvent(input$start_btn, {
    units_selected <- c(input$sel_cp, input$sel_ben, input$sel_ris, input$sel_dom)
    units_selected <- unique(units_selected[!is.na(units_selected) & nzchar(units_selected)])
    
    if (length(units_selected) == 0) {
      showModal(modalDialog(
        title = "Attenzione",
        "Seleziona almeno un blocco prima di iniziare.",
        easyClose = TRUE
      ))
      return()
    }
    
    ordered_units <- unlist(AREA_UNITS, use.names = FALSE)
    units_selected <- ordered_units[ordered_units %in% units_selected]
    
    fs_vec <- unlist(lapply(units_selected, function(id) UNITS[[id]]$scales), use.names = FALSE)
    fs_vec <- fs_vec[fs_vec %in% names(FS_TO_RULE_ID)]
    
    if (length(fs_vec) == 0) {
      showModal(modalDialog(
        title = "Attenzione",
        "I blocchi selezionati non contengono scale con regole configurate.",
        easyClose = TRUE
      ))
      return()
    }
    
    selected_scales(fs_vec)
    current_scale_index(1L)
    last_group(NULL)
    in_intro(TRUE)
    results_scores(
      data.frame(Scala = character(), Punteggio = numeric(), stringsAsFactors = FALSE)
    )
    
    init_scale(current_scale_id())
    
    hide("page_setup")
    show("page_questions")
    hide("page_results")
  })
  
  # Foglie compatibili
  Leaves_now <- reactive({
    L <- Leaves_current()
    if (is.null(L)) return(NULL)
    filter_leaves(L, answers())
  })
  
  # Prossimo item
  next_item <- reactive({
    if (isTRUE(in_intro())) return(NA_character_)
    L <- Leaves_now()
    if (is.null(L)) return(NA_character_)
    choose_next_item(L, asked())
  })
  
  # Domanda corrente
  output$question <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    ni <- next_item()
    sc <- current_scale_id()
    if (is.null(sc)) return(NULL)
    
    if (is.na(ni)) {
      return(h4("Nessuna ulteriore domanda per questa sezione. Premi Prosegui per continuare."))
    }
    
    dict <- Dict_all()
    row <- dplyr::filter(dict, Item == ni, Scale == sc)
    text <- if (nrow(row) > 0) row$Text[1] else ni
    h3(text)
  })
  
  # Pulsanti risposta
  output$buttons <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    cfg    <- cfg_reactive()
    vals   <- cfg$values
    labels <- cfg$labels
    k <- length(vals)
    k <- min(k, 7)
    
    btns <- lapply(seq_len(k), function(i) {
      lab <- if (!is.null(labels) && length(labels) >= i) labels[i] else as.character(vals[i])
      cls <- "btn-choice btn btn-primary"
      if (!is.null(pending_choice()) && identical(pending_choice(), vals[i])) {
        cls <- "btn-choice btn btn-primary active"
      }
      actionButton(paste0("resp_", i), lab, class = cls)
    })
    do.call(tagList, btns)
  })
  
  values_reactive <- reactive({
    cfg <- cfg_reactive()
    cfg$values
  })
  
  for (j in 1:7) {
    local({
      idx <- j
      observeEvent(input[[paste0("resp_", idx)]], {
        if (isTRUE(in_intro())) return()
        vals <- values_reactive()
        if (length(vals) < idx) return()
        pending_choice(vals[idx])
        enable("next_btn")
      })
    })
  }
  
  # Chiude scala corrente
  finalize_current_scale <- function() {
    L_now <- Leaves_now()
    sc    <- current_scale_id()
    if (is.null(sc) || is.null(L_now) || nrow(L_now) != 1) return(invisible(NULL))
    
    res_df <- results_scores()
    new_row <- data.frame(
      Scala     = sc,
      Punteggio = as.numeric(L_now$out_score[1]),
      stringsAsFactors = FALSE
    )
    res_df <- rbind(res_df, new_row)
    results_scores(res_df)
    
    scvec <- selected_scales()
    idx   <- current_scale_index()
    if (!is.na(idx) && idx < length(scvec)) {
      next_idx <- idx + 1L
      current_scale_index(next_idx)
      init_scale(scvec[next_idx])
    } else {
      hide("page_questions")
      show("page_results")
    }
    
    invisible(NULL)
  }
  
  # Prosegui
  observeEvent(input$next_btn, {
    if (isTRUE(in_intro())) return()
    
    sc  <- current_scale_id()
    req(!is.null(sc))
    
    ni  <- next_item()
    sel <- pending_choice()
    req(!is.na(ni))
    req(!is.null(sel))
    
    a <- answers()
    a[[ni]] <- as.numeric(sel)
    answers(a)
    asked(c(asked(), ni))
    
    pending_choice(NULL)
    disable("next_btn")
    
    L_now <- Leaves_now()
    if (!is.null(L_now) && nrow(L_now) == 1) {
      finalize_current_scale()
    }
  })
  
  # Testo risultati + massive save
  output$results_text <- renderUI({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    
    if (input$mode == "single") {
      div(
        p("Sono stati calcolati i punteggi per i blocchi selezionati."),
        p("È possibile scaricare un report Word con il profilo delle scale.")
      )
    } else {
      isolate({
        wide <- res %>%
          tidyr::pivot_wider(names_from = Scala, values_from = Punteggio)
        wide$subject_id <- input$subject_id
        wide$timestamp  <- as.character(Sys.time())
        cols <- c("subject_id","timestamp",
                  setdiff(names(wide), c("subject_id","timestamp")))
        wide <- wide[, cols]
        
        file_csv <- "results_massive.csv"
        if (file.exists(file_csv)) {
          old <- read.csv(file_csv, check.names = FALSE, stringsAsFactors = FALSE)
          all_cols <- union(names(old), names(wide))
          for (nm in setdiff(all_cols, names(old))) old[[nm]] <- NA
          for (nm in setdiff(all_cols, names(wide))) wide[[nm]] <- NA
          old  <- old[, all_cols]
          wide <- wide[, all_cols]
          new_all <- rbind(old, wide)
          write.csv(new_all, file_csv, row.names = FALSE)
        } else {
          write.csv(wide, file_csv, row.names = FALSE)
        }
      })
      div(
        p("I punteggi delle scale sono stati salvati nel file"),
        tags$code("results_massive.csv"),
        p("Può continuare con un altro partecipante ricominciando la somministrazione.")
      )
    }
  })
  
  # Link di ritorno a Qualtrics (se presente)
  output$return_link <- renderUI({
    ru <- return_url()
    if (is.null(ru) || !nzchar(ru)) return(NULL)
    
    id <- input$subject_id
    href <- if (nzchar(id)) {
      paste0(ru, if (grepl("\\?", ru)) "&" else "?", "id=", URLencode(id, reserved = TRUE))
    } else {
      ru
    }
    
    tags$p(
      style = "margin-top:10px;",
      tags$a("Torna al questionario", href = href, target = "_blank",
             class = "btn btn-link")
    )
  })
  
  # Tabella risultati
  output$mini_table <- renderTable({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    Nome   <- SCALE_REPORT_LABEL[res$Scala]
    Area   <- FS_TO_AREA[res$Scala]
    Gruppo <- FS_TO_GROUP[res$Scala]
    data.frame(
      Area      = Area,
      Gruppo    = Gruppo,
      Scala     = Nome,
      Punteggio = res$Punteggio,
      stringsAsFactors = FALSE
    )
  }, digits = 2)
  
  # Grafico
  output$hist_plot <- renderPlot({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    Nome <- SCALE_REPORT_LABEL[res$Scala]
    res$Nome <- factor(Nome, levels = Nome)
    ggplot(res, aes(x = Nome, y = Punteggio, fill = Nome)) +
      geom_col() +
      theme_minimal(base_size = 12) +
      labs(x = NULL, y = "Punteggio stimato") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5,5,5,5)
      )
  })
  
  # Download report Word
  output$download_ui <- renderUI({
    res <- results_scores()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    if (input$mode == "single") {
      downloadButton("dl_report", "Scarica report Word")
    } else {
      NULL
    }
  })
  
  output$dl_report <- downloadHandler(
    filename = function() {
      id <- if (nzchar(input$subject_id)) input$subject_id else "report"
      paste0("report_", id, ".docx")
    },
    content = function(file) {
      res <- results_scores()
      req(nrow(res) > 0)
      res$Area   <- FS_TO_AREA[res$Scala]
      res$Gruppo <- FS_TO_GROUP[res$Scala]
      res$Nome   <- SCALE_REPORT_LABEL[res$Scala]
      res_for_report <- res[, c("Area","Gruppo","Nome","Punteggio")]
      
      params_list <- list(
        scores     = res_for_report,
        subject_id = input$subject_id
      )
      tpl <- tempfile(fileext = ".Rmd")
      cat(report_template_text, file = tpl)
      
      tmpdir  <- tempdir()
      outfile <- file.path(tmpdir, "report.docx")
      rmarkdown::render(
        input         = tpl,
        output_format = "word_document",
        output_file   = basename(outfile),
        output_dir    = tmpdir,
        params        = params_list,
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
      file.copy(outfile, file, overwrite = TRUE)
    }
  )
  
  # RESET / EXIT
  confirm_exit_reset <- function() {
    showModal(modalDialog(
      title = "Vuoi uscire o ricominciare?",
      footer = tagList(
        modalButton("Annulla"),
        actionButton("do_reset", "Ricomincia", class = "btn-warning"),
        actionButton("do_exit",  "Esci", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  }
  
  observeEvent(input$exit_or_reset_mid,    confirm_exit_reset())
  observeEvent(input$exit_or_reset_bottom, confirm_exit_reset())
  
  observeEvent(input$do_reset, {
    removeModal()
    selected_scales(character())
    current_scale_index(NA_integer_)
    last_group(NULL)
    in_intro(TRUE)
    Leaves_current(NULL)
    answers(list())
    asked(character())
    pending_choice(NULL)
    results_scores(
      data.frame(Scala = character(), Punteggio = numeric(), stringsAsFactors = FALSE)
    )
    show("page_setup")
    hide("page_questions")
    hide("page_results")
  })
  
  observeEvent(input$do_exit, {
    removeModal()
    stopApp()
  })
}

# ============================================================
# TEMPLATE Rmd per il report Word (costruito con paste)
# ============================================================
report_template_text <- paste(
  "---",
  "title: \"Report adattivo multi-scala\"",
  "params:",
  "  scores: !r data.frame(Area=character(), Gruppo=character(), Nome=character(), Punteggio=numeric())",
  "  subject_id: \"\"",
  "output:",
  "  word_document:",
  "    df_print: default",
  "---",
  "",
  "## Informazioni generali",
  "",
  "**ID partecipante:** `r ifelse(nchar(params$subject_id) > 0, params$subject_id, \"—\")`  ",
  "**Data/ora report:** `r as.character(Sys.time())`",
  "",
  "## Punteggi per scala",
  "",
  "```{r, echo=FALSE, message=FALSE, warning=FALSE}",
  "knitr::kable(",
  "  params$scores,",
  "  digits = 2,",
  "  col.names = c(\"Area\", \"Gruppo\", \"Scala\", \"Punteggio stimato\")",
  ")",
  "library(ggplot2)",
  "dat <- params$scores",
  "dat$Nome <- factor(dat$Nome, levels = dat$Nome)",
  "ggplot(dat, aes(x = Nome, y = Punteggio, fill = Nome)) +",
  "  geom_col() +",
  "  theme_minimal(base_size = 12) +",
  "  labs(x = NULL, y = \"Punteggio stimato\") +",
  "  theme(legend.position = \"none\",",
  "        axis.text.x = element_text(angle = 45, hjust = 1),",
  "        plot.margin = margin(5,5,5,5))",
  "```",
  sep = "\n"
)

shinyApp(ui, server)
