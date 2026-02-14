# app.R ---------------------------------------------------------------
# LERS Shiny — Single + Builder (Massive) — robust local run
# - Builder generates reusable link with query params for participants.
# - Reverse-coding item-by-item from dictionary column: reverse
# - Single mode shows interpretation box
# - No scale acronyms shown in UI (only labels)
# - Scale definitions under each checkbox (DEF_IT / DEF_EN)
# - Fix duplicated factor level in plot (make.unique)
# - Robust scale selection via per-scale checkbox inputs

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

set.seed(1234)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# APP DIR + FILE PICKER
# ============================================================
get_app_dir <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) return(dirname(normalizePath(of, winslash = "/")))
  }
  ad <- tryCatch(getShinyOption("appDir", NULL), error = function(e) NULL)
  if (!is.null(ad) && nzchar(ad)) return(normalizePath(ad, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

APP_DIR <- get_app_dir()

pick_xlsx <- function(candidates, pattern_fallback = NULL, label = "File") {
  full <- file.path(APP_DIR, candidates)
  ok <- file.exists(full)
  if (any(ok)) return(full[which(ok)[1]])
  if (!is.null(pattern_fallback)) {
    m <- list.files(APP_DIR, pattern = pattern_fallback, ignore.case = TRUE, full.names = TRUE)
    if (length(m) > 0) return(m[1])
  }
  stop(sprintf(
    "%s non trovato nella cartella app (atteso uno tra: %s). File presenti: %s",
    label,
    paste(candidates, collapse = " / "),
    paste(list.files(APP_DIR, pattern="\\.(xlsx|xlsm|xls)$", ignore.case=TRUE), collapse=", ")
  ))
}

# ============================================================
# FILES (same folder as app)
# ============================================================
RULES_XLSX <- pick_xlsx(
  c("LERS_summary_scales6.xlsx"),
  pattern_fallback = "LERS_.*summary.*scales.*\\.(xlsx|xlsm|xls)$",
  label = "File regole"
)

LEAF_INFO_XLSX <- pick_xlsx(
  c("leaf_local_regressions_LERS_shrink6.xlsx"),
  pattern_fallback = "leaf_local_regressions.*\\.(xlsx|xlsm|xls)$",
  label = "File leaf"
)

DICT_XLSX <- pick_xlsx(
  c("dictionary_scale_items6.xlsx"),
  pattern_fallback = "dictionary.*items.*\\.(xlsx|xlsm|xls)$",
  label = "File dizionario"
)

META_XLSX <- pick_xlsx(
  c("scale_metadata_LERS6.xlsx"),
  pattern_fallback = "scale_metadata.*\\.(xlsx|xlsm|xls)$",
  label = "File metadata"
)

DICT_SHEET <- "items_dictionary"
LEAF_SHEET <- "leaf_LERS_all"
META_SHEET <- "scales_metadata"

# ============================================================
# I18N TEXT
# ============================================================
UI_TEXT <- list(
  it = list(
    title          = "Somministrazione adattiva",
    mode_title     = "Scegli la modalità",
    mode_single    = "Somministrazione singola",
    mode_builder   = "Raccolta massiva (builder)",
    subject_id     = "ID partecipante (opzionale):",
    select_msg     = "Seleziona una o più scale (per area):",
    start_btn      = "Inizia",
    builder_title  = "Builder (raccolta massiva)",
    builder_help   = "Seleziona le scale e genera un link riutilizzabile per i partecipanti.",
    opt_require_id = "Richiedi ID partecipante",
    opt_show_res   = "Mostra risultati al partecipante",
    opt_webhook    = "Webhook (Google Sheet / Qualtrics endpoint)",
    gen_link       = "Genera link",
    link_ready     = "Link pronto (copia e incolla):",
    embed_hint     = "Suggerimento: in Qualtrics usa un link-out (External URL). In Google Forms di solito si usa link-out (non iframe).",
    done_title     = "Somministrazione completata",
    dl_csv         = "Scarica CSV risultati",
    scores_ready   = "Punteggi calcolati.",
    exit_reset     = "Esci o ricomincia",
    warn_select    = "Seleziona almeno una scala prima di iniziare.",
    end_page       = "Somministrazione terminata",
    close_page     = "Può ora chiudere questa pagina.",
    participant_mode = "Modalità partecipante (da link).",
    scales_selected = "Scale selezionate:"
  ),
  en = list(
    title          = "Adaptive assessment",
    mode_title     = "Choose mode",
    mode_single    = "Single administration",
    mode_builder   = "Batch collection (builder)",
    subject_id     = "Participant ID (optional):",
    select_msg     = "Select one or more scales (by area):",
    start_btn      = "Start",
    builder_title  = "Builder (batch collection)",
    builder_help   = "Select scales and generate a reusable participant link.",
    opt_require_id = "Require participant ID",
    opt_show_res   = "Show results to participant",
    opt_webhook    = "Webhook (Google Sheet / Qualtrics endpoint)",
    gen_link       = "Generate link",
    link_ready     = "Link ready (copy/paste):",
    embed_hint     = "Tip: in Qualtrics use a link-out (External URL). In Google Forms you typically use link-out (not iframe).",
    done_title     = "Assessment completed",
    dl_csv         = "Download CSV results",
    scores_ready   = "Scores computed.",
    exit_reset     = "Exit or restart",
    warn_select    = "Select at least one scale before starting.",
    end_page       = "Assessment ended",
    close_page     = "You can now close this page.",
    participant_mode = "Participant mode (from link).",
    scales_selected = "Selected scales:"
  )
)
txt <- function(lang, key) UI_TEXT[[lang]][[key]]

# ============================================================
# RESPONSE CONFIG
# ============================================================
DEFAULT_CFG <- list(
  values       = 1:5,
  labels_it    = c("Per niente", "Poco", "Abbastanza", "Molto", "Moltissimo"),
  labels_en    = c("Not at all", "A little", "Somewhat", "Much", "Very much"),
  instructions_it = "Per ciascuna richiesta lavorativa indichi quanto la rappresenta rispetto al suo lavoro.",
  instructions_en = "For each work request, indicate how much it describes you with respect to your work."
)

SELF_LABELS_7_IT <- c("Per nulla capace","Poco capace","Piuttosto poco capace","Mediamente capace","Abbastanza capace","Molto capace","Del tutto capace")
SELF_LABELS_7_EN <- c("Not at all capable","Slightly capable","Rather slightly capable","Moderately capable","Fairly capable","Very capable","Fully capable")

FREQ5_IT <- c("Mai o quasi mai","Raramente","Qualche volta","Abbastanza spesso","Molto spesso o sempre")
FREQ5_EN <- c("Never or almost never","Rarely","Sometimes","Quite often","Very often or always")

FREQ4_IT <- c("Mai o quasi mai","Raramente","Qualche volta","Spesso")
FREQ4_EN <- c("Never or almost never","Rarely","Sometimes","Often")

AGREE5_IT <- c("Per nulla d'accordo","Poco d'accordo","Mediamente d'accordo","Molto d'accordo","Del tutto d'accordo")
AGREE5_EN <- c("Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree")

DIST4_IT <- c("Mai","Raramente","A volte","Spesso")
DIST4_EN <- c("Never","Rarely","Sometimes","Often")

IRRIT6_IT <- c(
  "Completamente vero per me","In buona misura vero per me","In una certa misura vero per me",
  "In una certa misura falso per me","In buona misura falso per me","Completamente falso per me"
)
IRRIT6_EN <- c(
  "Completely true for me","Largely true for me","Somewhat true for me",
  "Somewhat false for me","Largely false for me","Completely false for me"
)

NAQ5_IT <- c("Mai o quasi mai","Raramente","Qualche volta","Una volta alla settimana","Più volte durante la settimana")
NAQ5_EN <- c("Never or almost never","Rarely","Sometimes","Once a week","Several times a week")

OVERRIDE_CFG <- list(
  ICAWS_FS = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Consideri alcuni eventi che possono accadere nel lavoro. Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?",
                  instructions_en="Consider the following events that may occur at work. How often has each event happened in your current job?"),
  QWI_FS   = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?",
                  instructions_en="How often has each event happened in your current job?"),
  OCS_FS   = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Quante volte trova difficile o impossibile fare il suo lavoro a causa degli eventi indicati di seguito?",
                  instructions_en="How often is it difficult or impossible to do your job because of the events listed below?"),
  EQUIT_FS = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Indichi con quale frequenza si verificano nella sua organizzazione.",
                  instructions_en="Indicate how often they occur in your organization."),
  FAS_FS   = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Indichi con quale frequenza deve chiedere l’autorizzazione per svolgere le seguenti attività.",
                  instructions_en="Indicate how often you must ask for permission to do the following activities."),
  AFFpos_FS= list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Indichi quanto gli aspetti del suo attuale lavoro l'hanno fatta sentire nel modo descritto negli ultimi 30 giorni.",
                  instructions_en="Indicate how often work made you feel this way over the past 30 days."),
  AFFneg_FS= list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Indichi quanto gli aspetti del suo attuale lavoro l'hanno fatta sentire nel modo descritto negli ultimi 30 giorni.",
                  instructions_en="Indicate how often work made you feel this way over the past 30 days."),
  DIST_FS  = list(values=1:4, labels_it=DIST4_IT, labels_en=DIST4_EN,
                  instructions_it="Negli ultimi 6 mesi, con quale frequenza le è capitato di avvertire ciascuno dei seguenti disturbi?",
                  instructions_en="Over the past 6 months, how often have you experienced each symptom?"),
  DM_FS    = list(values=1:5, labels_it=AGREE5_IT, labels_en=AGREE5_EN,
                  instructions_it="Esprima il suo grado di accordo con le seguenti affermazioni.",
                  instructions_en="Indicate your level of agreement with the statements."),
  JCQ_Demand_FS  = list(values=1:4, labels_it=FREQ4_IT, labels_en=FREQ4_EN,
                        instructions_it="Indichi quanto spesso le capitano le situazioni indicate.",
                        instructions_en="Indicate how often the situations occur."),
  JCQ_Support_FS = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                        instructions_it="Indichi quanto spesso riceve il supporto descritto.",
                        instructions_en="Indicate how often you receive the support described."),
  SELF_EffLav_FS = list(values=1:7, labels_it=SELF_LABELS_7_IT, labels_en=SELF_LABELS_7_EN,
                        instructions_it="Indichi quanto si sente capace di mettere in atto il comportamento descritto.",
                        instructions_en="Indicate how capable you feel of performing the described behavior."),
  JDI_FS   = list(values=c(0,1), labels_it=c("No","Sì"), labels_en=c("No","Yes"),
                  instructions_it="Indichi \"Sì\" se descrive il suo lavoro nella maggior parte dei casi, \"No\" altrimenti.",
                  instructions_en="Mark \"Yes\" if it describes your job most of the time, otherwise \"No\"."),
  CWB_O_FS = list(values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Indichi con quale frequenza si verificano nella sua attuale occupazione.",
                  instructions_en="Indicate how often they occurred in your current job."),
  IRRIT_FS = list(values=1:6, labels_it=IRRIT6_IT, labels_en=IRRIT6_EN,
                  instructions_it="Selezioni l'opzione che meglio rispecchia la sua prima reazione.",
                  instructions_en="Select the option that best reflects your first reaction."),
  NAQ_work_FS = list(values=1:5, labels_it=NAQ5_IT, labels_en=NAQ5_EN,
                     instructions_it="Indichi con quale frequenza ognuna di tali situazioni è accaduta nel suo attuale lavoro.",
                     instructions_en="Indicate how often each situation has occurred in your current job.")
)

get_scale_cfg <- function(scale_fs, lang) {
  cfg <- OVERRIDE_CFG[[scale_fs]]
  if (is.null(cfg)) cfg <- DEFAULT_CFG
  values <- cfg$values %||% DEFAULT_CFG$values
  labels <- if (lang == "en") (cfg$labels_en %||% DEFAULT_CFG$labels_en) else (cfg$labels_it %||% DEFAULT_CFG$labels_it)
  instructions <- if (lang == "en") (cfg$instructions_en %||% DEFAULT_CFG$instructions_en) else (cfg$instructions_it %||% DEFAULT_CFG$instructions_it)
  list(values=values, labels=labels, instructions=instructions)
}

# ============================================================
# LOAD METADATA
# ============================================================
META <- readxl::read_xlsx(META_XLSX, sheet = META_SHEET)
req_meta <- c("scale_FS","area","group","label_it","order_area","order_group","order_scale")
if (!all(req_meta %in% names(META))) stop("META: mancano colonne: ", paste(setdiff(req_meta, names(META)), collapse=", "))

META <- META %>%
  mutate(across(all_of(req_meta), as.character)) %>%
  mutate(
    order_area  = as.numeric(order_area),
    order_group = as.numeric(order_group),
    order_scale = as.numeric(order_scale)
  ) %>%
  arrange(order_area, order_group, order_scale)

if (!"area_en"  %in% names(META)) META$area_en  <- META$area
if (!"group_en" %in% names(META)) META$group_en <- META$group
if (!"label_en" %in% names(META)) META$label_en <- META$label_it

# definitions (DEF_IT / DEF_EN)
if (!"DEF_IT" %in% names(META)) META$DEF_IT <- ""
if (!"DEF_EN" %in% names(META)) META$DEF_EN <- META$DEF_IT

# ============================================================
# LOAD DICTIONARY (IT + optional EN + reverse)
# ============================================================
DICT <- readxl::read_xlsx(DICT_XLSX, sheet = DICT_SHEET)
req_dic <- c("fs_var","Item","Text_IT")
if (!all(req_dic %in% names(DICT))) stop("DICT: mancano colonne: ", paste(setdiff(req_dic, names(DICT)), collapse=", "))

if (!"Text_EN" %in% names(DICT)) DICT$Text_EN <- NA_character_

# compat: reverse vs Reverse
if (!"reverse" %in% names(DICT) && "Reverse" %in% names(DICT)) DICT$reverse <- DICT$Reverse
if (!"reverse" %in% names(DICT)) DICT$reverse <- ""

DICT <- DICT %>%
  mutate(
    fs_var   = as.character(fs_var),
    Item     = trimws(as.character(Item)),
    Text_IT  = as.character(Text_IT),
    Text_EN  = as.character(Text_EN),
    reverse  = tolower(trimws(as.character(reverse)))
  ) %>%
  filter(!is.na(fs_var), fs_var != "", !is.na(Item), Item != "")

# ============================================================
# LOAD LEAF MODELS
# ============================================================
LEAF_RAW <- readxl::read_xlsx(LEAF_INFO_XLSX, sheet = LEAF_SHEET)
req_leaf <- c("scale_FS","leaf_id","intercept","coef_str")
if (!all(req_leaf %in% names(LEAF_RAW))) stop("LEAF: mancano colonne: ", paste(setdiff(req_leaf, names(LEAF_RAW)), collapse=", "))

LEAF_RAW <- LEAF_RAW %>%
  mutate(
    scale_FS  = as.character(scale_FS),
    leaf_id   = as.integer(leaf_id),
    intercept = as.numeric(intercept),
    coef_str  = ifelse(is.na(coef_str), "", as.character(coef_str))
  )

parse_coef_str <- function(s) {
  s <- trimws(s)
  if (is.na(s) || s == "") return(numeric(0))
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  if (!length(parts)) return(numeric(0))
  nm  <- sub("=.*$", "", parts)
  rhs <- sub("^.*=", "", parts)
  rhs <- gsub(",", ".", rhs, fixed = TRUE)
  vals <- suppressWarnings(as.numeric(rhs))
  keep <- !is.na(vals) & nm != ""
  if (!any(keep)) return(numeric(0))
  vals <- vals[keep]; nm <- nm[keep]
  names(vals) <- nm
  vals
}

LEAF_TBL <- LEAF_RAW %>%
  mutate(
    key   = paste0(scale_FS, "__", leaf_id),
    betas = lapply(coef_str, parse_coef_str)
  ) %>%
  select(key, scale_FS, leaf_id, intercept, betas)

LEAF_LIST <- setNames(
  lapply(seq_len(nrow(LEAF_TBL)), function(i) {
    list(intercept = LEAF_TBL$intercept[i], betas = LEAF_TBL$betas[[i]])
  }),
  LEAF_TBL$key
)

# ============================================================
# RULES
# ============================================================
read_rules_for_scale <- function(scale_fs) {
  sheet_name <- paste0("rules_", sub("_FS$", "", scale_fs))
  sh <- readxl::excel_sheets(RULES_XLSX)
  if (!sheet_name %in% sh) stop("Nel file regole manca il foglio: ", sheet_name, " (scala ", scale_fs, ")")
  df <- readxl::read_xlsx(RULES_XLSX, sheet = sheet_name)
  if (!all(c("leaf_id","rule") %in% names(df))) stop("Nel foglio ", sheet_name, " mancano 'leaf_id' e/o 'rule'.")
  if (!"depth" %in% names(df)) df$depth <- 0
  if (!"n_branch_vars" %in% names(df)) df$n_branch_vars <- 0
  df %>%
    filter(!is.na(rule), rule != "") %>%
    mutate(
      leaf_id = as.integer(leaf_id),
      rule    = as.character(rule),
      depth   = as.numeric(depth),
      n_branch_vars = as.numeric(n_branch_vars)
    )
}

parse_rule_conditions <- function(rule_str) {
  if (is.na(rule_str) || trimws(rule_str) == "" || rule_str == "TRUE") return(character(0))
  parts <- stringr::str_split(rule_str, "&")[[1]]
  parts <- stringr::str_trim(parts)
  parts[parts != ""]
}

parse_cond <- function(cond_str) {
  cond_str <- gsub("`", "", trimws(cond_str))
  rx <- "^(\\w+)\\s*(<=|>=|<|>|==|=)\\s*([-+]?[0-9]*\\.?[0-9]+)$"
  m <- stringr::str_match(cond_str, rx)
  if (any(is.na(m))) return(NULL)
  list(var = m[2], op = m[3], cut = suppressWarnings(as.numeric(m[4])))
}

eval_cond <- function(cond_str, answers_named) {
  pc <- parse_cond(cond_str)
  if (is.null(pc)) return(TRUE)
  v <- pc$var
  if (!v %in% names(answers_named)) return(TRUE)
  ans <- answers_named[[v]]
  if (!is.finite(ans)) return(TRUE)
  cut <- pc$cut
  op  <- pc$op
  switch(op,
         "<"  = ans <  cut,
         "<=" = ans <= cut,
         ">"  = ans >  cut,
         ">=" = ans >= cut,
         "==" = ans == cut,
         "="  = ans == cut,
         TRUE)
}

compatible_leaf <- function(rule_str, answers_named) {
  conds <- parse_rule_conditions(rule_str)
  if (!length(conds)) return(TRUE)
  for (cnd in conds) if (!eval_cond(cnd, answers_named)) return(FALSE)
  TRUE
}

filter_leaves <- function(Leaves, answers_named) {
  idx <- vapply(Leaves$rule, compatible_leaf, logical(1), answers_named = answers_named)
  Leaves[idx, , drop = FALSE]
}

choose_route_var <- function(Leaves_sub, answers_named) {
  if (is.null(Leaves_sub) || nrow(Leaves_sub) <= 1) return(NA_character_)
  cond_lists <- lapply(Leaves_sub$rule, parse_rule_conditions)
  maxlen <- max(lengths(cond_lists))
  if (maxlen == 0) return(NA_character_)
  
  for (k in seq_len(maxlen)) {
    kth <- vapply(cond_lists, function(x) if (length(x) >= k) x[[k]] else NA_character_, character(1))
    kth <- kth[!is.na(kth) & kth != ""]
    if (!length(kth)) next
    
    if (length(unique(kth)) > 1) {
      vars <- vapply(kth, function(cs) {
        pc <- parse_cond(cs)
        if (is.null(pc)) NA_character_ else pc$var
      }, character(1))
      vars <- vars[!is.na(vars) & vars != ""]
      vars <- vars[!(vars %in% names(answers_named))]
      if (!length(vars)) next
      tb <- sort(table(vars), decreasing = TRUE)
      return(names(tb)[1])
    } else {
      pc <- parse_cond(unique(kth)[1])
      if (!is.null(pc)) {
        v <- pc$var
        if (!(v %in% names(answers_named))) return(v)
      }
    }
  }
  NA_character_
}

choose_best_leaf <- function(Leaves_sub) {
  if (is.null(Leaves_sub) || nrow(Leaves_sub) == 0) return(NA_integer_)
  Leaves_sub$depth[is.na(Leaves_sub$depth)] <- 0
  Leaves_sub$n_branch_vars[is.na(Leaves_sub$n_branch_vars)] <- 0
  Leaves_sub <- Leaves_sub[order(-Leaves_sub$depth, -Leaves_sub$n_branch_vars, Leaves_sub$leaf_id), , drop=FALSE]
  Leaves_sub$leaf_id[1]
}

# ============================================================
# AVAILABLE SCALES (intersection)
# ============================================================
rule_sheets <- readxl::excel_sheets(RULES_XLSX)

META_AV <- META %>%
  filter(scale_FS %in% unique(LEAF_RAW$scale_FS)) %>%
  filter(paste0("rules_", sub("_FS$","", scale_FS)) %in% rule_sheets) %>%
  arrange(order_area, order_group, order_scale) %>%
  filter(!(group == "Autoefficacia" & scale_FS != "SELF_EffLav_FS")) %>%
  as_tibble()

# ============================================================
# Helpers
# ============================================================
reverse_code_value <- function(val, values_vec) {
  if (is.null(values_vec) || !length(values_vec) || is.null(val) || !is.finite(val)) return(val)
  pos <- match(val, values_vec)
  if (!is.na(pos)) return(rev(values_vec)[pos])
  mn <- suppressWarnings(min(values_vec, na.rm = TRUE))
  mx <- suppressWarnings(max(values_vec, na.rm = TRUE))
  if (is.finite(mn) && is.finite(mx)) return((mx + mn) - val)
  val
}

is_item_reverse <- function(scale_fs, item) {
  r <- DICT %>% dplyr::filter(fs_var == scale_fs, Item == item) %>% dplyr::slice(1)
  if (nrow(r) == 0) return(FALSE)
  flag <- r$reverse[[1]] %||% ""
  flag <- tolower(trimws(as.character(flag)))
  flag %in% c("x","1","true","t","yes","y")
}

compute_lers_score <- function(scale_fs, leaf_id, answers_named) {
  key <- paste0(scale_fs, "__", leaf_id)
  mod <- LEAF_LIST[[key]]
  if (is.null(mod)) return(NA_real_)
  score <- as.numeric(mod$intercept)
  if (length(mod$betas) == 0) return(score)
  for (nm in names(mod$betas)) {
    ans <- answers_named[[nm]]
    if (!is.null(ans) && is.finite(ans)) score <- score + mod$betas[[nm]] * ans
  }
  score
}

get_base_url <- function(session) {
  proto <- session$clientData$url_protocol %||% "http:"
  host  <- session$clientData$url_hostname %||% "127.0.0.1"
  port  <- session$clientData$url_port %||% ""
  path  <- session$clientData$url_pathname %||% "/"
  port_part <- if (!is.null(port) && nzchar(port)) paste0(":", port) else ""
  paste0(proto, "//", host, port_part, path)
}

get_scale_label <- function(scale_fs, L) {
  row <- META_AV %>% filter(scale_FS == scale_fs) %>% slice(1)
  if (nrow(row) == 0) return(scale_fs)
  if (L == "en") dplyr::coalesce(row$label_en, row$label_it, row$scale_FS) else dplyr::coalesce(row$label_it, row$scale_FS)
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(HTML("
      .box { background:#ffffff; padding:18px; border-radius:10px; border:1px solid #e5e5e5; }
      .center { text-align:center; }
      .btn-big { margin:10px; padding:12px 22px; font-size:18px; }
      .muted { color:#666; font-size:12px; }
      .instr { background:#f5f7fb; padding:10px 14px; border-radius:8px; margin-bottom:12px; }
      .qbox { padding:12px; border-radius:10px; border:1px solid #eee; background:#fafafa; }
      .choice { margin:6px; }
      .linkbox { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; background:#f6f8fa; padding:10px; border-radius:8px; border:1px solid #e1e4e8; }
      .results-box { background:#f8f9fa; padding:14px 16px; border-radius:10px; border:1px solid #e5e5e5; margin:12px 0; }

      .choices-row {
        display: flex;
        flex-wrap: nowrap;
        gap: 7px;
        justify-content: center;
        align-items: stretch;
      }
      .choices-row .btn.choice {
        flex: 0 1 auto;
        padding: 3px 7px;
        font-size: 10px;
        line-height: 1.2;
        white-space: nowrap;
      }
    "))
  ),
  
  shiny::div(id="page_language", class="box", style="max-width:680px; margin:40px auto;",
             shiny::h3("Seleziona la lingua / Select language"),
             shiny::div(class="center",
                        actionButton("choose_it", "ITALIANO", class="btn btn-primary btn-big"),
                        actionButton("choose_en", "ENGLISH", class="btn btn-primary btn-big")
             )
  ),
  
  hidden(shiny::div(id="page_mode", class="box", style="max-width:860px; margin:30px auto;",
                    uiOutput("ui_mode")
  )),
  
  hidden(shiny::div(id="page_setup", class="box", style="max-width:980px; margin:20px auto;",
                    uiOutput("ui_setup")
  )),
  
  hidden(shiny::div(id="page_questions", class="box", style="max-width:980px; margin:20px auto;",
                    uiOutput("ui_questions")
  )),
  
  hidden(shiny::div(id="page_results", class="box", style="max-width:980px; margin:20px auto;",
                    uiOutput("ui_results")
  )),
  
  hidden(shiny::div(id="page_exit", class="box", style="max-width:780px; margin:30px auto;",
                    uiOutput("ui_exit")
  ))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  query <- reactive({
    parseQueryString(session$clientData$url_search %||% "")
  })
  
  lang <- reactiveVal(NULL)
  mode <- reactiveVal(NULL)   # "single" | "builder" | "participant"
  
  participant_scales <- reactiveVal(character())
  participant_require_id <- reactiveVal(FALSE)
  participant_show_results <- reactiveVal(TRUE)
  participant_webhook <- reactiveVal(NULL)
  
  observeEvent(input$choose_it, { lang("it"); hide("page_language"); show("page_mode") }, ignoreInit = TRUE)
  observeEvent(input$choose_en, { lang("en"); hide("page_language"); show("page_mode") }, ignoreInit = TRUE)
  
  output$ui_mode <- renderUI({
    req(lang())
    L <- lang()
    shiny::div(
      shiny::h3(txt(L, "mode_title")),
      shiny::div(class="muted", shiny::p(txt(L, "title"))),
      shiny::div(class="center",
                 actionButton("go_single",  txt(L,"mode_single"),  class="btn btn-success btn-big"),
                 actionButton("go_builder", txt(L,"mode_builder"), class="btn btn-secondary btn-big")
      )
    )
  })
  
  observeEvent(input$go_single,  { mode("single");  hide("page_mode"); show("page_setup") }, ignoreInit = TRUE)
  observeEvent(input$go_builder, { mode("builder"); hide("page_mode"); show("page_setup") }, ignoreInit = TRUE)
  
  observe({
    q <- query()
    if (!is.null(q[["participant"]]) && q[["participant"]] %in% c("1","true","TRUE")) {
      mode("participant")
      if (is.null(lang())) lang(q[["lang"]] %||% "it")
      
      sc_raw <- q[["scales"]] %||% ""
      sc <- unlist(strsplit(sc_raw, ",", fixed = TRUE))
      sc <- trimws(sc)
      sc <- sc[sc %in% META_AV$scale_FS]
      participant_scales(unique(sc))
      
      participant_require_id((q[["require_id"]] %||% "0") %in% c("1","true","TRUE"))
      participant_show_results(!((q[["show_results"]] %||% "1") %in% c("0","false","FALSE")))
      participant_webhook(q[["webhook"]] %||% Sys.getenv("WEBHOOK_URL", ""))
      
      hide("page_language"); hide("page_mode"); show("page_setup")
    }
  })
  
  # ============================================================
  # BUILDER HELP
  # ============================================================
  output$builder_embed_help <- renderUI({
    req(lang())
    L <- lang()
    
    if (L == "en") {
      shiny::div(
        class = "results-box",
        shiny::h4("How to share the link with participants"),
        shiny::tags$ol(
          shiny::tags$li("Select the scales and click ", shiny::tags$b("Generate link"), "."),
          shiny::tags$li("Copy the link and send it to participants (email/WhatsApp) or paste it into your survey platform.")
        ),
        shiny::tags$hr(),
        shiny::h4("Data saving (IMPORTANT)"),
        shiny::tags$ul(
          shiny::tags$li("To save results, paste a URL in the ", shiny::tags$b("Webhook"), " field (e.g., Google Sheet via Apps Script)."),
          shiny::tags$li("If the Webhook field is empty, data are not saved reliably (the server may restart and files may be lost)."),
          shiny::tags$li(shiny::tags$b("➡️ For batch collection it is strongly recommended to use the Webhook.")),
          shiny::tags$li("Example (Google Sheet): paste the Apps Script Web App URL that ends with ", shiny::tags$code("/exec"), ".")
        ),
        shiny::tags$hr(),
        shiny::h4("To include the scales inside an existing survey (e.g., Qualtrics)"),
        shiny::p("Use ", shiny::tags$b("Link Out / Redirect (External URL)"), " and paste the generated link.")
      )
    } else {
      shiny::div(
        class = "results-box",
        shiny::h4("Come condividere il link con i partecipanti"),
        shiny::tags$ol(
          shiny::tags$li("Selezionare le scale e cliccare ", shiny::tags$b("Genera link"), "."),
          shiny::tags$li("Copiare il link e inviarlo ai partecipanti (email/WhatsApp) oppure incollarlo nella piattaforma di survey.")
        ),
        shiny::tags$hr(),
        shiny::h4("Salvataggio dei dati (IMPORTANTE)"),
        shiny::tags$ul(
          shiny::tags$li("Per salvare i risultati, inserire un URL nel campo ", shiny::tags$b("Webhook"), " (es. Google Sheet via Apps Script)."),
          shiny::tags$li("Se il campo Webhook è vuoto, i dati non vengono salvati in modo affidabile (il server può riavviarsi e i file possono andare persi)."),
          shiny::tags$li(shiny::tags$b("➡️ Quindi: per la raccolta massiva è fortemente consigliato usare il Webhook.")),
          shiny::tags$li("Esempio (Google Sheet): incollare l’URL della Web App di Apps Script che termina con ", shiny::tags$code("/exec"), ".")
        ),
        shiny::tags$hr(),
        shiny::h4("Per includere le scale selezionate in una survey già esistente (es. Qualtrics)"),
        shiny::p("Usare ", shiny::tags$b("Link Out / Redirect (External URL)"), " e incollare il link generato.")
      )
    }
  })
  
  # ============================================================
  # SCALE SELECTION UI (ROBUST FIX)
  # ============================================================
  output$area_select_ui <- renderUI({
    req(lang())
    L <- lang()
    
    area_col  <- if (L == "en") "area_en" else "area"
    label_col <- if (L == "en") "label_en" else "label_it"
    def_col   <- if (L == "en") "DEF_EN"  else "DEF_IT"
    
    # ensure we have a data.frame-like object
    mdf <- as.data.frame(META_AV, stringsAsFactors = FALSE)
    
    if (!area_col %in% names(mdf)) return(NULL)
    
    areas <- unique(mdf[[area_col]])
    areas <- areas[!is.na(areas) & nzchar(areas)]
    
    shiny::tagList(lapply(areas, function(a) {
      
      # base subsetting => always a data.frame (fixes $ on atomic vectors)
      subdf <- mdf[mdf[[area_col]] == a, , drop = FALSE]
      if (nrow(subdf) == 0) return(NULL)
      
      # keep ordering
      subdf <- subdf[order(subdf[["order_group"]], subdf[["order_scale"]]), , drop = FALSE]
      
      a_label <- a
      if (L == "it") a_label <- sub("^Domande\\s+", "Richieste ", a_label)
      else          a_label <- sub("^Questions\\s+", "Requests ", a_label)
      
      items_ui <- lapply(seq_len(nrow(subdf)), function(i) {
        fs   <- as.character(subdf[["scale_FS"]][i])
        
        lbl  <- subdf[[label_col]][i]
        if (is.na(lbl) || !nzchar(lbl)) lbl <- subdf[["label_it"]][i]
        lbl <- as.character(lbl)
        
        defn <- ""
        if (def_col %in% names(subdf)) defn <- subdf[[def_col]][i]
        defn <- ifelse(is.na(defn), "", as.character(defn))
        
        id <- paste0("chk_", make.names(fs))
        
        shiny::div(
          style="padding:8px 6px; border-bottom:1px solid #eee;",
          checkboxInput(id, label = lbl, value = FALSE),
          if (nzchar(defn)) shiny::div(class="muted", style="margin-left:26px; margin-top:-6px;", defn) else NULL
        )
      })
      
      shiny::div(
        style="margin-bottom:14px;",
        shiny::tags$strong(a_label),
        shiny::div(style="margin-top:6px; border:1px solid #eee; border-radius:10px; padding:6px 10px;",
                   do.call(shiny::tagList, items_ui)
        )
      )
    }))
  })
  
  # read selected scales from per-scale checkboxes
  get_selected_scales_from_ui <- reactive({
    picks <- character(0)
    for (fs in META_AV$scale_FS) {
      id <- paste0("chk_", make.names(fs))
      if (isTRUE(input[[id]])) picks <- c(picks, fs)
    }
    picks <- unique(picks)
    META_AV %>%
      filter(scale_FS %in% picks) %>%
      arrange(order_area, order_group, order_scale) %>%
      pull(scale_FS)
  })
  
  # ============================================================
  # Setup UI changes by mode
  # ============================================================
  output$ui_setup <- renderUI({
    req(lang(), mode())
    L <- lang()
    
    if (mode() == "builder") {
      shiny::div(
        shiny::h3(txt(L, "builder_title")),
        shiny::p(class="muted", txt(L, "builder_help")),
        uiOutput("builder_embed_help"),
        shiny::tags$hr(),
        shiny::h4(txt(L, "select_msg")),
        uiOutput("area_select_ui"),
        shiny::tags$hr(),
        checkboxInput("opt_require_id", txt(L,"opt_require_id"), value = TRUE),
        checkboxInput("opt_show_results", txt(L,"opt_show_res"), value = FALSE),
        textInput("opt_webhook", txt(L,"opt_webhook"), value = Sys.getenv("WEBHOOK_URL", "")),
        shiny::tags$hr(),
        shiny::div(class="center",
                   actionButton("gen_link_btn", txt(L,"gen_link"), class="btn btn-primary btn-big")
        ),
        shiny::tags$hr(),
        uiOutput("builder_link_ui")
      )
      
    } else if (mode() == "participant") {
      
      sc <- participant_scales()
      if (length(sc) == 0) {
        return(shiny::div(
          shiny::h3(if (L=="en") "Invalid link" else "Link non valido"),
          shiny::p(if (L=="en") "This link does not include valid scales. Go back to Builder and regenerate it." else
            "Questo link non contiene scale valide. Torna al builder e rigenera il link.")
        ))
      }
      
      show_id <- isTRUE(participant_require_id())
      
      sc_labels <- META_AV %>%
        filter(scale_FS %in% sc) %>%
        arrange(order_area, order_group, order_scale) %>%
        mutate(lbl = if (L=="en") dplyr::coalesce(label_en, label_it, scale_FS) else dplyr::coalesce(label_it, scale_FS)) %>%
        pull(lbl)
      
      shiny::div(
        shiny::h3(txt(L, "title")),
        shiny::p(class="muted", txt(L, "participant_mode")),
        if (show_id) textInput("subject_id", txt(L, "subject_id"), "") else NULL,
        shiny::tags$hr(),
        shiny::p(txt(L, "scales_selected")),
        shiny::tags$ul(lapply(sc_labels, function(lbl) shiny::tags$li(lbl))),
        shiny::tags$hr(),
        shiny::div(class="center",
                   actionButton("start_participant", txt(L,"start_btn"), class="btn btn-success btn-big")
        )
      )
      
    } else {
      shiny::div(
        shiny::h3(txt(L, "title")),
        textInput("subject_id", txt(L, "subject_id"), ""),
        shiny::tags$hr(),
        shiny::h4(txt(L, "select_msg")),
        uiOutput("area_select_ui"),
        shiny::tags$hr(),
        shiny::div(class="center",
                   actionButton("start_single", txt(L,"start_btn"), class="btn btn-success btn-big")
        )
      )
    }
  })
  
  output$builder_link_ui <- renderUI({
    req(lang(), mode())
    if (mode() != "builder") return(NULL)
    NULL
  })
  
  observeEvent(input$gen_link_btn, {
    req(lang())
    L <- lang()
    sc <- get_selected_scales_from_ui()
    if (length(sc) == 0) {
      showModal(modalDialog(title=if (L=="en") "Warning" else "Attenzione", txt(L,"warn_select"), easyClose=TRUE))
      return()
    }
    
    base <- get_base_url(session)
    req_id <- if (isTRUE(input$opt_require_id)) "1" else "0"
    show_res <- if (isTRUE(input$opt_show_results)) "1" else "0"
    wh <- input$opt_webhook %||% ""
    
    sc_str <- URLencode(paste(sc, collapse=","), reserved = TRUE)
    wh_str <- URLencode(wh, reserved = TRUE)
    
    link <- paste0(
      base,
      "?participant=1",
      "&lang=", lang(),
      "&scales=", sc_str,
      "&require_id=", req_id,
      "&show_results=", show_res,
      if (nzchar(wh)) paste0("&webhook=", wh_str) else ""
    )
    
    output$builder_link_ui <- renderUI({
      shiny::div(
        shiny::h4(txt(L,"link_ready")),
        shiny::div(class="linkbox", link),
        shiny::p(class="muted", txt(L,"embed_hint"))
      )
    })
  }, ignoreInit = TRUE)
  
  # ============================================================
  # TEST FLOW STATE
  # ============================================================
  selected_scales <- reactiveVal(character())
  current_scale_index <- reactiveVal(NA_integer_)
  in_intro <- reactiveVal(TRUE)
  last_group <- reactiveVal(NA_character_)
  
  Leaves_current <- reactiveVal(NULL)
  answers <- reactiveVal(list())
  asked   <- reactiveVal(character())
  
  results_scores <- reactiveVal(
    data.frame(
      scale_FS=character(),
      score=numeric(),
      leaf_id=integer(),
      n_items_asked=integer(),
      stringsAsFactors = FALSE
    )
  )
  
  current_scale_fs <- reactive({
    sc <- selected_scales()
    idx <- current_scale_index()
    if (is.na(idx) || length(sc) == 0 || idx < 1 || idx > length(sc)) return(NULL)
    sc[[idx]]
  })
  
  get_scale_group <- function(fs) {
    g <- META_AV %>% filter(scale_FS == fs) %>% pull(group)
    if (length(g) == 0) return(NA_character_)
    as.character(g[[1]])
  }
  
  init_scale <- function(fs) {
    df_rules <- read_rules_for_scale(fs)
    Leaves_current(df_rules)
    answers(list())
    asked(character())
    
    g_cur  <- get_scale_group(fs)
    g_prev <- last_group()
    if (!is.na(g_prev) && !is.na(g_cur) && identical(g_prev, g_cur)) in_intro(FALSE) else in_intro(TRUE)
    last_group(g_cur)
  }
  
  Leaves_now <- reactive({
    Lc <- Leaves_current()
    if (is.null(Lc)) return(NULL)
    filter_leaves(Lc, answers())
  })
  
  next_var <- reactive({
    if (isTRUE(in_intro())) return(NA_character_)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NA_character_)
    Ls <- Leaves_now()
    if (is.null(Ls) || nrow(Ls) == 0) return(NA_character_)
    
    v_route <- choose_route_var(Ls, answers())
    if (!is.na(v_route) && nzchar(v_route)) return(v_route)
    
    lid <- if (nrow(Ls) == 1) Ls$leaf_id[1] else choose_best_leaf(Ls)
    if (is.na(lid)) return(NA_character_)
    
    key <- paste0(fs, "__", lid)
    mod <- LEAF_LIST[[key]]
    if (is.null(mod)) return(NA_character_)
    
    req_vars <- names(mod$betas)
    req_vars <- req_vars[!is.na(req_vars) & req_vars != ""]
    missing  <- setdiff(req_vars, names(answers()))
    if (length(missing) == 0) return(NA_character_)
    missing[[1]]
  })
  
  values_reactive <- reactive({
    fs <- current_scale_fs()
    if (is.null(fs)) return(DEFAULT_CFG$values)
    get_scale_cfg(fs, lang())$values
  })
  
  # ============================================================
  # QUESTIONS UI
  # ============================================================
  output$ui_questions <- renderUI({
    req(lang())
    L <- lang()
    shiny::tagList(
      uiOutput("scale_intro_ui"),
      uiOutput("instr_ui"),
      shiny::div(class="qbox",
                 uiOutput("question_ui"),
                 shiny::br(),
                 uiOutput("buttons_ui")
      ),
      shiny::br(),
      shiny::div(class="center",
                 actionButton("exit_or_reset_mid", txt(L,"exit_reset"), class="btn btn-secondary")
      )
    )
  })
  
  output$scale_intro_ui <- renderUI({
    if (!isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NULL)
    L <- lang()
    
    label <- get_scale_label(fs, L)
    instr <- get_scale_cfg(fs, L)$instructions
    
    shiny::div(class="instr",
               shiny::h4(label),
               shiny::p(instr),
               shiny::div(class="center",
                          actionButton("begin_scale", if (L=="en") "Begin" else "Inizia", class="btn btn-primary")
               )
    )
  })
  
  observeEvent(input$begin_scale, { in_intro(FALSE) }, ignoreInit = TRUE)
  
  output$instr_ui <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NULL)
    L <- lang()
    shiny::div(class="instr", get_scale_cfg(fs, L)$instructions)
  })
  
  output$question_ui <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    v  <- next_var()
    if (is.null(fs) || is.na(v)) return(NULL)
    
    L <- lang()
    row <- DICT %>% filter(fs_var == fs, Item == v) %>% slice(1)
    txtq <- v
    if (nrow(row) > 0) {
      t_it <- row$Text_IT %||% NA_character_
      t_en <- row$Text_EN %||% NA_character_
      if (L=="en" && !is.na(t_en) && nzchar(t_en)) txtq <- t_en
      else if (!is.na(t_it) && nzchar(t_it)) txtq <- t_it
    }
    shiny::h3(txtq)
  })
  
  output$buttons_ui <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NULL)
    L <- lang()
    cfg <- get_scale_cfg(fs, L)
    vals <- cfg$values
    labs <- cfg$labels
    k <- min(length(vals), 7)
    
    btns <- lapply(seq_len(k), function(i) {
      lab <- if (!is.null(labs) && length(labs) >= i) labs[[i]] else as.character(vals[[i]])
      actionButton(paste0("resp_", i), lab, class="choice btn btn-primary")
    })
    
    shiny::div(class="choices-row", do.call(shiny::tagList, btns))
  })
  
  finalize_current_scale <- function() {
    fs <- current_scale_fs()
    if (is.null(fs)) return()
    
    Ls <- Leaves_now()
    if (is.null(Ls) || nrow(Ls) == 0) Ls <- Leaves_current()
    if (is.null(Ls) || nrow(Ls) == 0) return()
    
    lid <- if (nrow(Ls) == 1) Ls$leaf_id[1] else choose_best_leaf(Ls)
    score <- compute_lers_score(fs, lid, answers())
    
    res <- results_scores()
    res <- rbind(res, data.frame(
      scale_FS = fs,
      score = as.numeric(score),
      leaf_id = as.integer(lid),
      n_items_asked = length(asked()),
      stringsAsFactors = FALSE
    ))
    results_scores(res)
    
    scvec <- selected_scales()
    idx <- current_scale_index()
    if (!is.na(idx) && idx < length(scvec)) {
      current_scale_index(idx + 1L)
      init_scale(current_scale_fs())
    } else {
      hide("page_questions")
      show("page_results")
    }
  }
  
  process_response <- function(choice_index) {
    if (isTRUE(in_intro())) return()
    
    fs <- current_scale_fs()
    if (is.null(fs)) return()
    
    v <- next_var()
    if (is.na(v)) {
      finalize_current_scale()
      return()
    }
    
    vals <- values_reactive()
    if (length(vals) < choice_index) return()
    
    sel_num <- as.numeric(vals[[choice_index]])
    
    if (is_item_reverse(fs, v) && is.finite(sel_num)) {
      sel_num <- reverse_code_value(sel_num, vals)
    }
    
    a <- answers()
    a[[v]] <- sel_num
    answers(a)
    asked(c(asked(), v))
    
    L_now <- Leaves_now()
    if (is.null(L_now) || nrow(L_now) == 0) {
      finalize_current_scale()
      return()
    }
    
    if (is.na(next_var())) finalize_current_scale()
  }
  
  for (j in 1:7) {
    local({
      idx <- j
      observeEvent(input[[paste0("resp_", idx)]], {
        process_response(idx)
      }, ignoreInit = TRUE)
    })
  }
  
  # ============================================================
  # START BUTTONS
  # ============================================================
  start_assessment <- function(scales_vec) {
    req(lang())
    L <- lang()
    
    scales_vec <- unique(scales_vec)
    if (length(scales_vec) == 0) {
      showModal(modalDialog(title=if (L=="en") "Warning" else "Attenzione", txt(L,"warn_select"), easyClose=TRUE))
      return()
    }
    
    selected_scales(scales_vec)
    current_scale_index(1L)
    last_group(NA_character_)
    results_scores(data.frame(scale_FS=character(), score=numeric(), leaf_id=integer(), n_items_asked=integer(), stringsAsFactors = FALSE))
    init_scale(current_scale_fs())
    
    hide("page_setup")
    show("page_questions")
    hide("page_results")
    hide("page_exit")
  }
  
  observeEvent(input$start_single, {
    sc <- get_selected_scales_from_ui()
    start_assessment(sc)
  }, ignoreInit = TRUE)
  
  observeEvent(input$start_participant, {
    sc <- participant_scales()
    start_assessment(sc)
  }, ignoreInit = TRUE)
  
  # ============================================================
  # RESULTS: interpretation box (single only)
  # ============================================================
  output$interpretation_box <- renderUI({
    req(lang())
    L <- lang()
    if (!identical(mode(), "single")) return(NULL)
    
    if (L == "en") {
      shiny::div(
        class = "results-box",
        shiny::h4("How to read scores"),
        shiny::p("Scores are standardized with respect to the reference sample."),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("0"), " = in line with the sample mean"),
          shiny::tags$li(shiny::tags$b("+1 / −1"), " = slightly above / below the mean"),
          shiny::tags$li(shiny::tags$b("+2 / −2"), " = clearly above / below the mean")
        ),
        shiny::p("In general, positive values indicate higher levels of the measured characteristic and negative values indicate lower levels. Scores farther from 0 indicate increasingly larger deviations from the sample mean (|±2| should be considered meaningful)."),
        shiny::p(shiny::tags$b("Low autonomy/control: "), "for the Low autonomy/control dimension, higher scores indicate lower autonomy (less decision latitude).")
      )
    } else {
      shiny::div(
        class = "results-box",
        shiny::h4("Come leggere i punteggi"),
        shiny::p("I punteggi riportati sono standardizzati rispetto al campione di riferimento."),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("0"), " = in linea con la media del campione"),
          shiny::tags$li(shiny::tags$b("+1 / −1"), " = leggermente sopra / sotto la media"),
          shiny::tags$li(shiny::tags$b("+2 / −2"), " = nettamente sopra / sotto la media")
        ),
        shiny::p("In generale, valori positivi indicano livelli più alti della caratteristica misurata e valori negativi livelli più bassi. Punteggi più lontani da 0 (in positivo o in negativo) indicano uno scostamento via via più marcato rispetto alla media del campione (da ±2 lo scostamento è da considerarsi rilevante)."),
        shiny::p(shiny::tags$b("Bassa autonomia/controllo: "), "per la dimensione Bassa autonomia/controllo, punteggi più alti indicano minore autonomia (minore margine decisionale).")
      )
    }
  })
  
  # ============================================================
  # RESULTS UI
  # ============================================================
  output$ui_results <- renderUI({
    req(lang())
    L <- lang()
    show_res <- TRUE
    if (mode() == "participant") show_res <- isTRUE(participant_show_results())
    
    if (!show_res) {
      shiny::div(
        shiny::h3(txt(L, "end_page")),
        shiny::p(txt(L, "close_page")),
        shiny::div(class="center",
                   actionButton("exit_or_reset_bottom", txt(L,"exit_reset"), class="btn btn-secondary")
        )
      )
    } else {
      shiny::tagList(
        shiny::h3(txt(L, "done_title")),
        shiny::p(txt(L, "scores_ready")),
        uiOutput("interpretation_box"),
        shiny::br(),
        tableOutput("mini_table"),
        shiny::br(),
        downloadButton("dl_csv", txt(L,"dl_csv")),
        shiny::br(), shiny::br(),
        plotOutput("bar_plot", height="280px"),
        shiny::br(),
        shiny::div(class="center",
                   actionButton("exit_or_reset_bottom", txt(L,"exit_reset"), class="btn btn-secondary")
        )
      )
    }
  })
  
  output$mini_table <- renderTable({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    
    out <- res %>%
      left_join(META_AV %>% select(scale_FS, label_it, label_en, order_area, order_group, order_scale),
                by = "scale_FS") %>%
      arrange(order_area, order_group, order_scale) %>%
      transmute(
        subject_id = input$subject_id %||% "",
        label = if (L=="en") dplyr::coalesce(label_en, label_it) else label_it,
        score = score,
        n_items = n_items_asked
      )
    out
  }, digits = 2)
  
  output$bar_plot <- renderPlot({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    dd <- res %>%
      left_join(META_AV %>% select(scale_FS, label_it, label_en, order_area, order_group, order_scale),
                by = "scale_FS") %>%
      arrange(order_area, order_group, order_scale)
    
    dd$label <- if (L=="en") ifelse(is.na(dd$label_en) | !nzchar(dd$label_en), dd$label_it, dd$label_en) else dd$label_it
    
    # FIX: avoid duplicated factor levels
    dd$label_plot <- make.unique(dd$label)
    dd$label_plot <- factor(dd$label_plot, levels = dd$label_plot)
    
    ggplot(dd, aes(x = label_plot, y = score, fill = scale_FS)) +
      geom_col() +
      theme_minimal(base_size = 12) +
      labs(x=NULL, y="Score") +
      theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      id <- input$subject_id %||% ""
      id <- if (nzchar(id)) id else "results"
      paste0("results_", id, ".csv")
    },
    content = function(file) {
      res <- results_scores()
      if (nrow(res) == 0) return()
      L <- lang()
      
      out <- res %>%
        left_join(META_AV %>% select(scale_FS, area, area_en, group, group_en, label_it, label_en),
                  by = "scale_FS") %>%
        transmute(
          subject_id = input$subject_id %||% "",
          timestamp  = as.character(Sys.time()),
          lang = L,
          scale_FS = scale_FS,
          label = if (L=="en") dplyr::coalesce(label_en, label_it) else label_it,
          score = as.numeric(score),
          leaf_id = leaf_id,
          n_items_asked = n_items_asked
        )
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  # ============================================================
  # MASSIVE: send to webhook OR local csv (participant mode)
  # ============================================================
  observeEvent(results_scores(), {
    if (!(mode() %in% c("participant"))) return()
    res <- results_scores()
    if (nrow(res) == 0) return()
    if (length(selected_scales()) != nrow(res)) return()
    
    L <- lang()
    wh <- participant_webhook() %||% ""
    payload <- res %>%
      transmute(scale_FS, score) %>%
      pivot_wider(names_from = scale_FS, values_from = score)
    
    payload$subject_id <- input$subject_id %||% ""
    payload$timestamp  <- as.character(Sys.time())
    payload$lang <- L
    
    file_csv <- file.path(APP_DIR, "results_massive.csv")
    
    if (nzchar(wh)) {
      ok_post <- FALSE
      if (requireNamespace("httr", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)) {
        try({
          r <- httr::POST(
            url = wh,
            body = jsonlite::toJSON(payload, auto_unbox = TRUE),
            encode = "raw",
            httr::add_headers("Content-Type"="application/json")
          )
          ok_post <- httr::status_code(r) >= 200 && httr::status_code(r) < 300
        }, silent = TRUE)
      }
      
      if (!ok_post) {
        wide <- as.data.frame(payload, check.names = FALSE)
        if (file.exists(file_csv)) {
          old <- read.csv(file_csv, check.names = FALSE, stringsAsFactors = FALSE)
          all_cols <- union(names(old), names(wide))
          for (nm in setdiff(all_cols, names(old)))  old[[nm]]  <- NA
          for (nm in setdiff(all_cols, names(wide))) wide[[nm]] <- NA
          old  <- old[, all_cols, drop=FALSE]
          wide <- wide[, all_cols, drop=FALSE]
          write.csv(rbind(old, wide), file_csv, row.names = FALSE)
        } else {
          write.csv(wide, file_csv, row.names = FALSE)
        }
      }
    } else {
      wide <- as.data.frame(payload, check.names = FALSE)
      if (file.exists(file_csv)) {
        old <- read.csv(file_csv, check.names = FALSE, stringsAsFactors = FALSE)
        all_cols <- union(names(old), names(wide))
        for (nm in setdiff(all_cols, names(old)))  old[[nm]]  <- NA
        for (nm in setdiff(all_cols, names(wide))) wide[[nm]] <- NA
        old  <- old[, all_cols, drop=FALSE]
        wide <- wide[, all_cols, drop=FALSE]
        write.csv(rbind(old, wide), file_csv, row.names = FALSE)
      } else {
        write.csv(wide, file_csv, row.names = FALSE)
      }
    }
  }, ignoreInit = TRUE)
  
  # ============================================================
  # Exit / reset
  # ============================================================
  output$ui_exit <- renderUI({
    req(lang())
    L <- lang()
    shiny::div(shiny::h3(txt(L,"end_page")), shiny::p(txt(L,"close_page")))
  })
  
  confirm_exit_reset <- function() {
    L <- lang() %||% "it"
    showModal(modalDialog(
      title = txt(L, "exit_reset"),
      footer = shiny::tagList(
        modalButton(if (L=="en") "Cancel" else "Annulla"),
        actionButton("do_reset", if (L=="en") "Restart" else "Ricomincia", class="btn btn-warning"),
        actionButton("do_exit",  if (L=="en") "Exit" else "Esci", class="btn btn-danger")
      ),
      easyClose = TRUE
    ))
  }
  
  observeEvent(input$exit_or_reset_mid, confirm_exit_reset(), ignoreInit = TRUE)
  observeEvent(input$exit_or_reset_bottom, confirm_exit_reset(), ignoreInit = TRUE)
  
  observeEvent(input$do_reset, {
    removeModal()
    mode(NULL)
    selected_scales(character())
    current_scale_index(NA_integer_)
    last_group(NA_character_)
    Leaves_current(NULL)
    answers(list())
    asked(character())
    in_intro(TRUE)
    results_scores(data.frame(scale_FS=character(), score=numeric(), leaf_id=integer(), n_items_asked=integer(), stringsAsFactors = FALSE))
    
    hide("page_setup"); hide("page_questions"); hide("page_results"); hide("page_exit")
    show("page_mode")
  }, ignoreInit = TRUE)
  
  observeEvent(input$do_exit, {
    removeModal()
    hide("page_setup"); hide("page_questions"); hide("page_results"); show("page_exit")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
