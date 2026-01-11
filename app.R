# app.R ---------------------------------------------------------------
# LERS Shiny (tree-driven routing -> 1 leaf -> ask beta vars -> score)
# Output: CSV only (Word removed)
# BILINGUAL IT/EN:
# - language selection page (ITALIANO / ENGLISH)
# - UI + instructions + results note + scale definitions in selected language
# - items read from dictionary column Text_IT / Text_EN (fallback to IT if EN missing)
#
# FIX (2026-01): IRRIT_FS reverse-coding
# - If IRRIT items were reverse-coded in the analyses / model building,
#   we MUST reverse-code the app answers BEFORE using them for routing + scoring.
#
# UX CHANGE (2026-01):
# - Removed Word report button + rendering (was failing on deploy)
# - Removed "Next" button: answering a choice auto-advances

library(shiny)
library(shinyjs)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(ggplot2)

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
    "%s non trovato nella cartella app (atteso uno tra: %s). File .xlsx/.xlsm presenti: %s",
    label,
    paste(candidates, collapse = " / "),
    paste(list.files(APP_DIR, pattern="\\.(xlsx|xlsm|xls)$", ignore.case=TRUE), collapse=", ")
  ))
}

# ============================================================
# FILES (same folder as app)
# ============================================================
RULES_XLSX <- pick_xlsx(
  c("LERS_summary_scales.xlsx"),
  pattern_fallback = "LERS_.*summary.*scales.*\\.(xlsx|xlsm|xls)$",
  label = "File regole"
)

LEAF_INFO_XLSX <- pick_xlsx(
  c("leaf_local_regressions_LERS_shrink3.xlsx",
    "leaf_local_regressions_LERS_shrink.xlsx",
    "leaf_local_regressions_LERS.xlsx"),
  pattern_fallback = "leaf_local_regressions.*\\.(xlsx|xlsm|xls)$",
  label = "File leaf"
)

DICT_XLSX <- pick_xlsx(
  c("dictionary_scale_items3.xlsx",
    "dictionary_scale_items2.xlsx",
    "dictionary_scale_items.xlsx"),
  pattern_fallback = "dictionary.*items.*\\.(xlsx|xlsm|xls)$",
  label = "File dizionario"
)

META_XLSX <- pick_xlsx(
  c("scale_metadata_LERS4b.xlsx", "scale_metadata_LERS3.xlsx", "scale_metadata_LERS2.xlsx"),
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
    lang_title   = "Seleziona la lingua",
    lang_it_btn  = "ITALIANO",
    lang_en_btn  = "ENGLISH",
    title        = "Somministrazione adattiva",
    mode_label   = "Modalità:",
    mode_choices = c("Somministrazione singola" = "single", "Raccolta massiva" = "massive"),
    subject_id   = "ID partecipante (opzionale):",
    select_msg   = "Seleziona una o più scale (per area):",
    start_btn    = "Inizia",
    exit_reset   = "Esci o ricomincia",
    done_title   = "Somministrazione completata",
    warn_select  = "Seleziona almeno una scala prima di iniziare.",
    saved_massive = "Punteggi salvati in results_massive.csv",
    begin_scale  = "Inizia questa sezione",
    end_page     = "Somministrazione terminata",
    close_page   = "Può ora chiudere questa pagina.",
    dl_csv       = "Scarica CSV risultati",
    scores_ready = "Punteggi calcolati. Puoi scaricare il CSV risultati.",
    area_col     = "Area",
    group_col    = "Gruppo",
    scale_col    = "Scala",
    score_col    = "Punteggio",
    score_axis   = "Punteggio stimato",
    exit_title   = "Vuoi uscire o ricominciare?",
    cancel       = "Annulla",
    restart      = "Ricomincia",
    exit         = "Esci",
    attention    = "Attenzione"
  ),
  en = list(
    lang_title   = "Select language",
    lang_it_btn  = "ITALIANO",
    lang_en_btn  = "ENGLISH",
    title        = "Adaptive assessment",
    mode_label   = "Mode:",
    mode_choices = c("Single administration" = "single", "Batch collection" = "massive"),
    subject_id   = "Participant ID (optional):",
    select_msg   = "Select one or more scales (by area):",
    start_btn    = "Start",
    exit_reset   = "Exit or restart",
    done_title   = "Assessment completed",
    warn_select  = "Select at least one scale before starting.",
    saved_massive = "Scores saved to results_massive.csv",
    begin_scale  = "Start this section",
    end_page     = "Assessment ended",
    close_page   = "You can now close this page.",
    dl_csv       = "Download CSV results",
    scores_ready = "Scores computed. You can download the CSV results.",
    area_col     = "Area",
    group_col    = "Group",
    scale_col    = "Scale",
    score_col    = "Score",
    score_axis   = "Estimated score",
    exit_title   = "Exit or restart?",
    cancel       = "Cancel",
    restart      = "Restart",
    exit         = "Exit",
    attention    = "Warning"
  )
)
txt <- function(lang, key) UI_TEXT[[lang]][[key]]

# ============================================================
# RESPONSE CONFIG / INSTRUCTIONS
# ============================================================
DEFAULT_CFG <- list(
  label        = NULL,
  values       = 1:5,
  labels_it    = c("Per niente", "Poco", "Abbastanza", "Molto", "Moltissimo"),
  labels_en    = c("Not at all", "A little", "Somewhat", "Much", "Very much"),
  instructions_it = "Per ciascuna affermazione indichi quanto la rappresenta rispetto al suo lavoro.",
  instructions_en = "For each statement, indicate how much it describes you with respect to your work."
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
  ICAWS_FS = list(label_it="Aggressioni verbali", label_en="Interpersonal conflict", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Consideri alcuni eventi che possono accadere nel lavoro. Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?",
                  instructions_en="Consider the following events that may occur at work. How often has each event happened in your current job?"),
  QWI_FS   = list(label_it="Carico di lavoro", label_en="Quantitative workload", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Consideri alcuni eventi che possono accadere nel lavoro. Con quale frequenza ognuno di tali eventi è accaduto nel suo attuale lavoro?",
                  instructions_en="Consider events that may occur at work. How often has each event happened in your current job?"),
  OCS_FS   = list(label_it="Stressors organizzativi", label_en="Organizational constraints", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Quante volte trova difficile o impossibile fare il suo lavoro a causa degli eventi indicati di seguito?",
                  instructions_en="How often is it difficult or impossible to do your job because of the events listed below?"),
  EQUIT_FS = list(label_it="Equità organizzativa", label_en="Organizational equity", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Relativamente alle situazioni di seguito descritte indichi con quale frequenza si verificano nella sua organizzazione.",
                  instructions_en="For the situations described below, indicate how often they occur in your organization."),
  FAS_FS   = list(label_it="Autonomia", label_en="Autonomy", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Le affermazioni di seguito presentate descrivono alcune situazioni comuni nel lavoro. Legga attentamente ciascuna frase e indichi con quale frequenza deve chiedere l’autorizzazione per svolgere le seguenti attività nella sua attuale occupazione.",
                  instructions_en="The statements below describe common situations at work. For each one, indicate how often you must ask for permission to carry out the following activities in your current job."),
  AFFpos_FS= list(label_it="Affettività positiva", label_en="Positive affect", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Di seguito sono presentate alcune emozioni che il lavoro può indurre. Indichi quanto gli aspetti del suo attuale lavoro l'hanno fatta sentire nel modo descritto negli ultimi 30 giorni.",
                  instructions_en="Below are emotions that work may elicit. Indicate how often aspects of your current job made you feel this way over the past 30 days."),
  AFFneg_FS= list(label_it="Affettività negativa", label_en="Negative affect", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Di seguito sono presentate alcune emozioni che il lavoro può indurre. Indichi quanto gli aspetti del suo attuale lavoro l'hanno fatta sentire nel modo descritto negli ultimi 30 giorni.",
                  instructions_en="Below are emotions that work may elicit. Indicate how often aspects of your current job made you feel this way over the past 30 days."),
  RCA_Ambig_FS = list(label_it="Ambiguità di ruolo", label_en="Role ambiguity", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                      instructions_it="Le affermazioni di seguito presentate descrivono alcune situazioni comuni nel lavoro. Per ciascuna di esse indichi l'opzione che meglio descrive la sua esperienza nel suo attuale lavoro.",
                      instructions_en="The statements below describe common situations at work. For each, select the option that best describes your experience in your current job."),
  RCA_Conflict_FS = list(label_it="Conflitti di ruolo", label_en="Role conflict", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                         instructions_it="Le affermazioni di seguito presentate descrivono alcune situazioni comuni nel lavoro. Per ciascuna di esse indichi l'opzione che meglio descrive la sua esperienza nel suo attuale lavoro.",
                         instructions_en="The statements below describe common situations at work. For each, select the option that best describes your experience in your current job."),
  
  DIST_FS = list(label_it="Disturbi psicofisici", label_en="Psychophysical symptoms", values=1:4, labels_it=DIST4_IT, labels_en=DIST4_EN,
                 instructions_it="Negli ultimi 6 mesi, con quale frequenza le è capitato di avvertire ciascuno dei seguenti disturbi?",
                 instructions_en="Over the past 6 months, how often have you experienced each of the following symptoms?"),
  
  DM_FS   = list(label_it="Disimpegno morale", label_en="Moral disengagement", values=1:5, labels_it=AGREE5_IT, labels_en=AGREE5_EN,
                 instructions_it="Pensando alla sua esperienza lavorativa, esprima il suo grado di accordo con le seguenti affermazioni.",
                 instructions_en="Thinking about your work experience, indicate your level of agreement with the following statements."),
  
  JCQ_Control_FS = list(label_it="Controllo sul lavoro", label_en="Job control", values=1:4, labels_it=FREQ4_IT, labels_en=FREQ4_EN,
                        instructions_it="Le chiediamo di esprimere il suo parere su vari aspetti della sua vita lavorativa indicando quanto spesso le capitano le situazioni che sono riportate nelle righe seguenti.",
                        instructions_en="Please indicate your opinion about various aspects of your working life by reporting how often the situations below occur."),
  JCQ_Demand_FS  = list(label_it="Richieste del lavoro", label_en="Psychological demands", values=1:4, labels_it=FREQ4_IT, labels_en=FREQ4_EN,
                        instructions_it="Le chiediamo di esprimere il suo parere su vari aspetti della sua vita lavorativa indicando quanto spesso le capitano le situazioni che sono riportate nelle righe seguenti.",
                        instructions_en="Please indicate your opinion about various aspects of your working life by reporting how often the situations below occur."),
  JCQ_Support_FS = list(label_it="Supporto sociale", label_en="Social support", values=1:4, labels_it=FREQ4_IT, labels_en=FREQ4_EN,
                        instructions_it="Indichi quanto spesso riceve il supporto descritto nelle affermazioni seguenti.",
                        instructions_en="Indicate how often you receive the support described in the following statements."),
  
  SELF_EffLav_FS = list(label_it="Autoefficacia lavorativa", label_en="Work self-efficacy", values=1:7, labels_it=SELF_LABELS_7_IT, labels_en=SELF_LABELS_7_EN,
                        instructions_it="Le seguenti affermazioni descrivono comportamenti riferiti all’attività lavorativa. Indichi, per ognuna, quanto si sente capace di mettere in atto il comportamento descritto.",
                        instructions_en="The following statements describe work-related behaviors. For each, indicate how capable you feel of performing the described behavior."),
  
  JDI_FS = list(label_it="Job Descriptive Index", label_en="Job Descriptive Index", values=c(0,1), labels_it=c("No","Sì"), labels_en=c("No","Yes"),
                instructions_it="Pensi al suo lavoro in generale. Indichi \"Sì\" se la parola o frase descrive il suo lavoro nella maggior parte dei casi, \"No\" se non lo descrive.",
                instructions_en="Think about your job in general. Mark \"Yes\" if the word/phrase describes your job most of the time, and \"No\" if it does not."),
  
  CWB_O_FS = list(label_it="Comportamenti controproduttivi", label_en="Counterproductive behaviors", values=1:5, labels_it=FREQ5_IT, labels_en=FREQ5_EN,
                  instructions_it="Relativamente alle situazioni di seguito descritte indichi con quale frequenza si verificano nella sua attuale occupazione.",
                  instructions_en="For the situations described below, indicate how often they have occurred in your current job."),
  
  IRRIT_FS = list(label_it="Irritabilità", label_en="Irritability", values=1:6, labels_it=IRRIT6_IT, labels_en=IRRIT6_EN,
                  instructions_it="Le affermazioni di seguito descrivono alcune situazioni comuni. Non esistono risposte “giuste” o “sbagliate”, la migliore risposta è quella immediata, spontanea. Legga attentamente ciascuna frase, decida se la frase per lei è vera o falsa, quindi selezioni l'opzione che meglio rispecchia la sua prima reazione.",
                  instructions_en="The statements below describe common situations. There are no right or wrong answers; the best response is your immediate, spontaneous one. Read each statement and select the option that best reflects your first reaction."),
  
  NAQ_work_FS = list(label_it="Comportamenti negativi subiti", label_en="Negative acts experienced", values=1:5, labels_it=NAQ5_IT, labels_en=NAQ5_EN,
                     instructions_it="Le seguenti affermazioni descrivono comportamenti riferiti all’attività lavorativa. Legga attentamente ogni affermazione e indichi con quale frequenza ognuna di tali situazioni è accaduta nel suo attuale lavoro.",
                     instructions_en="The following statements describe work-related behaviors. For each statement, indicate how often each situation has occurred in your current job.")
)

get_scale_cfg <- function(scala_fs, lang) {
  cfg <- OVERRIDE_CFG[[scala_fs]]
  if (is.null(cfg)) cfg <- DEFAULT_CFG
  label <- if (lang == "en") (cfg$label_en %||% cfg$label_it %||% scala_fs) else (cfg$label_it %||% scala_fs)
  values <- cfg$values %||% DEFAULT_CFG$values
  labels <- if (lang == "en") (cfg$labels_en %||% DEFAULT_CFG$labels_en) else (cfg$labels_it %||% DEFAULT_CFG$labels_it)
  instructions <- if (lang == "en") (cfg$instructions_en %||% DEFAULT_CFG$instructions_en) else (cfg$instructions_it %||% DEFAULT_CFG$instructions_it)
  list(label = label, values = values, labels = labels, instructions = instructions)
}

# ============================================================
# SCALE DEFINITIONS (ITA/EN)
# ============================================================
SCALE_DEFS_IT <- c(
  SELF_EffLav_FS   = "Autoefficacia lavorativa: percezione di efficacia nel gestire compiti e obiettivi, mantenere l’impegno e affrontare le richieste lavorative anche sotto pressione. Valori più alti indicano maggiore autoefficacia.",
  DM_FS            = "Disimpegno morale: tendenza a giustificare o minimizzare condotte scorrette riducendo il senso di responsabilità personale. Valori più alti indicano maggiore propensione al disimpegno morale.",
  IRRIT_FS         = "Irritabilità: disposizione alla reattività emotiva e all’impulsività aggressiva (facilità a innervosirsi, perdere la pazienza, reagire con rabbia). Valori più alti indicano maggiore irritabilità.",
  CWB_O_FS         = "Comportamenti controproduttivi verso l’organizzazione: frequenza di comportamenti volontari e intenzionali che possono danneggiare l’organizzazione (es. riduzione dell’impegno, violazioni di regole, uso improprio di tempo/risorse). Valori più alti indicano maggiore frequenza di tali comportamenti.",
  JCQ_Support_FS   = "Supporto sociale: percezione di aiuto, collaborazione e disponibilità da parte di colleghi/superiori nel portare a termine il lavoro. Valori più alti indicano maggiore supporto percepito.",
  EQUIT_FS         = "Equità organizzativa: percezione di correttezza ed equità nelle relazioni e nelle dinamiche organizzative (es. distribuzione equa del carico di lavoro). Valori più alti indicano maggiore equità percepita.",
  FAS_FS           = "Autonomia: grado di autonomia decisionale e controllo sul lavoro (es. modifiche a orari/attività). Nota: in questa scala valori più alti indicano minore autonomia (maggiore necessità di chiedere permesso).",
  QWI_FS           = "Carico di lavoro quantitativo: intensità quantitativa delle richieste (quantità di compiti, velocità, urgenze). Valori più alti indicano maggiore carico.",
  JCQ_Demand_FS    = "Richieste psicologiche: livello di pressione e intensità delle richieste mentali nel lavoro (ritmo, concentrazione, richieste cognitive). Valori più alti indicano maggiori richieste.",
  OCS_FS           = "Vincoli/ostacoli organizzativi: presenza di impedimenti che rendono difficile svolgere il lavoro (es. risorse/strumenti inadeguati, procedure, disorganizzazione). Valori più alti indicano più vincoli.",
  RCA_Ambig_FS     = "Ambiguità di ruolo: mancanza di chiarezza su compiti, responsabilità e aspettative. Valori più alti indicano maggiore ambiguità (minore chiarezza).",
  RCA_Conflict_FS  = "Conflitto di ruolo: presenza di richieste incompatibili o contraddittorie. Valori più alti indicano maggiore conflitto di ruolo.",
  ICAWS_FS         = "Conflitto interpersonale: frequenza di interazioni conflittuali sul lavoro (es. discussioni, urla, scortesia/ostilità). Valori più alti indicano maggiore conflitto interpersonale.",
  NAQ_work_FS      = "Esposizione ad atti negativi: frequenza di comportamenti negativi subiti sul lavoro (es. ostilità, isolamento, azioni che ostacolano la possibilità di lavorare in modo efficace). Valori più alti indicano maggiore esposizione.",
  AFFpos_FS        = "Affetto positivo: frequenza di emozioni positive associate al lavoro (es. entusiasmo) nell’ultimo periodo. Valori più alti indicano più affetto positivo.",
  AFFneg_FS        = "Affetto negativo: frequenza di emozioni negative associate al lavoro (es. ansia) nell’ultimo periodo. Valori più alti indicano più affetto negativo.",
  DIST_FS          = "Sintomi psicofisici: frequenza di sintomi fisici e psicologici riportati in un arco temporale recente. Valori più alti indicano sintomi più frequenti.",
  JDI_FS           = "Soddisfazione lavorativa globale: valutazione complessiva del lavoro. Valori più alti indicano maggiore soddisfazione.",
  JCQ_Control_FS   = "Controllo sul lavoro: grado di autonomia/possibilità di decisione e utilizzo di competenze nel lavoro. Valori più alti indicano maggiore controllo percepito."
)

SCALE_DEFS_EN <- c(
  SELF_EffLav_FS   = "Work self-efficacy: perceived ability to manage tasks and goals, sustain effort, and cope with work demands even under pressure. Higher values indicate greater work self-efficacy.",
  DM_FS            = "Moral disengagement: tendency to justify or downplay misconduct by reducing one’s sense of personal responsibility. Higher values indicate a greater propensity for moral disengagement.",
  IRRIT_FS         = "Irritability: a trait-like tendency toward emotional reactivity and aggressive impulsivity (getting easily irritated, losing patience, reacting with anger). Higher values indicate greater irritability.",
  CWB_O_FS         = "Counterproductive behaviors toward the organization: frequency of voluntary and intentional behaviors that may harm the organization (e.g., intentionally reducing effort, rule violations, misuse of time/resources). Higher values indicate more frequent organization-directed counterproductive work behaviors (CWB).",
  JCQ_Support_FS   = "Social support: perceived help, cooperation, and availability from coworkers and supervisors in getting the job done. Higher values indicate greater perceived support.",
  EQUIT_FS         = "Organizational equity: perceived fairness in organizational relationships and dynamics (e.g., fair distribution of workload). Higher values indicate greater perceived equity.",
  FAS_FS           = "Autonomy: degree of decision latitude and control at work (e.g., changing schedules/activities). Note: in this scale, higher values indicate lower autonomy (i.e., a greater need to ask for permission).",
  QWI_FS           = "Quantitative workload: intensity of quantitative demands (amount of work, speed, time pressure). Higher values indicate higher workload.",
  JCQ_Demand_FS    = "Psychological demands: pressure and intensity of mental demands at work (pace, concentration, cognitive demands). Higher values indicate greater demands.",
  OCS_FS           = "Organizational constraints: obstacles that make it difficult to do one’s job (e.g., inadequate resources/equipment, procedures, disorganization). Higher values indicate greater constraints.",
  RCA_Ambig_FS     = "Role ambiguity: lack of clarity about tasks, responsibilities, and expectations. Higher values indicate greater ambiguity (i.e., less clarity).",
  RCA_Conflict_FS  = "Role conflict: incompatible or contradictory requests. Higher values indicate greater role conflict.",
  ICAWS_FS         = "Interpersonal conflict: frequency of conflictual interactions at work (e.g., arguments, yelling, rudeness or hostility). Higher values indicate greater interpersonal conflict.",
  NAQ_work_FS      = "Exposure to negative acts: frequency of negative behaviors experienced at work (e.g., hostility, isolation, actions that undermine effective work). Higher values indicate greater exposure.",
  AFFpos_FS        = "Positive affect: frequency of positive emotions associated with work (e.g., enthusiasm) over the past 30 days. Higher values indicate more positive affect.",
  AFFneg_FS        = "Negative affect: frequency of negative emotions associated with work (e.g., anxiety) over the past 30 days. Higher values indicate more negative affect.",
  DIST_FS          = "Psychophysical symptoms: frequency of reported physical and psychological symptoms over the past 6 months. Higher values indicate more frequent symptoms.",
  JDI_FS           = "Global job satisfaction: overall evaluation of one’s job. Higher values indicate greater satisfaction.",
  JCQ_Control_FS   = "Job control: perceived autonomy/decision latitude and skill discretion at work. Higher values indicate greater perceived control."
)

scale_def <- function(lang, scale_fs) {
  if (lang == "en") (SCALE_DEFS_EN[[scale_fs]] %||% SCALE_DEFS_IT[[scale_fs]] %||% "Definition not available.")
  else (SCALE_DEFS_IT[[scale_fs]] %||% "Definizione non disponibile.")
}

# --- remove duplicated "<label>:" from definition if present
escape_regex <- function(x) gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", x)
clean_definition <- function(def_text, label_text) {
  if (is.null(def_text) || is.na(def_text) || !nzchar(def_text)) return(def_text)
  if (is.null(label_text) || is.na(label_text) || !nzchar(label_text)) return(def_text)
  lab_rx <- escape_regex(trimws(label_text))
  rx <- paste0("^\\s*", lab_rx, "\\s*[:\\-–]\\s*")
  sub(rx, "", def_text, ignore.case = TRUE, perl = TRUE)
}

# ============================================================
# LOAD METADATA
# ============================================================
META <- read_xlsx(META_XLSX, sheet = META_SHEET)
req_meta <- c("scale_FS","area","group","label_it","order_area","order_group","order_scale")
if (!all(req_meta %in% names(META))) {
  stop("META: mancano colonne: ", paste(setdiff(req_meta, names(META)), collapse=", "))
}
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

# ============================================================
# LOAD DICTIONARY (IT + optional EN)
# ============================================================
DICT <- read_xlsx(DICT_XLSX, sheet = DICT_SHEET)
req_dic_min <- c("fs_var","Item","Text_IT")
if (!all(req_dic_min %in% names(DICT))) {
  stop("DICT: mancano colonne: ", paste(setdiff(req_dic_min, names(DICT)), collapse=", "))
}
if (!"Text_EN" %in% names(DICT)) DICT$Text_EN <- NA_character_

DICT <- DICT %>%
  mutate(
    fs_var   = as.character(fs_var),
    Item     = trimws(as.character(Item)),
    Text_IT  = as.character(Text_IT),
    Text_EN  = as.character(Text_EN)
  ) %>%
  filter(!is.na(fs_var), fs_var != "", !is.na(Item), Item != "")

# ============================================================
# LOAD LEAF MODELS
# ============================================================
LEAF_RAW <- read_xlsx(LEAF_INFO_XLSX, sheet = LEAF_SHEET)
req_leaf <- c("scale_FS","leaf_id","intercept","coef_str")
if (!all(req_leaf %in% names(LEAF_RAW))) {
  stop("LEAF: mancano colonne: ", paste(setdiff(req_leaf, names(LEAF_RAW)), collapse=", "))
}
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
    list(intercept = LEAF_TBL$intercept[i],
         betas     = LEAF_TBL$betas[[i]])
  }),
  LEAF_TBL$key
)

# ============================================================
# RULES
# ============================================================
read_rules_for_scale <- function(fs_col) {
  sheet_name <- paste0("rules_", sub("_FS$", "", fs_col))
  sh <- excel_sheets(RULES_XLSX)
  if (!sheet_name %in% sh) stop("Nel file regole manca il foglio: ", sheet_name, " (scala ", fs_col, ")")
  df <- read_xlsx(RULES_XLSX, sheet = sheet_name)
  if (!all(c("leaf_id","rule") %in% names(df))) stop("Nel foglio ", sheet_name, " mancano 'leaf_id' e/o 'rule'.")
  if (!"depth" %in% names(df)) df$depth <- 0
  if (!"n_branch_vars" %in% names(df)) df$n_branch_vars <- 0
  if (!"branch_vars" %in% names(df)) df$branch_vars <- ""
  df %>%
    filter(!is.na(rule), rule != "") %>%
    mutate(
      leaf_id = as.integer(leaf_id),
      rule    = as.character(rule),
      depth   = as.numeric(depth),
      n_branch_vars = as.numeric(n_branch_vars),
      branch_vars   = as.character(branch_vars)
    )
}

parse_rule_conditions <- function(rule_str) {
  if (is.na(rule_str) || trimws(rule_str) == "" || rule_str == "TRUE") return(character(0))
  parts <- str_split(rule_str, "&")[[1]]
  parts <- str_trim(parts)
  parts[parts != ""]
}

parse_cond <- function(cond_str) {
  cond_str <- gsub("`", "", trimws(cond_str))
  rx <- "^(\\w+)\\s*(<=|>=|<|>|==|=)\\s*([-+]?[0-9]*\\.?[0-9]+)$"
  m <- str_match(cond_str, rx)
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
# SCALES AVAILABLE (intersection META, LEAF, RULES)
# + keep only SELF_EffLav_FS inside Autoefficacia
# ============================================================
rule_sheets <- excel_sheets(RULES_XLSX)

META_AV <- META %>%
  filter(scale_FS %in% unique(LEAF_RAW$scale_FS)) %>%
  filter(paste0("rules_", sub("_FS$","", scale_FS)) %in% rule_sheets) %>%
  arrange(order_area, order_group, order_scale) %>%
  filter(!(group == "Autoefficacia" & scale_FS != "SELF_EffLav_FS"))

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .btn-choice { margin:6px; padding:10px 16px; font-size:16px; }
    .panel-instructions { background:#f8f9fa; }
    .results-box { background:#fafafa; padding:12px; border-radius:8px; border:1px solid #e5e5e5; }
    .centered { text-align:center; }
    .instr-bar { background:#f5f7fb; padding:10px 16px; margin-bottom:16px;
                 font-size:18px; color:#003366; border-radius:8px; font-weight:500; }
    .scale-intro-box { background:#eef2f7; padding:16px; border-radius:8px; margin-bottom:16px; }
    .lang-box { background:#ffffff; padding:22px; border-radius:10px; border:1px solid #e5e5e5; max-width:560px; margin:40px auto; }
    .lang-btn { margin:10px; padding:12px 20px; font-size:18px; }
  "))),
  div(
    id = "page_language",
    div(class="lang-box",
        h3("Seleziona la lingua / Select language"),
        div(class="centered",
            actionButton("choose_it", "ITALIANO", class="btn btn-primary lang-btn"),
            actionButton("choose_en", "ENGLISH", class="btn btn-primary lang-btn")
        )
    )
  ),
  hidden(div(id="page_setup", uiOutput("page_setup_ui"))),
  hidden(div(id="page_questions", uiOutput("page_questions_ui"))),
  hidden(div(id="page_results", uiOutput("page_results_ui"))),
  hidden(div(id="page_exit", uiOutput("page_exit_ui")))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  lang <- reactiveVal(NULL)
  
  observeEvent(input$choose_it, { lang("it"); hide("page_language"); show("page_setup") }, ignoreInit = TRUE)
  observeEvent(input$choose_en, { lang("en"); hide("page_language"); show("page_setup") }, ignoreInit = TRUE)
  
  output$page_setup_ui <- renderUI({
    req(lang())
    L <- lang()
    fluidRow(
      column(12,
             wellPanel(class="panel-instructions",
                       h3(txt(L, "title")),
                       radioButtons("mode", txt(L, "mode_label"),
                                    choices = UI_TEXT[[L]]$mode_choices,
                                    selected = "single"),
                       textInput("subject_id", txt(L, "subject_id"), ""),
                       tags$hr(),
                       h4(txt(L, "select_msg")),
                       uiOutput("area_select_ui"),
                       tags$hr(),
                       div(class="centered",
                           actionButton("start_btn", txt(L, "start_btn"), class="btn btn-success"))
             )
      )
    )
  })
  
  output$page_questions_ui <- renderUI({
    req(lang())
    L <- lang()
    tagList(
      fluidRow(column(12, uiOutput("scale_intro"))),
      fluidRow(column(12, uiOutput("instructions_bar"))),
      fluidRow(
        column(12, align="center",
               uiOutput("question"),
               br(),
               uiOutput("buttons"),
               br(),
               actionButton("exit_or_reset_mid", txt(L, "exit_reset"), class="btn btn-secondary")
        )
      )
    )
  })
  
  output$page_results_ui <- renderUI({
    req(lang())
    L <- lang()
    tagList(
      fluidRow(column(12, h3(txt(L, "done_title")),
                      uiOutput("results_note"),
                      uiOutput("selected_scale_defs"),
                      uiOutput("results_text"))),
      fluidRow(
        column(6,
               div(class="results-box", tableOutput("mini_table")),
               br(),
               downloadButton("dl_csv", txt(L, "dl_csv")),
               br(), br(),
               actionButton("exit_or_reset_bottom", txt(L, "exit_reset"), class="btn btn-secondary")
        ),
        column(6, plotOutput("hist_plot", height="260px"))
      )
    )
  })
  
  output$page_exit_ui <- renderUI({
    req(lang())
    L <- lang()
    fluidRow(column(12, h3(txt(L, "end_page")), p(txt(L, "close_page"))))
  })
  
  # --- selection scales by area
  output$area_select_ui <- renderUI({
    req(lang())
    L <- lang()
    
    area_col  <- if (L == "en" && "area_en"  %in% names(META_AV)) "area_en"  else "area"
    label_col <- if (L == "en" && "label_en" %in% names(META_AV)) "label_en" else "label_it"
    
    areas <- unique(META_AV[[area_col]])
    areas <- areas[!is.na(areas) & nzchar(areas)]
    
    tagList(lapply(areas, function(a) {
      sub <- META_AV %>% filter(.data[[area_col]] == a) %>% arrange(order_group, order_scale)
      
      labs <- sub[[label_col]]
      if ("label_it" %in% names(sub)) labs <- ifelse(is.na(labs) | !nzchar(labs), sub$label_it, labs)
      
      choices <- setNames(sub$scale_FS, labs)
      
      checkboxGroupInput(
        inputId = paste0("sel_", make.names(a)),
        label   = strong(a),
        choices = choices
      )
    }))
  })
  
  get_selected_scales <- reactive({
    req(lang())
    L <- lang()
    area_col <- if (L == "en" && "area_en" %in% names(META_AV)) "area_en" else "area"
    
    areas <- unique(META_AV[[area_col]])
    areas <- areas[!is.na(areas) & nzchar(areas)]
    
    picks <- unlist(lapply(areas, function(a) input[[paste0("sel_", make.names(a))]]))
    picks <- unique(picks[!is.na(picks) & nzchar(picks)])
    
    META_AV %>%
      filter(scale_FS %in% picks) %>%
      arrange(order_area, order_group, order_scale) %>%
      pull(scale_FS)
  })
  
  selected_scales <- reactiveVal(character())
  current_scale_index <- reactiveVal(NA_integer_)
  in_intro <- reactiveVal(TRUE)
  last_group <- reactiveVal(NA_character_)
  
  Leaves_current <- reactiveVal(NULL)
  answers <- reactiveVal(list())
  asked   <- reactiveVal(character())
  
  results_scores <- reactiveVal(
    data.frame(Scala=character(), Punteggio=numeric(), leaf_id=integer(),
               n_items_asked=integer(), stringsAsFactors = FALSE)
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
  
  output$scale_intro <- renderUI({
    if (!isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NULL)
    L <- lang()
    cfg <- get_scale_cfg(fs, L)
    
    div(
      class="scale-intro-box",
      h4(cfg$label),
      p(cfg$instructions),
      br(),
      div(class="centered",
          actionButton("begin_scale", txt(L, "begin_scale"), class="btn btn-primary"))
    )
  })
  
  observeEvent(input$begin_scale, { in_intro(FALSE) })
  
  output$instructions_bar <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NULL)
    L <- lang()
    cfg <- get_scale_cfg(fs, L)
    div(class="instr-bar", cfg$instructions)
  })
  
  observeEvent(input$start_btn, {
    req(lang())
    L <- lang()
    sc <- get_selected_scales()
    if (length(sc) == 0) {
      showModal(modalDialog(title = txt(L, "attention"), txt(L, "warn_select"), easyClose=TRUE))
      return()
    }
    
    selected_scales(sc)
    current_scale_index(1L)
    last_group(NA_character_)
    results_scores(data.frame(Scala=character(), Punteggio=numeric(), leaf_id=integer(),
                              n_items_asked=integer(), stringsAsFactors = FALSE))
    
    init_scale(current_scale_fs())
    
    hide("page_setup")
    show("page_questions")
    hide("page_results")
    hide("page_exit")
  })
  
  Leaves_now <- reactive({
    L <- Leaves_current()
    if (is.null(L)) return(NULL)
    filter_leaves(L, answers())
  })
  
  next_var <- reactive({
    if (isTRUE(in_intro())) return(NA_character_)
    fs <- current_scale_fs()
    if (is.null(fs)) return(NA_character_)
    L  <- Leaves_now()
    if (is.null(L) || nrow(L) == 0) return(NA_character_)
    
    v_route <- choose_route_var(L, answers())
    if (!is.na(v_route) && nzchar(v_route)) return(v_route)
    
    lid <- if (nrow(L) == 1) L$leaf_id[1] else choose_best_leaf(L)
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
  
  output$question <- renderUI({
    if (isTRUE(in_intro())) return(NULL)
    fs <- current_scale_fs()
    v  <- next_var()
    if (is.null(fs) || is.na(v)) return(NULL)
    
    L <- lang()
    row <- DICT %>% filter(fs_var == fs, Item == v)
    txtq <- v
    if (nrow(row) > 0) {
      t_it <- row$Text_IT[1]
      t_en <- row$Text_EN[1]
      if (L == "en" && !is.na(t_en) && nzchar(t_en)) txtq <- t_en
      else if (!is.na(t_it) && nzchar(t_it)) txtq <- t_it
    }
    h3(txtq)
  })
  
  values_reactive <- reactive({
    fs <- current_scale_fs()
    if (is.null(fs)) return(DEFAULT_CFG$values)
    get_scale_cfg(fs, lang())$values
  })
  
  output$buttons <- renderUI({
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
      actionButton(paste0("resp_", i), lab, class = "btn-choice btn btn-primary")
    })
    do.call(tagList, btns)
  })
  
  # ============================================================
  # FIX HELPERS: reverse-coding for scales (IRRIT_FS)
  # ============================================================
  REVERSE_CODE_SCALES <- c("IRRIT_FS")
  
  reverse_code_value <- function(val, values_vec) {
    if (is.null(values_vec) || !length(values_vec) || is.null(val) || !is.finite(val)) return(val)
    pos <- match(val, values_vec)
    if (!is.na(pos)) return(rev(values_vec)[pos])
    mn <- suppressWarnings(min(values_vec, na.rm = TRUE))
    mx <- suppressWarnings(max(values_vec, na.rm = TRUE))
    if (is.finite(mn) && is.finite(mx)) return((mx + mn) - val)
    val
  }
  
  compute_lers_score <- function(fs, leaf_id, answers_named) {
    key <- paste0(fs, "__", leaf_id)
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
  
  finalize_current_scale <- function() {
    fs <- current_scale_fs()
    if (is.null(fs)) return()
    
    L <- Leaves_now()
    if (is.null(L) || nrow(L) == 0) L <- Leaves_current()
    if (is.null(L) || nrow(L) == 0) return()
    
    lid <- if (nrow(L) == 1) L$leaf_id[1] else choose_best_leaf(L)
    score <- compute_lers_score(fs, lid, answers())
    
    res <- results_scores()
    res <- rbind(res, data.frame(
      Scala = fs,
      Punteggio = as.numeric(score),
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
  
  # --- Process a chosen response and auto-advance (NO NEXT BUTTON)
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
    
    # reverse-code if needed (IRRIT_FS)
    if (identical(fs, "IRRIT_FS") && fs %in% REVERSE_CODE_SCALES && is.finite(sel_num)) {
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
  
  output$results_text <- renderUI({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    
    if (input$mode == "massive") {
      isolate({
        wide <- res %>% select(Scala, Punteggio) %>% pivot_wider(names_from = Scala, values_from = Punteggio)
        wide$subject_id <- input$subject_id
        wide$timestamp  <- as.character(Sys.time())
        
        file_csv <- "results_massive.csv"
        if (file.exists(file_csv)) {
          old <- read.csv(file_csv, check.names = FALSE, stringsAsFactors = FALSE)
          all_cols <- union(names(old), names(wide))
          for (nm in setdiff(all_cols, names(old)))  old[[nm]]  <- NA
          for (nm in setdiff(all_cols, names(wide))) wide[[nm]] <- NA
          old  <- old[, all_cols]
          wide <- wide[, all_cols]
          write.csv(rbind(old, wide), file_csv, row.names = FALSE)
        } else {
          write.csv(wide, file_csv, row.names = FALSE)
        }
      })
      div(p(txt(L, "saved_massive")))
    } else {
      div(p(txt(L, "scores_ready")))
    }
  })
  
  output$results_note <- renderUI({
    req(lang())
    L <- lang()
    if (L == "en") {
      div(class="results-box",
          h4("How to read the scores"),
          p("The reported scores are standardized with respect to the reference sample."),
          tags$ul(
            tags$li(tags$b("0"), " = in line with the sample mean"),
            tags$li(tags$b("+1 / −1"), " = slightly above / below the mean"),
            tags$li(tags$b("+2 / −2"), " = clearly above / below the mean")
          ),
          p("In general, positive values indicate higher levels of the measured characteristic and negative values indicate lower levels. Scores farther from 0 indicate increasingly larger deviations from the sample mean (|±2| should be considered meaningful)."),
          p(tags$b("Autonomy: "), "for the Autonomy dimension, higher scores indicate lower autonomy (less decision latitude).")
      )
    } else {
      div(class="results-box",
          h4("Come leggere i punteggi"),
          p("I punteggi riportati sono standardizzati rispetto al campione di riferimento."),
          tags$ul(
            tags$li(tags$b("0"), " = in linea con la media del campione"),
            tags$li(tags$b("+1 / −1"), " = leggermente sopra / sotto la media"),
            tags$li(tags$b("+2 / −2"), " = nettamente sopra / sotto la media")
          ),
          p("In generale, valori positivi indicano livelli più alti della caratteristica misurata e valori negativi livelli più bassi. Punteggi più lontani da 0 (in positivo o in negativo) indicano uno scostamento via via più marcato rispetto alla media del campione (da ±2 lo scostamento è da considerarsi rilevante)."),
          p(tags$b("Autonomia: "), "per la dimensione Autonomia, punteggi più alti indicano minore autonomia (minore margine decisionale).")
      )
    }
  })
  
  output$selected_scale_defs <- renderUI({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    
    df <- META_AV %>%
      filter(scale_FS %in% unique(res$Scala)) %>%
      arrange(order_area, order_group, order_scale)
    
    if (nrow(df) == 0) return(NULL)
    
    div(class="results-box",
        h4(if (L=="en") "Definitions of selected scales" else "Definizioni delle scale selezionate"),
        tags$ul(lapply(seq_len(nrow(df)), function(i) {
          lab <- if (L=="en") (df$label_en[i] %||% df$label_it[i]) else df$label_it[i]
          dfn_raw <- scale_def(L, df$scale_FS[i])
          dfn <- clean_definition(dfn_raw, lab)
          tags$li(tags$b(lab), ": ", dfn)
        }))
    )
  })
  
  output$mini_table <- renderTable({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    
    out <- res %>%
      left_join(META_AV %>% select(scale_FS, area, area_en, group, group_en, label_it, label_en, order_area, order_group, order_scale),
                by = c("Scala" = "scale_FS")) %>%
      transmute(
        Area   = if (L=="en") area_en else area,
        Gruppo = if (L=="en") group_en else group,
        Scala  = if (L=="en") coalesce(label_en, label_it) else label_it,
        Punteggio = Punteggio
      )
    
    names(out) <- c(txt(L,"area_col"), txt(L,"group_col"), txt(L,"scale_col"), txt(L,"score_col"))
    out
  }, digits = 2)
  
  output$hist_plot <- renderPlot({
    res <- results_scores()
    if (nrow(res) == 0) return(NULL)
    L <- lang()
    
    dd <- res %>%
      left_join(META_AV %>% select(scale_FS, label_it, label_en, order_area, order_group, order_scale),
                by = c("Scala" = "scale_FS")) %>%
      arrange(order_area, order_group, order_scale)
    
    dd$label <- if (L=="en") ifelse(is.na(dd$label_en) | !nzchar(dd$label_en), dd$label_it, dd$label_en) else dd$label_it
    dd$label <- factor(dd$label, levels = dd$label)
    
    ggplot(dd, aes(x = label, y = Punteggio, fill = label)) +
      geom_col() +
      theme_minimal(base_size = 12) +
      labs(x = NULL, y = txt(L, "score_axis")) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      id <- if (nzchar(input$subject_id)) input$subject_id else "results"
      paste0("results_", id, ".csv")
    },
    content = function(file) {
      res <- results_scores()
      L <- lang()
      
      out <- res %>%
        left_join(META_AV %>% select(scale_FS, area, area_en, group, group_en, label_it, label_en),
                  by = c("Scala" = "scale_FS")) %>%
        transmute(
          subject_id = input$subject_id,
          timestamp  = as.character(Sys.time()),
          scale_FS   = Scala,
          area       = if (L=="en") area_en else area,
          group      = if (L=="en") group_en else group,
          label      = if (L=="en") coalesce(label_en, label_it) else label_it,
          score      = as.numeric(Punteggio),
          leaf_id    = leaf_id,
          n_items_asked = n_items_asked,
          lang = L
        )
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  confirm_exit_reset <- function() {
    L <- lang() %||% "it"
    showModal(modalDialog(
      title = txt(L, "exit_title"),
      footer = tagList(
        modalButton(txt(L, "cancel")),
        actionButton("do_reset", txt(L, "restart"), class="btn btn-warning"),
        actionButton("do_exit",  txt(L, "exit"), class="btn btn-danger")
      ),
      easyClose = TRUE
    ))
  }
  
  observeEvent(input$exit_or_reset_mid, confirm_exit_reset())
  observeEvent(input$exit_or_reset_bottom, confirm_exit_reset())
  
  observeEvent(input$do_reset, {
    removeModal()
    selected_scales(character())
    current_scale_index(NA_integer_)
    last_group(NA_character_)
    Leaves_current(NULL)
    answers(list())
    asked(character())
    in_intro(TRUE)
    results_scores(data.frame(Scala=character(), Punteggio=numeric(), leaf_id=integer(),
                              n_items_asked=integer(), stringsAsFactors = FALSE))
    show("page_setup"); hide("page_questions"); hide("page_results"); hide("page_exit")
  })
  
  observeEvent(input$do_exit, {
    removeModal()
    hide("page_setup"); hide("page_questions"); hide("page_results"); show("page_exit")
  })
}

shinyApp(ui, server)
