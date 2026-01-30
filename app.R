

library(shiny)
library(tidyverse)
library(httr)
library(readxl)
library(RColorBrewer)
library(feather)
library(gridExtra)
library(bslib)
library(thematic)
library(shinyWidgets)
library(scales)
library(shinyjs)
library(shinyBS)
library(kableExtra)
library(shinycssloaders)
library(shinyvalidate)
library(rmarkdown)
library(tinytex)
library(gt)
library(jsonlite)
library(htmltools)
library(shiny.i18n)
library(rsconnect)

#==mail
library(emayili) #email
#library(polished) #email validate evt
library(shinyalert)

#hier lokaal writemanifest doen, voor posit connect
if(str_detect(getwd(), "pieter")) {rsconnect::writeManifest()}

  #opzet vertaling vanuit spreadsheet
  df_translations <- read_excel("translations/translations.xlsx", sheet = "Sheet1")
  #vqn excel sheet naar json
  # Build the JSON structure ---------------------------------------------
  json_structure <- list(
    cultural_date_format = "%d-%m-%Y",
    languages = colnames(df_translations),
    translation = df_translations
  )
  # Convert to JSON (pretty = TRUE for nice formatting)
  json_output <- toJSON(json_structure, 
                        pretty = TRUE, 
                        auto_unbox = TRUE, 
                        na = "null")
  
  json_path <- file.path(tempdir(), "translations.json")
  
  # Save to file
  writeLines(json_output, json_path)
  #writeLines(json_output, "translations/translations.json")

  vertaler <- Translator$new(translation_json_path = json_path)
  #vertaler <- Translator$new(translation_json_path = "translations/translations.json")
  #vast op engels
  vertaler$set_translation_language("nl")
  #vertaling

#geen liggend streepje in naam, anders werkt het niet in windows, het gaat om de titel op de eerste bladzijde

#om mail vanaf gmail te kunnen sturen is
# een zgn "app password" noodzakelijk voor "less secure apps"
# je moet two-step hebben ingeregeld
# ga hiervoor naar https://support.google.com/accounts/answer/185833?hl=en
# ga naar your google account, dan security, dan app password
# deze gegevens staan in het bestand .Renviron en worden hieronder ingelezen
# de .Renviron file staat in de root van de project folder

test_modus <- FALSE

IP <- content(GET("https://api.ipify.org?format=json"))$ip


#==mail variabelen, komt uit .Renviron, varieert per user
smtp_pp <- emayili::server(
  host = "smtp.gmail.com",
  port = "587",
  username = "pieterprins2@gmail.com",
  password = "ohfsvttthbtyypts",
  max_times = 1
)

smtp_username_pp <- Sys.getenv("SMTP_USERNAME_PP")

smtp_ba <- emayili::server(
  host = Sys.getenv("SMTP_SERVER_BA"),
  port = Sys.getenv("SMTP_PORT_BA"),
  username = Sys.getenv("SMTP_USERNAME_BA"),
  password = Sys.getenv("SMTP_PASSWORD_BA"),
  max_times = 1
)
smtp_username_ba <- Sys.getenv("SMTP_USERNAME_BA")


#colors
ba_color <- "#4E9080"
sim_lijntjes_kleur <- "#E59D00"
darkgray <- "#252525" #brewer.pal(n = 9, name = 'Greys')[8]
euroblue <- "#004494"
euroyellow <- "#ffd617"
usred <- "#bf0a30"

#twee modules inladen
source("scenariosplot_module.R")
source("barplot_module.R")


#==bestanden inlezen
df_bijstortingen_onttrekkingen_params_compleet_met_gem <-
  read_feather("df_bijstortingen_onttrekkingen_params_compleet_met_gem_vijf_250.feather")
#df_bijstortingen_onttrekkingen_params_compleet_met_gem <- read_feather("/Users/pieter/Library/CloudStorage/OneDrive-Personal/R Projects/Beleggersprofiel/shiny/df_bijstortingen_onttrekkingen_params_compleet_met_gem_vijf_250.feather")
index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr <-
  read_feather("index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr_vijf_250.feather")

#== parameters
parameters <-
  read_xlsx("parameters.xlsx", sheet = "Sheet1")
#parameters <- read_xlsx("/Users/pieter/Library/CloudStorage/OneDrive-Personal/R Projects/Beleggersprofiel/shiny/parameters.xlsx", sheet = "Sheet1")
ret_wereld_aandelen <- parameters %>% filter(parameter == "ret_wereld_aandelen") %>% select(waarde) %>% pull()
ret_nl_obl <- parameters %>% filter(parameter == "ret_nl_obl") %>% select(waarde) %>% pull()
sd_wereld_aandelen <- parameters %>% filter(parameter == "sd_wereld_aandelen") %>% select(waarde) %>% pull()
sd_nl_obl <- parameters %>% filter(parameter == "sd_nl_obl") %>% select(waarde) %>% pull()
cor_wereld_aandelen_nl_obl <- parameters %>% filter(parameter == "cor_wereld_aandelen_nl_obl") %>% select(waarde) %>% pull()
inflatie <- parameters %>% filter(parameter == "inflatie") %>% select(waarde) %>% pull()
#==

#om een header rij toe te voegen voor het inlezen in AIRS
AIRS_en_Beleggersprofiel_NL_vragen_kolom <- read_csv("AIRS_en_Beleggersprofiel_NL_vragen_kolom.csv", show_col_types = FALSE)

profiel_levels <- c("RM100", "RD30RM70", "RD50RM50", "RD70RM30", "RD100")
#horizon_levels <- c("3 jaar", "5 jaar", "10 jaar", "20 jaar", "30 jaar")
profiel_levels_AIRS <- c("100 RM", "30 RD - 70 RM", "50 RD - 50 RM", "70 RD - 30 RM", "100 RD")


#datum nl
#functie om de nl datum in de tekst te zetten
maanden_nl <- c("januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktober", "november", "december")

maanden_naar_nl <- function(datum_met_us_maanden) {
  str_replace_all(datum_met_us_maanden, c("Jan" = "januari", "Feb" = "februari", "Mar" = "maart", "Apr" = "april", "May" = "mei", 
                                          "Jun" = "juni", "Jul" = "juli", "Aug" = "augustus", "Sep" = "september", "Oct" = "oktober", "Nov" = "november", "Dec" = "december"))
}

format_datum_nl <- function(date) {
  dag <- format(as.Date(date), "%d")
  maand <- maanden_nl[month(date)]
  paste0(dag, " ", maand, " ", format(as.Date(date), "%Y"))
}

format_datum_tijd_nl <- function(datum_tijd) {
  dag <- format(as.Date(datum_tijd), "%d")
  maand <- maanden_nl[month(datum_tijd)]
  paste0(dag, " ", maand, " ", format(as.Date(datum_tijd), "%Y"), " ", as.POSIXct(Sys.time()) |> format("%H:%M"))
}

#==== vragen en antwoorden database
vragen_antwoorden_excel_bestand <- "vragen_antwoorden_vijf_dz_plus.xlsx"
#
vragen_antwoorden_start_new <-
  bind_rows(read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_prive"),
            read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_enof"),
            read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_bv"),
            read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_ve"),
            #st
            read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_ve") %>%
              mutate_at(vars(c(vraag_nl, antwoord_nl, selected_nl)), ~ str_replace(., "vereniging", "stichting")) %>%
              mutate_at(vars(c(vraag_en, antwoord_en, selected_en)), ~ str_replace(., "association", "foundation")),
            #org
            read_xlsx(vragen_antwoorden_excel_bestand, sheet = "vragen_antwoorden_ve") %>%
              mutate_at(vars(c(vraag_nl, antwoord_nl, selected_nl)), ~ str_replace(., "vereniging", "organisatie")) %>%
              mutate_at(vars(c(vraag_en, antwoord_en, selected_en)), ~ str_replace(., "association", "organization"))) %>%
  pivot_longer(
            cols = ends_with("_nl") | ends_with("_en"),
            names_to = c(".value", "taal"),
            names_sep = "_"
          ) %>%
  group_by(entiteit, taal, vraag) %>%
  uncount((1:n() == 1) + 1) %>%
  mutate(n = n(),
         #nr = seq(n),
         nr = row_number(),
         antwoord_aanhef = vraag,
         #vult het eerste antwoord in, nl. "Maak een hieronder keuze:"
         antwoord = ifelse(nr == 1 & taal == "nl",  "Maak hieronder een keuze:", antwoord),#Maak hieronder een keuze:", antwoord),
         antwoord = ifelse(nr == 1 & taal == "en",  "Make a choice below:", antwoord),#Maak hieronder een keuze:", antwoord),
         selected = ifelse(nr == 1, NA, selected),
         punten = ifelse(nr == 1, 0, punten)) %>%
  select(-c(n, nr)) %>%
  ungroup() %>%
  select(taal, entiteit, everything())


theme_modern <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#4E9080",
  base_font = font_google("Inter", local = TRUE),
  heading_font = font_google("Inter", local = TRUE, wght = 600)
) %>%
  bs_add_rules("
    /* knoppen voor SO invoer */
    .btn-neutraal {
      background-color: #4E9080;
      border-color: #ced4da;
      color: white;
    }
    .btn-neutraal.active {
      background-color: #adb5bd;
      border-color: #adb5bd;
      color: white;
    }
    /* ────────────────────────────────────────────────
       NAVBAR BACKGROUND – Flatly default (light grey)
    ──────────────────────────────────────────────── */
    .navbar,
    .navbar-expand {
      background-color: #f8f9fa !important;
      border-bottom: 1px solid #dee2e6 !important;
    }

    /* Prevent any green/teal from bleeding into navbar */
    .navbar,
    .navbar-expand,
    .bg-primary,
    .bg-success,
    .bg-teal,
    .bg-green {
      background-color: #f8f9fa !important;
    }

    /* ────────────────────────────────────────────────
       NAVBAR – LOGO & BRAND
    ──────────────────────────────────────────────── */
    .navbar-brand {
      display: flex !important;
      align-items: center !important;
      gap: 12px;
    }

    .navbar-brand img {
      max-height: 70px !important;           /* adjust as desired */
      width: auto !important;
      object-fit: contain;
      margin: 0 !important;
    }

    /* ────────────────────────────────────────────────
       NAVBAR – MENU ITEMS (GENERAL)
    ──────────────────────────────────────────────── */
    .navbar-nav .nav-link {
      font-size: 1.15rem !important;         /* bigger font */
      padding: 0.65rem 1.2rem !important;
      transition: all 0.25s ease;
      border-radius: 8px;
      background-color: #f8f9fa !important;  /* grey background inactive */
    }
    /* Vertically center text in ALL nav links */
    .navbar-nav .nav-link {
      display: flex !important;          /* turn into flex container */
      align-items: center !important;    /* vertically center content */
      justify-content: center !important; /* horizontally center if needed (bonus) */
      height: 100% !important;           /* ensure it fills the nav item height */
      min-height: 50px;                  /* optional: enforce a minimum height if navbar is tall */
    }
    
    /* If you have varying padding already, you can fine-tune: */
    .navbar-nav .nav-link {
      padding-top: 0.65rem !important;
      padding-bottom: 0.65rem !important;
      /* your existing padding was 0.65rem 1.2rem → symmetric vertical padding helps centering */
    }

    /* ────────────────────────────────────────────────
       ALL TABS EXCEPT THE LAST THREE
       Inactive: bold #4E9080 + grey bg
       Active/hover: #4E9080 bg + white bold
    ──────────────────────────────────────────────── */
    .navbar-nav .nav-link:not([data-value='panel_voorlopige_conclusie']):not([data-value='panel_projecties']):not([data-value='panel_tabel']) {
      color: #4E9080 !important;
      font-weight: 600 !important;           /* bold when inactive */
    }

    .navbar-nav .nav-link:not([data-value='panel_voorlopige_conclusie']):not([data-value='panel_projecties']):not([data-value='panel_tabel']):hover,
    .navbar-nav .nav-link:not([data-value='panel_voorlopige_conclusie']):not([data-value='panel_projecties']):not([data-value='panel_tabel']).active {
      color: white !important;
      background-color: #4E9080 !important;
      font-weight: 600 !important;           /* extra bold when active */
    }

    /* ────────────────────────────────────────────────
       LAST THREE TABS
       Inactive: blue text + grey bg
       Active/hover: white text + blue bg
    ──────────────────────────────────────────────── */
    .navbar-nav .nav-link[data-value='panel_voorlopige_conclusie'],
    .navbar-nav .nav-link[data-value='panel_projecties'],
    .navbar-nav .nav-link[data-value='panel_tabel'] {
      font-size: 1rem !important;     /* ← explicitly match the others */
      color: #004494 !important;             /* blue text when inactive */
      background-color: #f8f9fa !important;  /* grey bg inactive */
      font-weight: 500 !important;           /* normal weight inactive */
    }

    .navbar-nav .nav-link[data-value='panel_voorlopige_conclusie']:hover,
    .navbar-nav .nav-link[data-value='panel_projecties']:hover,
    .navbar-nav .nav-link[data-value='panel_tabel']:hover,
    .navbar-nav .nav-link[data-value='panel_voorlopige_conclusie'].active,
    .navbar-nav .nav-link[data-value='panel_projecties'].active,
    .navbar-nav .nav-link[data-value='panel_tabel'].active {
      font-size: 1rem !important;     /* ← explicitly match the others */
      color: white !important;               /* white text when active/hover */
      background-color: #004494 !important;
      font-weight: 600 !important;           /* bold when active/hover */
      border-radius: 8px;
    }

    /* ────────────────────────────────────────────────
       VERTICAL CENTERING – LOGO & FLAG/EMOJI
    ──────────────────────────────────────────────── */
    .navbar-nav .nav-link:first-child,
    .navbar-nav .nav-link[data-value='panel_voorlopige_conclusie'] {
      display: flex !important;
      align-items: center !important;
      gap: 8px;
    }

    .navbar-nav .nav-link:first-child span,
    .navbar-nav .nav-link:first-child [data-emoji],
    .navbar-nav .nav-link em {
      line-height: 1;
      font-size: 1.3em;
      vertical-align: middle;
      display: inline-flex;
      align-items: center;
    }

    /* ────────────────────────────────────────────────
       BUTTONS, CARDS, OTHER STYLES
    ──────────────────────────────────────────────── */
    .card {
      border-radius: 12px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.08);
      margin-bottom: 1.5rem;
    }

    .btn-primary {
      background-color: #4E9080;
      border-color: #4E9080;
    }
    .btn-primary:hover {
      background-color: #3e7566;
    }

    .button {
      background-color: #4E9080;
      color: white;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
    }

    .back_button {
      background-color: #80808080;
      color: white;
      border: 1px solid #dee2e6;
      padding: 10px 20px;
      font-size: 16px;
    }

    .werk_button {
      background-color: #004494;
      color: white;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
    }

    .infobutton {
      background-color: #94bcb2;
      color: white;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
    }

    .disabled {
      opacity: 0.65;
      pointer-events: none;
      cursor: not-allowed;
    }
    
/* Make the notification bigger and move it (e.g. top-right or center) */
    .shiny-notification {
      position: fixed !important;
      top: 20px !important;          /* or calc(50% - 100px) for center */
      right: 20px !important;        /* or left: 50%; transform: translateX(-50%) for center */
      width: 400px !important;       /* wider box */
      height: auto !important;
      border-radius: 8px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.2);
      z-index: 9999 !important;
    }
    
    /* Style the progress inside it */
    .shiny-notification .progress {
      height: 12px !important;       /* thicker bar */
      margin: 10px 0 !important;
    }
    .shiny-notification .progress-bar {
      background-color: #4E9080 !important;
      transition: width 0.4s ease;
    }
    .shiny-notification .shiny-notification-content {
      font-size: 40px !important;
      padding: 15px !important;
      text-align: center !important;
      color: #444444 !important;
    }
    .shiny-notification .shiny-notification-close {
      font-size: 18px !important;
    }
    .shiny-notification {
      top: calc(50% - 80px) !important;
      left: calc(50% - 250px) !important;
      right: auto !important;
      bottom: auto !important;
      width: 500px !important;
      max-width: 90% !important;
    }
    /*#donkerrode waarschuwingen - check */
    .shiny-output-error-validation {
      color: #bf0a30;
      font-weight: bold;
    }
  ")
# ────────────────────────────────────────────────
# UI – fully static with page_navbar + nav_panel
# ────────────────────────────────────────────────
ui <- page_navbar(
  id = "tabset", 
  title = div(
    img(
      src = "logo.png",
      height = "80px",
      style = "margin-top:-8px; margin-right:12px;"
    ),
    "" #geen tekst in titel, moet anders in server
  ),
  
  theme = theme_modern,
  fillable = TRUE,
  collapsible = TRUE,
  
  # ==========================================================
  # HEADER — everything that is NOT a nav_panel goes here
  # ==========================================================
  header = tagList(
    
    tags$head(
      # tags$link(
      #   rel = "stylesheet",
      #   type = "text/css",
      #   href = "beleggersprofiel.css"
      # ),
      
      # Trigger download from server
      tags$script(HTML("
        Shiny.addCustomMessageHandler('triggerDownload', function(message) {
          const link = document.createElement('a');
          link.href = message.href;
          link.download = message.filename;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        });
      ")),
      
      # Heartbeat / keep-alive
      tags$script(HTML("
        let socket_interval;
        let counter = 0;

        $(document).on('shiny:connected', function () {
          socket_interval = setInterval(function () {
            Shiny.setInputValue('heartbeat', counter++, {priority: 'event'});
          }, 15000);
        });

        $(document).on('shiny:disconnected', function () {
          clearInterval(socket_interval);
        });
      "))
    ),
    
    shinyjs::useShinyjs(),
    usei18n(vertaler)
  ),
  
  # ==========================================================
  # TAAL
  # ==========================================================
  nav_panel(
    #ui_tabset_taal_titel is de vlag
    title = uiOutput("ui_tabset_taal_titel"), value = "panel_taal", class = "p-4",
    card(
      card_header(h4(uiOutput("ui_vul_taal_in"))),
      card_body(layout_columns(
        col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
        uiOutput("ui_jumpToP_start_button")
        )
      )
    )
   ),
   # ==========================================================
   # START
   # ==========================================================
  nav_panel(title = "Start", value = "panel_start", class = "p-4",
     card(
        card_header(h3(uiOutput("ui_intro_head"))),
        card_body(
          h5(uiOutput("ui_intro_text")),
          #h5(uiOutput("ui_vul_naam_in")),
          h5(uiOutput("ui_vakje_vul_naam_in")),
          h5(uiOutput("ui_u_krijgt_rapport_text")),
          h5(uiOutput("ui_vraag_emailadres")),
          #h5(uiOutput("ui_intro_vraag_entiteit")),
          h5(uiOutput("ui_vraag_entiteit")),
          h5(uiOutput("ui_vraag_naam_entiteit")),
          h5(uiOutput("ui_vraag_beleggingsstatuut")),
          textOutput("error_msg_start")
        ),
        card_footer(
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
            uiOutput("ui_jump_back_ToP_taal_button"),
            NULL,
            uiOutput("ui_jumpToP_beleggingsdoelstelling_button")
          )
        )
      )
    ),
#   # ==========================================================
#   # I. BELEGGINGSDOELSTELLING
#   # ==========================================================
  nav_panel(title = "I", value = "panel_beleggingsdoelstelling", class = "p-4",
    card(
      card_header(h3("I. ", uiOutput("ui_beleggingsdoelstelling", inline = TRUE))),
      card_body(
        h5(uiOutput("ui_aanhef_doelstelling")),
        uiOutput("ui_selectize_beleggingsdoelstelling"),
        uiOutput("ui_cond_panel_toelichting_doelstelling"),
        textOutput("error_msg_beleggingsdoelstelling")
      ),
      card_footer(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
          uiOutput("ui_jump_back_ToP_start_button"),
          NULL,
          uiOutput("ui_jumpToP_financiele_situatie_button")
        )
      )
    )
  ),
#   
#   # ==========================================================
#   # II. FINANCIËLE SITUATIE
#   # ==========================================================
  nav_panel(title = "II", value = "panel_financiele_situatie", class = "p-4",
    card(
      card_header(h3("II. ", uiOutput("ui_financiele_situatie", inline = TRUE))),
      card_body(
        uiOutput("ui_aanhef_financiele_situatie"),
        uiOutput("ui_selectize_financiele_situatie"),
        textOutput("error_msg_financiele_situatie")
      ),
      card_footer(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
          uiOutput("ui_jump_back_ToP_beleggingsdoelstelling_button"),
          NULL,
          uiOutput("ui_jumpToP_kennis_ervaring_button")
        )
      )
    )
  ),
#   
#   # ==========================================================
#   # III. KENNIS & ERVARING
#   # ==========================================================
  nav_panel(title = "III", value = "panel_kennis_ervaring", class = "p-4",
    card(
      card_header(h3("III. ", uiOutput("ui_kennis_en_ervaring", inline = TRUE))),
      card_body(
        uiOutput("ui_selectize_kennis_ervaring"),
        textOutput("error_msg_kennis_ervaring")
      ),
      card_footer(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
          uiOutput("ui_jump_back_ToP_financiele_situatie_button"),
          NULL,
          uiOutput("ui_jumpToP_risicobereidheid_button")
        )
      )
    )
  ),

  # ==========================================================
  # IV. RISICOBEREIDHEID
  # ==========================================================
  nav_panel(title = "IV", value = "panel_risicobereidheid", class = "p-4",
    card(
      card_header(h3("IV. ", uiOutput("ui_risicobereidheid", inline = TRUE))),
      card_body(
        uiOutput("ui_aanhef_risicobereidheid"),
        uiOutput("ui_selectize_risicobereidheid"),
        textOutput("error_msg_risicobereidheid")
      ),
      card_footer(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
          uiOutput("ui_jump_back_ToP_kennis_ervaring_button"),
          NULL,
          uiOutput("ui_jumpToP_startvermogen_button")
        )
      )
    )
  ),
#   
#   # ==========================================================
#   # V. STARTVERMOGEN / STORTINGEN
#   # ==========================================================
  nav_panel(title = "V", value = "panel_startvermogen", class = "p-4",
    card(
      card_header(
        h3("VI-2. ", uiOutput("ui_startvermogen_en_so", inline = TRUE))),
      card_body(
        h5(uiOutput("ui_input_startvermogen")),
        textOutput("error_msg_startvermogen"),
        h5(uiOutput("ui_input_stortingen_onttrekkingen")),

        fluidRow(
          column(
            6,
            conditionalPanel(
              condition = "input.keuze_SO == 'ja_bijstorten'",
              h5(uiOutput("ui_input_bijstorting"))
            ),
            conditionalPanel(
              condition = "input.keuze_SO == 'nee_onttrekken'",
              h5(uiOutput("ui_input_onttrekking"))
            )
          )
        )
      ),
      card_footer(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
          uiOutput("ui_jump_back_ToP_risicobereidheid_button"),
          NULL,
          uiOutput("ui_jumpToP_visuele_keuze1_button")
        )
      )
    )
  ),
#   # ==========================================================
#   # VI-1. Visuele keuze stap 1
#   # ==========================================================
nav_panel(title = "VI-1", value = "panel_visuele_keuze1", class = "p-4",
  card(
    card_header(
      h3("VI-1. ", uiOutput("ui_visuele_keuze_rd_rm_stap1", inline = TRUE))),
    card_body(
      p(
        style = "text-align: justify;",
        h5(uiOutput("ui_op_deze_pagina_twee_risicoprofielen"))
      ),
      uiOutput("ui_barplot_een_module"),
      h5(uiOutput("ui_selectize_visuele_keuze1")),
      textOutput("error_msg_visuele_keuze1")
    ),
    
    card_footer(
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
        uiOutput("ui_jump_back_ToP_startvermogen_button"),
        actionButton(
          "grafieken_info1",
          uiOutput("ui_toelichting_grafieken_label1", inline = TRUE),
          class = "infobutton btn-lg"
        ),
        uiOutput("ui_jumpToP_visuele_keuze2_button")
      )
    )
  )
),
#   # ==========================================================
#   # VI-2. Visuele keuze stap 2
#   # ==========================================================
nav_panel(title = "VI-2", value = "panel_visuele_keuze2",  class = "p-4",
  card(
    card_header(
      h3("VI-2. ", uiOutput("ui_visuele_keuze_rd_rm_stap2", inline = TRUE))),
    card_body(
      p(
        style = "text-align: justify;",
        h5(uiOutput("ui_de_keuze_die_u_maakte_staat_in_het_midden"))
      ),
      uiOutput("ui_barplot_twee_module"),
      h5(uiOutput("ui_selectize_visuele_keuze2")),
      textOutput("error_msg_visuele_keuze2")
    ),
    
    card_footer(
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
        uiOutput("ui_jump_back_ToP_visuele_keuze1_button"),
        actionButton(
          "grafieken_info2",
          uiOutput("ui_toelichting_grafieken_label2", inline = TRUE),
          class = "infobutton btn-lg"
        ),
        uiOutput("ui_jumpToP_duurzaamheid_button")
      )
    )
  )
),
#   # ==========================================================
#   # VII. Duurzaamheid
#   # ==========================================================
nav_panel(title = "VII", value = "panel_duurzaamheid", class = "p-4",
        card(
          card_header(
            h3("VII. ", uiOutput("ui_duurzaamheid", inline = TRUE))),
        card_body(
          h5(uiOutput("ui_intro_duurzaamheid")),
          h5(uiOutput("ui_uitleg_duurzaamheid")),
          h5(uiOutput("ui_ba_moet_uitvragen")),
          uiOutput("ui_selectize_duurzaamheid_1_en_2"),
          textOutput("error_msg_duurzaamheid"),
          fluidRow(
            column(
              12,
           conditionalPanel(
             #hier ging het fout, tekst van deze conditie moet gelijk zijn voor de verschillende entiteiten
             condition = "input.duurzaamheid_bavd.includes('belangrijkste nadelige effecten') || input.duurzaamheid_bavd.includes('principal adverse impacts')", # "str_detect(input.duurzaamheid_bavd, `de belangrijkste nadelige effecten`)", #== `moet rekening gehouden worden met de belangrijkste nadelige effecten op duurzaamheidsfactoren zoals hierboven genoemd`",
             style = "width: 100%;",
             h5(uiOutput("ui_duurzame_beleggingen_zijn")),
             uiOutput("ui_selectize_duurzaamheid_3"),
             h5(style = "text-align: justify;", vertaler$t("eu_taxonomie")),
             uiOutput("ui_selectize_duurzaamheid_4")
           ))),
          card_footer(
            layout_columns(
              col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
              uiOutput("ui_jump_back_ToP_visuele_keuze2_button"),
              NULL,
              uiOutput('ui_jumpToP_overig_button')
              )
           )
          )
        )
        ),
#   # ==========================================================
#   # VII. Overig
#   # ==========================================================
nav_panel(title = "VIII", value = "panel_overig", class = "p-4",
          card(
            card_header(
              h3("VIII. ", uiOutput("ui_overig", inline = TRUE))),
            card_body(
              h5(uiOutput("ui_overige_aspecten_toelichten")),
              h5(uiOutput("ui_selectize_overig")),
              uiOutput("ui_cond_panel_toelichting_overig"),
              textOutput('error_msg_overig')
              ),
            card_footer(
              #layout_columns(
               # col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
                #col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
                uiOutput("ui_panel_overig_buttons")
              #)
            )
            )
          ),
#   # ==========================================================
#   # VII. Rapport
#   # ==========================================================
nav_panel(title = uiOutput("ui_tabset_rapport_titel"), value = "panel_rapport", class = "p-4",
        card(
          card_header(
            h3(uiOutput("ui_belangrijke_informatie"))),
          card_body(
            h5(uiOutput("ui_disclaimer")),
            h5(uiOutput("ui_cond_panel_vraag_opsturen_beleggingsstatuut")),
            h5(uiOutput("ui_rapport_maken_en_versturen")),
            h5(uiOutput("ui_als_u_op_onderstaande_knop_klikt"))
          ),
          card_footer(
            layout_columns(
            col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
              uiOutput("ui_jump_back_ToP_overig_button"),
              uiOutput("ui_start_rapport_button"),
              uiOutput('ui_naar_voorlopige_conclusie')),
            downloadButton(outputId = "rapport_hidden", "Hidden Download", style = "display: none;")
          )
        )
    )
)

 server <- function(input, output, session) {
# 
  taal <- reactive({
    input$taal %||% "nl"  # Use %||% from rlang or purrr, or if (is.null(input$taal)) "nl" else input$taal
  })

  output$ui_tabset_taal_titel <- renderUI({
    # vlag afh van taal
    flag <- if (taal() == "en") span("🇬🇧", style = "font-size: 1.75em;") else if (taal() == "nl") span("🇳🇱", style = "font-size: 1.75em;") else ""
    tagList(flag)
  })
  
  output$ui_tabset_rapport_titel <- renderUI({taal(); vertaler$t("rapport")})

  test_modus <- reactive({
    req(naam())
    if(naam() == "testen") {TRUE} else {FALSE}
  })

  #===keepAlive voor de teller
  #geen text tonen, alleen maar in een reactive zetten
  output$keepAlive <- reactive({
    input$tellertje_actief
   })
  #===
  entiteit_soort <- reactive({
    switch(input$entiteit,
           "enof" = vertaler$t("enof"),
           "prive" = vertaler$t("prive"),
           "bv" = vertaler$t("bv"),
           "ve" = vertaler$t("ve"),
           "st" = vertaler$t("st"),
           "ov" = vertaler$t("ov")
    )
  })

  observeEvent(input$taal, {
    print(paste("verandering taal:", input$taal))
    shiny.i18n::update_lang(input$taal)
  })

  #====
  big_mark <- reactive({if (taal() == "nl") "." else ","})
  decimal_mark <- reactive({if (taal() == "nl") "," else "."})

  bedragenInput <- function(inputId, label, value) {
    shinyWidgets::autonumericInput(
      inputId = inputId,
      label = label,
      value = value,
      currencySymbolPlacement = "p",
      currencySymbol = "€",
      decimalPlaces = 0,
      minimumValue = 0,
      digitGroupSeparator = big_mark(),
      decimalCharacter = decimal_mark(),
      align = "left",
      width = 800,
      class = "autonum-green")
  }
  
  format_bedrag_tabellen <- function(bedrag, big_mark, decimal_mark) {
    paste0(" €", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
                         format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  }
  #voor bijstorting tot 100 euro nauwkeurig
  format_bedrag_tabellen_bijstorting <- function(bedrag, big_mark, decimal_mark) {
    paste0(" €", formatC(as.numeric(round(bedrag/100, 0) * 100),
                         format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  }
  #====


  vragen_antwoorden <- reactive({
    vragen_antwoorden_start <-
    vragen_antwoorden_start_new %>%
    filter(entiteit == entiteit(), taal == taal()) %>%
      select(-entiteit, - taal)

      factor_vragen <-
        vragen_antwoorden_start %>%
        select(vraag) %>%
        unique() %>% pull()

      vragen_antwoorden_start %>%
        mutate(nummer = as.numeric(factor(vraag, levels = factor_vragen)))

  })

  max_min_punten_categorie <- function(categorie, min_max) {
    vragen_antwoorden() %>%
      filter(antwoord != vertaler$t("maak_keuze"),
             categorie == !!categorie) %>%
      group_by(vraag) %>%
      # van een tot max, geen nullen
      filter(punten != 0) %>%
      filter(punten == if(min_max == "max") {max(punten)} else {min(punten)}) %>%
      ungroup() %>%
      summarise(min_of_max = sum(punten)) %>% pull()
  }

  #een tabel is reactive, een functie kan reactives gebruiken
  vragen <- reactive({
    vragen_antwoorden() %>% select(nummer, vraag, onderwerp, antwoord_aanhef, categorie) %>% unique()
  })

  vraag <- function(nummer = 1) {
    #vraag behorend bij nummer
    vragen_antwoorden() %>% filter(nummer == !!nummer) %>% select(vraag) %>% head(1) %>% pull(1)
  }

  #vragen_antwoorden waarbij aantal punten achter het antwoord staat, dus niet de visuele
  vragen_antwoorden_punten <- reactive({
    vragen_antwoorden() %>%
    rowwise() %>%
    mutate(punten_tekst =
             ifelse(antwoord == vertaler$t("maak_keuze"), "",
                    str_c(" (", punten, if(punten == 1) {" punt)"} else if(punten != 1) {" punten)"})),
           antwoord_met_punten = str_c(antwoord, punten_tekst),
           selected = str_c(selected, punten_tekst))
  })

  opties_vraag <- function(nummer = 1) {
    #opties van een vraag met aantal punten erachter
    #vragen_antwoorden_punten %>% filter(nummer == !!nummer) %>% select(antwoord_met_punten) %>% pull(1)
    vragen_antwoorden_punten() %>% filter(nummer == !!nummer) %>% select(antwoord) %>% pull(1)
  }
  onderwerp_vraag <- function(nummer = 1) {
    #onderwerp behorend bij vraag
    vragen_antwoorden_punten() %>% filter(nummer == !!nummer) %>% select(onderwerp) %>% head(1) %>% pull(1)
  }
  categorie_vraag <- function(nummer = 1) {
    #categorie behorend bij vraag
    vragen_antwoorden_punten() %>% filter(nummer == !!nummer) %>% select(categorie) %>% head(1) %>% pull(1)
  }
  sub_nummer_vraag <- function(id) {
    #nummer van vraag i van alle vragen binnen een categorie
    categorie_vraag_nummers <-
      vragen_antwoorden() %>% filter(categorie == categorie_vraag(id)) %>% select(nummer) %>% unique() %>% pull()
    which(categorie_vraag_nummers == id)
  }
  # aantal_vragen_categorie <- function(id) {
  #   #hoeveel vragen binnen de categorie
  #   categorie_vraag_nummers <-
  #     vragen_antwoorden() %>% filter(categorie == categorie_vraag(id)) %>% select(nummer) %>% unique() %>% pull()
  #   length(categorie_vraag_nummers)
  # }
  selected_vraag <- function(nummer = 1) {
    #initiele keuze behorend bij vraag
    vragen_antwoorden() %>% filter(nummer == !!nummer) %>% select(selected) %>% arrange(selected) %>% head(1) %>% pull()
    #geeft punten erachter
  }

  punten_antwoord_vraag <- function(nummer = 1, antwoord) {
    #aantal punten behorend bij antwoord op vraag nummer
    vragen_antwoorden_punten() %>% filter(nummer == !!nummer, antwoord == !!antwoord) %>% select(punten) %>% as.integer()
  }
  antwoord_aanhef <- function(nummer = 1) {
    vragen_antwoorden() %>% filter(nummer == !!nummer) %>% select(antwoord_aanhef) %>% head(1) %>% pull(1)
  }

  vraagnummers_categorie <- function(categorie) {
    #nummers van de vragen uit een categorie
    vragen() %>% filter(categorie == !!categorie) %>%
      select(nummer) %>% unique() %>% pull(nummer)
  }
  vraag_met_subnummer <- function(id) {
    #subnummer - vraag i (voor tabel rapport)
    str_c(sub_nummer_vraag(id), " - ", vraag(id))
  }

naam_in_file_en_aanhef <- reactive({
  if(entiteit() %in% c("bv", "st", "ve", "ov")) {
    naam_entiteit() |>
      str_replace("\\([^()]{0,}\\)", "") |>
      str_replace("Mevrouw", "Mevr.") |>
      str_replace("mevrouw", "mevr.") |>
      str_replace("De heer", "Dhr.") |>
      str_replace("de heer", "dhr.") |>
      str_replace("B.V.", "BV") |>
      str_replace("b.v.", "BV") |>
      str_replace("/", "")
  }
  else {
   naam() |>
      str_replace("\\([^()]{0,}\\)", "") |>
      str_replace("Mevrouw", "Mevr.") |>
      str_replace("mevrouw", "mevr.") |>
      str_replace("De heer", "Dhr.") |>
      str_replace("de heer", "dhr.") |>
      str_replace("B.V.", "BV") |>
      str_replace("b.v.", "BV") |>
      str_replace("/", "")
  }
})
output$ui_intro_head <- renderText({vertaler$t("intro_head")})
output$ui_intro_text <- renderText({vertaler$t("intro_text")})
output$ui_vul_naam_in <- renderText({vertaler$t("vul_naam_in")})



output$ui_vul_taal_in <- renderUI({
  selectInput(inputId = "taal",
              label = "English/Nederlands",
              choices = c(setNames("nl", "Nederlands"),
                          setNames("en", "English")),
              selected = "nl")
})

output$ui_vakje_vul_naam_in <- renderUI({
  #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
  #tagQuery(
    textInput(inputId = "naam",
              value = NULL,
              placeholder = "...",
              label = vertaler$t("uw_naam"),
              width = '750px')#)$find("input")$addAttrs(autocomplete = "off")$allTags()
})

output$ui_vraag_emailadres <- renderUI({
 # req(test_modus())
  #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
 # tagQuery(
    textInput(inputId = "emailadres",
              value = if(test_modus()) {"pieterprins@yahoo.com"} else {""},
              placeholder = vertaler$t("email_placeholder"),
              label = vertaler$t("uw_emailadressen"),
              width = '750px')#)$find("input")$addAttrs(autocomplete = "off")$allTags()
})
output$ui_u_krijgt_rapport_text <- renderText({vertaler$t("u_krijgt_rapport_text")})

#Knoppen
output$ui_toelichting_grafieken_label1 <- renderText({taal(); vertaler$t("toelichting_grafieken")})
output$ui_toelichting_grafieken_label2 <- renderText({taal(); vertaler$t("toelichting_grafieken")})

output$ui_jumpToP_start_button <- renderUI({taal(); actionButton("jumpToP_start", "Start", class = "button btn-lg")}) #vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_beleggingsdoelstelling_button <- renderUI({taal(); actionButton("jumpToP_beleggingsdoelstelling", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_financiele_situatie_button <- renderUI({taal(); actionButton("jumpToP_financiele_situatie", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_kennis_ervaring_button <- renderUI({taal(); actionButton("jumpToP_kennis_ervaring", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_risicobereidheid_button <- renderUI({taal(); actionButton("jumpToP_risicobereidheid", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_startvermogen_button <- renderUI({taal(); actionButton("jumpToP_startvermogen", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_visuele_keuze1_button <- renderUI({taal(); actionButton("jumpToP_visuele_keuze1", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_visuele_keuze2_button <- renderUI({taal(); actionButton("jumpToP_visuele_keuze2", vertaler$t("volgende_pagina"), class = "button btn-lg")})
output$ui_jumpToP_duurzaamheid_button <- renderUI({actionButton(class = "button", 'jumpToP_duurzaamheid', vertaler$t("volgende_pagina"))})
output$ui_jumpToP_private_beleggingen <- renderUI({actionButton(class = "button", 'jumpToP_private_beleggingen', vertaler$t("volgende_pagina"))})
output$ui_jumpToP_rapport_button <- renderUI({actionButton(class = "button", 'jumpToP_rapport', vertaler$t("volgende_pagina"))})
output$ui_jumpToP_overig_button <- renderUI({actionButton(class = "button", 'jumpToP_overig', vertaler$t("volgende_pagina"))})
output$ui_jumpToP_overig <- renderUI({actionButton(class = "button", 'jumpToP_overig', vertaler$t("volgende_pagina"))})
# output$ui_jumpToP_taal <- renderUI({actionButton(class = "werk_button", 'jumpToP_taal', vertaler$t("volgende_pagina"))})

output$ui_jump_back_ToP_taal_button <- renderUI({taal(); actionButton("jumpToP_taal", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_start_button <- renderUI({taal(); actionButton("jumpToP_start", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_beleggingsdoelstelling_button <- renderUI({taal(); actionButton("jumpToP_beleggingsdoelstelling", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_financiele_situatie_button <- renderUI({taal(); actionButton("jumpToP_financiele_situatie", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_kennis_ervaring_button <- renderUI({taal(); actionButton("jumpToP_kennis_ervaring", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_risicobereidheid_button <- renderUI({taal(); actionButton("jumpToP_risicobereidheid", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_startvermogen_button <- renderUI({taal(); actionButton("jumpToP_startvermogen", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_visuele_keuze1_button <- renderUI({taal(); actionButton("jumpToP_visuele_keuze1", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_visuele_keuze2_button <- renderUI({taal(); actionButton("jumpToP_visuele_keuze2", vertaler$t("vorige_pagina"), class = "back_button")})
output$ui_jump_back_ToP_duurzaamheid_button <- renderUI({actionButton(class = "back_button", 'jumpToP_duurzaamheid', vertaler$t("vorige_pagina"))})
output$ui_jump_back_ToP_private_beleggingen <- renderUI({actionButton(class = "back_button", 'jumpToP_private_beleggingen', vertaler$t("vorige_pagina"))})
output$ui_jump_back_ToP_rapport_button <- renderUI({actionButton(class = "back_button", 'jumpToP_rapport', vertaler$t("vorige_pagina"))})
output$ui_jump_back_ToP_overig_button <- renderUI({actionButton(class = "back_button", 'jumpToP_overig', vertaler$t("vorige_pagina"))})
output$ui_jump_back_ToP_overig <- renderUI({actionButton(class = "back_button", 'jumpToP_overig', vertaler$t("vorige_pagina"))})


output$ui_start_rapport_button <- renderUI({actionButton(inputId = "start_rapport", label = vertaler$t("rapport_maken_en_versturen") , icon = icon("file-pdf"), class = "button")})

#===


#Aanhef van tabs
output$ui_beleggingsdoelstelling <- renderText({vertaler$t("beleggingsdoelstelling")})
output$ui_financiele_situatie <- renderText({vertaler$t("financiele_situatie")})
output$ui_risicobereidheid <- renderText({vertaler$t("risicobereidheid")})
output$ui_kennis_en_ervaring <- renderText({vertaler$t("kennis_en_ervaring")})
output$ui_startvermogen_en_so <- renderText({vertaler$t("startvermogen_en_so")})
output$ui_visuele_keuze_rd_rm_stap1 <- renderText({vertaler$t("visuele_keuze_rd_rm_stap1")})
output$ui_visuele_keuze_rd_rm_stap2 <- renderText({vertaler$t("visuele_keuze_rd_rm_stap2")})
output$ui_duurzaamheid <- renderText({vertaler$t("duurzaamheid")})
output$ui_overig <- renderText({vertaler$t("overig")})
output$ui_belangrijke_informatie <- renderText({vertaler$t("belangrijke_informatie")})

output$ui_op_deze_pagina_twee_risicoprofielen <- renderText(vertaler$t("op_deze_pagina_twee_risicoprofielen"))
output$ui_de_keuze_die_u_maakte_staat_in_het_midden <- renderText(vertaler$t("de_keuze_die_u_maakte_staat_in_het_midden"))

output$ui_intro_duurzaamheid <- renderText({vertaler$t("intro_duurzaamheid")})
output$ui_uitleg_duurzaamheid <- renderText({vertaler$t("uitleg_duurzaamheid")})
output$ui_ba_moet_uitvragen <- renderText({vertaler$t("ba_moet_uitvragen")})
output$ui_duurzame_beleggingen_zijn <- renderText({vertaler$t("duurzame_beleggingen_zijn")})

output$ui_overige_aspecten_toelichten <- renderText({vertaler$t("overige_aspecten_toelichten")})

output$ui_rapport_maken_en_versturen <- renderText(vertaler$t("rapport_maken_en_versturen"))

output$ui_als_u_op_onderstaande_knop_klikt <- renderText(vertaler$t("als_u_op_onderstaande_knop_klikt"))

output$ui_disclaimer <- renderText(vertaler$t("disclaimer"))

output$ui_private_beleggingen <- reactive({vertaler$t("private_beleggingen")})
output$ui_private_beleggingen_zijn_intro <- reactive({vertaler$t("private_beleggingen_zijn_intro")})
output$ui_private_beleggingen_zijn_heel_divers <- reactive({vertaler$t("private_beleggingen_zijn_heel_divers")})
output$ui_private_beleggingen_voordelen <- reactive({vertaler$t("private_beleggingen_voordelen")})
output$ui_private_beleggingen_nadelen <- reactive({vertaler$t("private_beleggingen_nadelen")})
output$ui_private_beleggingen_kosten <- reactive({vertaler$t("private_beleggingen_kosten")})
output$ui_private_beleggingen_duidelijkheid_horizon <- reactive({vertaler$t("private_beleggingen_duidelijkheid_horizon")})
output$ui_private_beleggingen_mogelijk_geschikt <- reactive({vertaler$t("private_beleggingen_mogelijk_geschikt")})
output$ui_private_beleggingen_bavd_kan_helpen <- reactive({vertaler$t("private_beleggingen_bavd_kan_helpen")})
output$ui_hieronder_vraag_interesse_private_beleggingen <- reactive({vertaler$t("hieronder_vraag_interesse_private_beleggingen")})


output$ui_intro_vraag_entiteit <- reactive({vertaler$t("intro_vraag_entiteit")})

output$ui_vraag_entiteit <- renderUI({
  #req(test_modus())  # Ensures test_modus() exists before using it
  selectInput(inputId = "entiteit",
              choices = c(vertaler$t("maak_keuze"),
                          #Maak hieronder een keuze",
                          setNames("prive", vertaler$t("prive")),
                          setNames("enof", vertaler$t("enof")),
                          setNames("bv", vertaler$t("bv")),
                          setNames("ve", vertaler$t("ve")),
                          setNames("st", vertaler$t("st")),
                          setNames("ov", vertaler$t("ov"))),
              selected = if(test_modus()) {"prive"} else {""},
              label = vertaler$t("voor_wie"),
              width = '750px')
})

output$ui_vraag_naam_entiteit <- renderUI({
  req(entiteit())
  if(entiteit() %in% c("bv", "st", "ve", "ov")) {
    p(
      h5(paste(vertaler$t("wat_is_de_naam_van"), de_org_aanduiding(), "?")),
      #paste voor een spatie
      #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
      tagQuery(textInput("naam_entiteit", value = "...",
                          label = paste(vertaler$t("de_naam_van"), de_org_aanduiding()),
                          width = '750px'))$find("input")$addAttrs(autocomplete = "off")$allTags()
    )
  }
})

#UI element in de server
output$ui_vraag_beleggingsstatuut <- renderUI({
  req(entiteit())
  if(entiteit() %in% c("st", "ve", "ov")) {
    p(h5(paste(vertaler$t("intro_pensioenbrief1"), de_org_aanduiding(), vertaler$t("intro_pensioenbrief2"), vertaler$t("intro_pensioenbrief3"))), #,
              selectizeInput(inputId = "beleggingsstatuut",
                     label = paste(vertaler$t("heeft"), de_org_aanduiding(), vertaler$t("een_beleggingsstatuut")),
                     choices = c(vertaler$t("maak_keuze"),
                                 setNames("ja_beleggingsstatuut", paste(vertaler$t("ja"), de_org_aanduiding(), vertaler$t("heeft_beleggingsstatuut"))),
                                 setNames("nee_beleggingsstatuut", paste(vertaler$t("nee"), de_org_aanduiding(), vertaler$t("heeft_geen_beleggingsstatuut")))),
                     selected = vertaler$t("maak_keuze"),#Maak hieronder een keuze:",
                     width = "750px")
    )
  } else if (entiteit() == "bv") {
      #p(h4(str_c("Indien de besloten vennootschap een pensioenbrief heeft, kunt u dit mogelijk gebruiken bij het invullen van dit beleggersprofiel.")),
      p(h5(str_c(vertaler$t("intro_pensioenbrief_bv"))),
              selectizeInput(inputId = "pensioenbrief",
                     label = vertaler$t("heeft_bv_pensioenbrief"),
                     choices = c(vertaler$t("maak_keuze"),
                                 setNames("ja_pensioenbrief", str_c(vertaler$t("bv_heeft_pensioenbrief"))), #Ja, de besloten vennootschap heeft een pensioenbrief")),
                                 setNames("nee_pensioenbrief", vertaler$t("bv_heeft_geen_pensioenbrief"))),
                     selected = vertaler$t("maak_keuze"),#Maak hieronder een keuze:",
                     width = "750px")
    )
    }
  })

  output$ui_cond_panel_vraag_opsturen_beleggingsstatuut <- renderUI({
    if (input$entiteit%in% c("st", "ve", "ov")) {
        conditionalPanel(
          condition = "input.beleggingsstatuut == `ja_beleggingsstatuut`",# `Ja, de stichting heeft een beleggingsstatuut`",
            h3(vertaler$t("beleggingsstatuut")),
            p(style = "text-align: justify;", str_c(vertaler$t("u_heeft_aangegeven_dat"), " ", de_org_aanduiding(), " ", vertaler$t("een_beleggingsstatuut_heeft")),
            vertaler$t("deze_kunt_u_sturen_naar_uw_vermogensbeheerder")),
          br()
        )
      } else if (input$entiteit == "bv") {
        conditionalPanel(
          condition = "input.pensioenbrief == ja_pensioenbrief`", #`Ja, de besloten vennootschap heeft een pensioenbrief`",
          h3(vertaler$t("pensioenbrief")),
          p(style = "text-align: justify;", vertaler$t("u_heeft_aangegeven_dat_de_bv_een_pensioenbrief_heeft_sturen")),
          br()
        )
      } else {NULL}
    })

  #UI element in de server
  #=== aanhef vragen entiteitsspecifiek
  output$ui_aanhef_doelstelling <- renderPrint({
    if(entiteit() %in% c("bv", "st", "ve", "ov")) {
      p(
        h4(str_c(vertaler$t("beleggingsdoelstelling_intro_org"), de_org_aanduiding(), vertaler$t("wordt_nagestreefd"))),
        br(),
        h4(tags$b(tags$i(vertaler$t("hou_geen_rekening_met_so"))))
      )
    } else {
      p(
        #h4("De vraag hieronder gaat over het doel dat u met uw beleggingen nastreeft."),
        h4(vertaler$t("beleggingsdoelstelling_intro")),
        br(),
        #h4(tags$b(tags$i("Houd hierbij geen rekening met eventuele stortingen of onttrekkingen. Die kunt u aangeven op een van de volgende pagina's.")))
        h4(tags$b(tags$i(vertaler$t("hou_geen_rekening_met_so"))))
      )
    }
  })

  #nadere toelichting overig box
  output$ui_cond_panel_toelichting_overig <- renderUI({
    conditionalPanel(
      condition = "input.overig.includes('graag') || input.overig.includes('please')",
    list(
      #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
      h5(
        tagQuery(
      textInput(inputId = "overig_toelichting",
                label = vertaler$t("hier_nader_toelichten"),
                value = "...", width = '6500px',
                placeholder = "..."))$find("input")$addAttrs(autocomplete = "off")$allTags()
      )
     )
    )
  })
  
  output$ui_panel_overig_buttons <- renderUI({
    prev_button <- if (isTRUE(private_beleggingen_uitvragen())) {
      actionButton("jump_back_ToP_private_beleggingen", vertaler$t("vorige_pagina"), class = "back_button")
    } else {
      actionButton("jump_back_ToP_duurzaamheid", vertaler$t("vorige_pagina"), class = "back_button")
    }
    
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
        prev_button,
        NULL,
        uiOutput("ui_jumpToP_rapport_button")
    )
  })

  #nadere toelichting doelstelling box
  output$ui_cond_panel_toelichting_doelstelling <- renderUI({
      conditionalPanel(
        condition = "input.beleggingsdoelstelling_toelichting.includes('graag') || input.beleggingsdoelstelling_toelichting.includes('please')",
        #condition = "input.beleggingsdoelstelling_toelichting == `ja graag - hieronder opent een tekstvakje waarin u een toelichting kunt schrijven`",
        list(
          #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
          h5(
            tagQuery(
              textInput(inputId = "doel_toelichting1",
                    label = if(entiteit() %in% c("bv", "st", "ve", "ov")) {
                          paste(vertaler$t("beleggingsdoelstelling_org_toelichten"), de_org_aanduiding(), vertaler$t("nader_toelichten"))
                      } else {vertaler$t("hier_kunt_u_beleggingsdoelstelling_nader_toelichten")},
                    value = "...",
                    width = '6500px',
                    placeholder = "..."))$find("input")$addAttrs(autocomplete = "off")$allTags()
          )
          )
        )
  })

  output$ui_input_startvermogen <- renderUI({
    fluidRow(
      column(5,
      bedragenInput(
        inputId = "startvermogen",
        label = str_c("1) ", vertaler$t("hoe_groot_is_het_startvermogen")),#Hoe groot is het te beleggen vermogen?",
        value = if(test_modus()) {1000000} else {0})
    ))
  })
  output$ui_input_stortingen_onttrekkingen <- renderUI({
  fluidRow(
    column(5,
           shinyWidgets::radioGroupButtons(
             inputId = "keuze_SO",
             label = str_c("2) ", vertaler$t("vraag_periodieke_so")),
             choices = c(vertaler$t("nee_zonder_komma"),
                         setNames("ja_bijstorten", (vertaler$t("jaarlijks_bijstorten"))),
                         setNames("nee_onttrekken", (vertaler$t("jaarlijks_onttrekken")))
             ),
             justified = TRUE,
             status = "neutraal",
             width = 800)))
  })

  output$ui_input_bijstorting <- renderUI({
     bedragenInput(inputId = "bijstorting", label = vertaler$t("hoe_groot_bijstorting"), value = 0)
  })
  output$ui_input_onttrekking <- renderUI({
    bedragenInput(inputId = "onttrekking", label = vertaler$t("hoe_groot_onttrekking"), value = 0)
  })

  # #=== aanhef vragen financiele_situatie
  output$ui_aanhef_financiele_situatie <- renderPrint({
    if(entiteit() %in% c("bv", "st", "ve", "ov")) {
      p(
        h4(paste(vertaler$t("doel_volgende_vragen_indruk_mate_waarin"), de_org_aanduiding()), tags$b(vertaler$t("kan")),
           paste(vertaler$t("met_de_beleggingen_risico")), vertaler$t("lopen_gezien_de_financiele_situatie"),
              paste(vertaler$t("volgende_pagina_aangeven_in_hoeverre"), de_org_aanduiding()), tags$b(vertaler$t("bereid_is")),
              vertaler$t("beleggingsrisico_te_nemen"))
      )
    } else {
      p(
        h4(paste(vertaler$t("doel_volgende_vragen_indruk_mate_waarin_u")), tags$b(vertaler$t("kunt")), vertaler$t("lopen_gezien_de_financiele_situatie_op_de_volgende_pagina"),
           tags$b(vertaler$t("wilt")), vertaler$t("nemen"))
        )

    }
  })

  #=== aanhef vragen risicobereidheid
  output$ui_aanhef_risicobereidheid <- renderUI({
    if(entiteit() %in% c("bv", "st", "ve", "ov")) {
      p(
        h4(paste(vertaler$t("op_deze_pagina_vragen_over_de_mate_waarin"), de_org_aanduiding()), tags$b(vertaler$t("bereid_is")),
           vertaler$t("risico_te_nemen_met_de_beleggingen"))
      )
      } else {
        p(
          h4(vertaler$t("op_deze_pagina_vragen_over_de_mate_waarin_u"), tags$b(vertaler$t("bereid_bent")), vertaler$t("beleggingsrisico_te_nemen"))
        )
      }
    })

  #=== einde vragen etc

  #== UI is afh van keuze database, dat is op te lossen met renderUI in de server
  selectizeInput01 <- function(id = 1) {
    h5(
      style = "margin-top: 0rem; margin-bottom: 0rem; line-height: 1.3;",  # ← adjust these values
      selectizeInput(
      inputId = onderwerp_vraag(id),
      label = str_c(sub_nummer_vraag(id), ") ", vraag(id)),
      choices = opties_vraag(id),
      selected = if(test_modus()) {selected_vraag(id)} else {NULL},
      width = '100%')
    )
  }

  #aparte voor visuele keuzes 1 en 2, met Setnames voor meerdere talen
  output$ui_selectize_visuele_keuze1 <- renderUI ({
    selectizeInput(inputId = "visuele_keuze1",
                   label = vertaler$t("welke_figuur_in_eerste_instantie"),
                   choices = c(vertaler$t("maak_keuze"),
                               setNames("de_linker_illustratie", vertaler$t("de_linker_illustratie")),
                               setNames("de_rechter_illustratie", vertaler$t("de_rechter_illustratie"))),
                   selected = if(test_modus()) {"de_linker_illustratie"} else {vertaler$t("maak_keuze")},
                   width = '600px'
    )
  })
  output$ui_selectize_visuele_keuze2 <- renderUI ({
    selectizeInput(inputId = "visuele_keuze2",
                   label = vertaler$t("spreekt_een_van_beide_u_meer_aan"),
                   choices = c(vertaler$t("maak_keuze"),
                               setNames("de_linker_illustratie", vertaler$t("de_linker_illustratie")),
                               setNames("de_midden_illustratie", vertaler$t("de_midden_illustratie")),
                               setNames("de_rechter_illustratie", vertaler$t("de_rechter_illustratie"))),
                   selected = if(test_modus()) {"de_midden_illustratie"} else {vertaler$t("maak_keuze")},
                   width = '600px'
    )
  })


  output$ui_selectize_beleggingsdoelstelling <- renderUI ({
    tagList(map(vraagnummers_categorie("beleggingsdoelstelling"), selectizeInput01))
  })
  output$ui_selectize_overig <- renderUI ({
    tagList(map(vraagnummers_categorie("overig"), selectizeInput01))
  })
  output$ui_selectize_financiele_situatie <- renderUI ({
    tagList(map(vraagnummers_categorie("financiele_situatie"), selectizeInput01))
  })
  output$ui_selectize_kennis_ervaring <- renderUI ({
    tagList(map(vraagnummers_categorie("kennis_ervaring"), selectizeInput01))
  })
  output$ui_selectize_risicobereidheid <- renderUI ({
    tagList(map(vraagnummers_categorie("risicobereidheid"), selectizeInput01))
  })

  output$ui_barplot_een_module <- renderUI ({
    barplot_een_module_UI(id = "een", vertaler = vertaler)
  })
  output$ui_barplot_twee_module <- renderUI ({
    barplot_twee_module_UI(id = "twee", vertaler = vertaler)
  })

  

  output$ui_selectize_duurzaamheid_1_en_2 <- renderUI ({
    tagList(map(vraagnummers_categorie("duurzaamheid")[1:2], selectizeInput01))
  })
  output$ui_selectize_duurzaamheid_3 <- renderUI ({
    selectizeInput01(vraagnummers_categorie("duurzaamheid")[3])
  })
  output$ui_selectize_duurzaamheid_4 <- renderUI ({
    selectizeInput01(vraagnummers_categorie("duurzaamheid")[4])
  })
  output$ui_selectize_private_beleggingen <- renderUI ({
    tagList(map(vraagnummers_categorie("private_beleggingen"), selectizeInput01))
  })
  output$ui_naar_voorlopige_conclusie <- renderUI (expr = if (download$aantal_downloads > 0) {
    #download$aantal_downloads is een tellertje met het aantal malen dat download is gegenereerd
    actionButton(class = "werk_button", 'jumpToP_voorlopige_conclusie', label = vertaler$t("voorlopige_conclusie"))
  } else {
   NULL
  })

  #==== panels
  panels <- tribble(~panel,
                    "taal",
                    "start",
                    "naam",
                    "beleggingsdoelstelling",
                    "financiele_situatie",
                    "kennis_ervaring",
                    "risicobereidheid",
                    "startvermogen",
                    "visuele_keuze1",
                    "visuele_keuze2",
                    "duurzaamheid",
                    "private_beleggingen",
                    "overig",
                    "rapport",
                    "voorlopige_conclusie",
                    "tabel",
                    "projecties"
                    )

  #panel_uitschakelen <- function(panel) {
  #  eval(parse(str_c("shinyjs::disable(selector = '.navbar-nav a[data-value = \"panel_", panel, "\"]')")))
  #}
  #alle_panels_uitschakelen
  #pmap(panels, panel_uitschakelen)
  #mislukt

  alle_panels_uitschakelen <- function() {
    panels <- c("panel_taal", "panel_start", "panel_beleggingsdoelstelling", "panel_financiele_situatie",
                "panel_kennis_ervaring", "panel_risicobereidheid", "panel_startvermogen",
                "panel_visuele_keuze1", "panel_visuele_keuze2", "panel_duurzaamheid",
                "panel_overig", "panel_rapport", "panel_voorlopige_conclusie",
                "panel_projecties", "panel_tabel")

    for (p in panels) {
      shinyjs::disable(selector = sprintf('.navbar-nav a[data-value = "%s"]', p))
    }
  }

  # Call at startup
  alle_panels_uitschakelen()

  thematic::thematic_shiny()

  #past ggplot aan aan overall theme
  thematic::thematic_shiny()

  #==== back_buttons
  my_backbutton_observeEvent <- function(panel) {
    observeEvent(input[[paste0("jump_back_ToP_", panel)]], {
      updateNavbarPage(session, inputId = "tabset",
                        selected = str_c("panel_", panel))
    })
  }
  #=== terug springen naar ...
  pmap(panels, my_backbutton_observeEvent)

  #=== vanaf einde weer naar begin springen
  observeEvent(input$jumpToP_start, {
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_start"]')
    updateTabsetPanel(session, inputId = "tabset",
                      selected = "panel_start")
  })

  observeEvent(input$jumpToP_taal, {
      #drie tabs met de resultaten weer disablen, private beleggingen idem
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_private_beleggingen"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_voorlopige_conclusie"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_tabel"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_projecties"]')
    #drie tabs met de resultaten weer verwijderen, private beleggingen idem
    removeTab(inputId = "tabset", target = "panel_private_beleggingen", session = getDefaultReactiveDomain())
    removeTab(inputId = "tabset", target = "panel_voorlopige_conclusie", session = getDefaultReactiveDomain())
    removeTab(inputId = "tabset", target = "panel_tabel", session = getDefaultReactiveDomain())
    removeTab(inputId = "tabset", target = "panel_projecties", session = getDefaultReactiveDomain())
  #  updateTabsetPanel(session, inputId = "tabset", selected = "panel_start")
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_taal")
    alle_panels_uitschakelen()
    #tellertjes weer op nul
    download$aantal_downloads <- 0
    geklikt_op_voorlopige_conclusie$geklikt <- 0
    private_beleggingen_tab_geopend$geopend <- 0
    reset("startvermogen")
    })
  #===
  observeEvent(input$jumpToP_beleggingsdoelstelling, {
    output$error_msg_start <- renderText({
      shiny::validate(
        shiny::need(input$naam != '...', vertaler$t("uw_naam")),
        shiny::need(#!TRUE %in% str_detect(input$emailadres, c(" ","!", "&","$", "^", "#", ":", ";", "<", ">", "|", "=","§")) &
          #!TRUE %in% str_detect(input$emailadres, c(" ","..", "#")) &
            !FALSE %in% str_detect(input$emailadres, c("@","\\.")) &
                       (str_locate(input$emailadres, "@")[[1]] < #laatste punt moet komen na de ampersand
                        str_locate_all(input$emailadres, "\\.")[[1]] %>% as_tibble() %>% tail(1) %>% select(start) %>% pull()),
            vertaler$t("voer_geldig_emailadres_in")), #'Voer een geldig e-mailadres in. Het moet een "@" bevatten en daarna nog een punt'),
        #naam van de entiteit
        #shiny::need(input$entiteit != "Maak hieronder een keuze", "Voor wie vult u dit formulier in?"),
        shiny::need(input$entiteit != vertaler$t("maak_keuze"), vertaler$t("voor_wie")),
        if(input$entiteit %in% c("bv", "st", "ve", "ov"))
          {shiny::need(input$naam_entiteit != '...', paste(vertaler$t("de_naam_van"), de_org_aanduiding()))},
         #paste vanwege de spatie
         #bij bv, st of ve of ov moet een antwoord gegeven worden op de vraag over het bel statuut
        if(input$entiteit %in% c("st", "ve", "ov"))
          {shiny::need(input$beleggingsstatuut != vertaler$t("maak_keuze"), vertaler$t("maak_een_keuze"))},
        if(input$entiteit %in% c("bv"))
          {shiny::need(input$pensioenbrief != vertaler$t("maak_keuze"), vertaler$t("maak_een_keuze"))}
      )
    })
      shiny::req(input$naam != "...",
                 input$entiteit != vertaler$t("maak_keuze"),
                  #  !TRUE %in% str_detect(input$emailadres, c(" ","..", "#")) &
                 !FALSE %in% str_detect(input$emailadres, c("@","\\.")) &
                    (str_locate(input$emailadres, "@")[[1]] < #laatste punt moet komen na de ampersand
                     str_locate_all(input$emailadres, "\\.")[[1]] %>% as_tibble() %>% tail(1) %>% select(start) %>% pull()))
                 if(input$entiteit %in% c("bv" ,"st", "ve", "ov")) {
                    shiny::req(input$naam_entiteit != "...")}
                 if(input$entiteit %in% c("st", "ve", "ov")) {
                    shiny::req(input$beleggingsstatuut != vertaler$t("maak_keuze"))}
                  if(input$entiteit %in% c("bv")) {
                    shiny::req(input$pensioenbrief != vertaler$t("maak_keuze"))}
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_beleggingsdoelstelling"]')
    updateTabsetPanel(session, inputId = "tabset",
                      selected = "panel_beleggingsdoelstelling")
  })
  #===
  #functies om 1) de doublures in de error afvang functies te voorkomen
  #en om 2) flexibiliteit in te bouwen voor een verschillende aantal vragen per categorie
  #voor verschillende entiteiten (bv. en/of vs prive en b.v. en stichtingen )
  #uitkomst van onderstaand is bijv. input$inkomen - alle antwoorden moeten gemaakt worden - zie validate_need_functie
  eval_parse_etc <- function(i, categorie) {
    eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie(categorie = categorie)[i]))))
  }

  #valideren van input - input moet een keuze zijn ipv "Maak een keuze"
  #mapt over alle vragen per categorie - aantal vragen daarbinnen is flexibel
  validate_need_functie <- function(categorie) {
    aantal_vragen <- length(vraagnummers_categorie(categorie = categorie))
    shiny::validate(
      shiny::need(!opties_vraag(vraagnummers_categorie(categorie = categorie)[1])[1] %in% #"Maak hieronder een keuze:"
                 # map_chr(1:length(vraagnummers_categorie(categorie = categorie)), eval_parse_etc, categorie = categorie),
                 map_chr(1:aantal_vragen, eval_parse_etc, categorie = categorie),
                if(aantal_vragen > 1 ) {vertaler$t("maak_keuze_alle_bovenstaande_vragen")
                } else {vertaler$t("maak_een_keuze")}))
  }
  req_functie <- function(categorie) {
    shiny::req(!opties_vraag(vraagnummers_categorie(categorie = categorie)[1])[1] %in%
                 map_chr(1:length(vraagnummers_categorie(categorie = categorie)), eval_parse_etc, categorie = categorie))
  }
  #===
# poging om oderstaande observeEvent s in functie te gieten - mislukt
#    my_observeEvent2 <- function(from_panel) {
#       observeEvent(input[[paste0("jump_ToP_", panels$panel[which(panels$panel == from_panel) + 1])]], {
#         output[[paste0("error_msg_", from_panel)]] <- renderText({
#           validate_need_functie(from_panel)
#         })
#       req_functie(from_panel)
#       shinyjs::enable(selector = paste0("'.navbar-nav a[data-value = panel_", panels$panel[which(panels$panel == from_panel) + 1], "]'"))
#       updateTabsetPanel(session, inputId = "tabset", selected = paste0("panel_", panels$panel[which(panels$panel == from_panel) + 1]))
#    })
# }
# #
#    #pmap(panels, my_observeEvent2)
#   my_observeEvent2(from_panel = "beleggingsdoelstelling")

  observeEvent(input$jumpToP_financiele_situatie, {
    output$error_msg_beleggingsdoelstelling <- renderText({
      validate_need_functie("beleggingsdoelstelling")
    })
    req_functie("beleggingsdoelstelling")
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_financiele_situatie"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_financiele_situatie")
  })
  #==
  observeEvent(input$jumpToP_kennis_ervaring, {
    output$error_msg_financiele_situatie <- renderText({
      validate_need_functie("financiele_situatie")
    })
    req_functie("financiele_situatie")
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_kennis_ervaring"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_kennis_ervaring")
  })
  #===
  observeEvent(input$jumpToP_risicobereidheid, {
    output$error_msg_kennis_ervaring <- renderText({
      validate_need_functie("kennis_ervaring")
    })
    req_functie("kennis_ervaring")
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_risicobereidheid"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_risicobereidheid")
  })
  #===
  observeEvent(input$jumpToP_startvermogen, {
    output$error_msg_risicobereidheid <- renderText({
      validate_need_functie("risicobereidheid")
    })
    req_functie("risicobereidheid")
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_startvermogen"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_startvermogen")
  })
  #===
  observeEvent(input$jumpToP_visuele_keuze1, {
    output$error_msg_startvermogen <- renderText({
      shiny::validate(
        shiny::need(input$startvermogen > 0, vertaler$t("voer_bedrag_in")),
        shiny::need(is.null(input$onttrekking) || input$onttrekking <= input$startvermogen / 10, vertaler$t("onttrekking_moet_kleiner_zijn_dan_tien_pct_startvermogen"))    )
    })
    shiny::req(input$startvermogen > 0)
    shiny::req(is.null(input$onttrekking) || input$onttrekking <= input$startvermogen/10)
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_visuele_keuze1"]')
    #hier Extra tab private beleggingen removen als het al bestaat
    if (private_beleggingen_uitvragen() == FALSE & private_beleggingen_tab_geopend$geopend == 1) {
      removeTab(inputId = "tabset", target = "panel_private_beleggingen", session = getDefaultReactiveDomain())
      private_beleggingen_tab_geopend$geopend <- 0
    }
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_visuele_keuze1")
  })
  #===
  observeEvent(input$jumpToP_visuele_keuze2, {
    output$error_msg_visuele_keuze1 <- renderText({
      shiny::validate(
        shiny::need(#eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("visueel")[1])))) !=
                     # opties_vraag(vraagnummers_categorie("visueel")[1])[1],
                    input$visuele_keuze1 != vertaler$t("maak_keuze"),
                    vertaler$t("maak_een_keuze")
                    ))
    })
    shiny::req(#eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("visueel")[1])))) !=
                 #opties_vraag(vraagnummers_categorie("visueel")[1])[1])
      input$visuele_keuze1 != vertaler$t("maak_keuze"))
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_visuele_keuze2"]')
    delay(1000, #niet te snel drukken, anders "floept ie eruit"
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_visuele_keuze2")
    )
  })
  #===
  observeEvent(input$jumpToP_duurzaamheid, {
    output$error_msg_visuele_keuze2 <- renderText({
      shiny::validate(
        shiny::need(#eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("visueel")[1])))) !=
          # opties_vraag(vraagnummers_categorie("visueel")[1])[1],
          input$visuele_keuze2 != vertaler$t("maak_keuze"),
          vertaler$t("maak_een_keuze")
        ))
    })
    shiny::req(#eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("visueel")[1])))) !=
      #opties_vraag(vraagnummers_categorie("visueel")[1])[1])
      input$visuele_keuze2 != vertaler$t("maak_keuze"))
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_duurzaamheid"]')
        updateTabsetPanel(session, inputId = "tabset", selected = "panel_duurzaamheid")
  })

  #===

  private_beleggingen_tab_geopend <- reactiveValues()
  private_beleggingen_tab_geopend$geopend <- 0

  observeEvent(input$jumpToP_overig, {
    # --- helper: safely get selected input for duurzaamheid ---
    selected_duurzaamheid <- function(i) {
      input[[onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[i])]]
    }
    
    # --- ERROR MESSAGE ---
    output$error_msg_duurzaamheid <- renderText({
      if (meer_vragen_duurzaamheid()) {
        validate_need_functie("duurzaamheid")
      } else {
        verboden_optie <- opties_vraag(
          vraagnummers_categorie("duurzaamheid")[1]
        )[1]
        gekozen <- c(
          selected_duurzaamheid(1),
          selected_duurzaamheid(2)
        )
        shiny::validate(
          shiny::need(
            !verboden_optie %in% gekozen,
            vertaler$t("maak_keuze_alle_bovenstaande_vragen")
          )
        )
      }
      
      ""
    })
    
    # --- HARD BLOCKING VALIDATION ---
    if (meer_vragen_duurzaamheid()) {
      req_functie("duurzaamheid")
    } else {
      verboden_optie <- opties_vraag(
        vraagnummers_categorie("duurzaamheid")[1]
      )[1]
      gekozen <- c(
        selected_duurzaamheid(1),
        selected_duurzaamheid(2)
      )
      shiny::req(!verboden_optie %in% gekozen)
    }
    
    # --- INSERT PRIVATE BELEGGINGEN TAB (ONCE) ---
    if (
      isTRUE(private_beleggingen_uitvragen()) &&
      private_beleggingen_tab_geopend$geopend == 0
    ) {
      
      bslib::nav_insert(
        id  = "tabset",
        target  = "panel_duurzaamheid",
        position = "after",
        session = session,
        nav_panel(title = "Extra", value = "panel_private_beleggingen", class = "p-4",
        card(
            card_header(
              h3(uiOutput("ui_private_beleggingen"))
            ),
           card_body(
              h5(uiOutput("ui_private_beleggingen_zijn_intro")),
              h5(uiOutput("ui_private_beleggingen_zijn_heel_divers")),
              h5(uiOutput("ui_private_beleggingen_voordelen")),
              h5(uiOutput("ui_private_beleggingen_nadelen")),
              h5(uiOutput("ui_private_beleggingen_kosten")),
              h5(uiOutput("ui_private_beleggingen_duidelijkheid_horizon")),
              h5(uiOutput("ui_private_beleggingen_mogelijk_geschikt")),
              h5(uiOutput("ui_private_beleggingen_bavd_kan_helpen")),
              h5(uiOutput("ui_hieronder_vraag_interesse_private_beleggingen")),
              uiOutput("ui_selectize_private_beleggingen"),
              textOutput("error_msg_private_beleggingen")
            ),
           card_footer(
             layout_columns(
               col_widths = breakpoints(sm = c(12, 12, 12), md = c(3, 6, 3)),
                actionButton("jump_back_ToP_duurzaamheid", vertaler$t("vorige_pagina"), class = "back_button"),
               NULL,
                actionButton("jumpToP_overig2", vertaler$t("volgende_pagina"), class = "button")
              )
           )
            )
          )
        )
    }
      private_beleggingen_tab_geopend$geopend <- 1
    
    
    # --- NAVIGATE (bslib-style) ---
    if (isTRUE(private_beleggingen_uitvragen())) {
      shinyjs::enable(
        selector = '.nav-link[data-value="panel_private_beleggingen"]'
      )
      bslib::nav_select(
        id = "tabset",
        selected = "panel_private_beleggingen",
        session = session
      )
      
    } else {
      
      shinyjs::enable(
        selector = '.nav-link[data-value="panel_overig"]'
      )
      
      bslib::nav_select(
        id = "tabset",
        selected = "panel_overig",
        session = session
      )
    }
  })
  
  observeEvent(input$jumpToP_overig2, {
    output$error_msg_private_beleggingen <- renderText({
      validate_need_functie("private_beleggingen")
    })
    req_functie("private_beleggingen")
      shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_overig"]')
      updateTabsetPanel(session, inputId = "tabset", selected = "panel_overig")
  })
  #===
  observeEvent(input$jumpToP_rapport, {
    output$error_msg_overig <- renderText({
      validate_need_functie("overig")
    })
    req_functie("overig")
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_rapport"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_rapport")
  })

  #========

  geklikt_op_voorlopige_conclusie <- reactiveValues()
  geklikt_op_voorlopige_conclusie$geklikt <- 0

  observeEvent(input$jumpToP_voorlopige_conclusie, {
    if (geklikt_op_voorlopige_conclusie$geklikt == 0) {
      bslib::nav_insert(
        id  = "tabset",
        target  = "panel_rapport",
        position = "after",
        session = session,
        bslib::nav_panel(
          title = span(vertaler$t("voorlopige_conclusie"), class = "custom-tab"),
          value = "panel_voorlopige_conclusie",
          class = "p-4",
          
          card(
            card_header(
              layout_columns(
                col_widths = breakpoints(
                  xs = c(12, 12, 12),
                  sm = c(4, 4, 4),
                  md = c(3, 6, 3)
                ),
                
                actionButton(
                  class = "back_button",
                  inputId = 'jump_back_ToP_rapport',
                  label = vertaler$t("vorige_pagina")
                ),
                
                NULL,
                
                actionButton(
                  class = "werk_button",
                  inputId = 'jumpToP_projecties',
                  label = vertaler$t("projecties")
                ),
                
                div()  # placeholder for symmetry
              )
            ),
            
            card_body(
              h3(vertaler$t("resultaat_per_categorie")),
              # First table – centered (unchanged from before)
              div(
                style = "display: flex; justify-content: center; margin: 1rem 0;",
                shinycssloaders::withSpinner(
                  p(style = "text-align: justify; max-width: 900px;",
                    htmlOutput("check_pagina")
                  ),
                  color = euroblue
                )
              ),
              
              h5(textOutput('profiel_conclusie_tekst')),
              h5(vertaler$t("vertaling_punten_naar_profiel_uitleg")),
              
              # 5 translation tables – side by side, natural width, title on top
              div(
                style = "display: flex; flex-wrap: wrap; gap: 2rem; justify-content: center; margin-top: 1.5rem;",
                
                # Each table gets its own container with natural width
                div(
                  style = "flex: 0 1 auto; min-width: 220px; max-width: 380px;",  # adjust min/max as needed
                  h5(style = "text-align: center; margin-bottom: 0.6rem; font-weight: 600;", 
                     vertaler$t("beleggingsdoelstelling")),  # ← title ABOVE
                  htmlOutput("vertaaltabel_beleggingsdoelstelling")
                ),
                
                div(
                  style = "flex: 0 1 auto; min-width: 220px; max-width: 380px;",
                  h5(style = "text-align: center; margin-bottom: 0.6rem; font-weight: 600;", 
                     vertaler$t("financiele_situatie")),
                  htmlOutput("vertaaltabel_financiele_situatie")
                ),
                
                div(
                  style = "flex: 0 1 auto; min-width: 220px; max-width: 380px;",
                  h5(style = "text-align: center; margin-bottom: 0.6rem; font-weight: 600;", 
                     vertaler$t("risicobereidheid")),
                  htmlOutput("vertaaltabel_risicobereidheid")
                ),
                
                div(
                  style = "flex: 0 1 auto; min-width: 220px; max-width: 380px;",
                  h5(style = "text-align: center; margin-bottom: 0.6rem; font-weight: 600;", 
                     vertaler$t("visueel")),
                  htmlOutput("vertaaltabel_visueel")
                ),
                
                div(
                  style = "flex: 0 1 auto; min-width: 220px; max-width: 380px;",
                  h5(style = "text-align: center; margin-bottom: 0.6rem; font-weight: 600;", 
                     vertaler$t("kennis_ervaring")),
                  htmlOutput("vertaaltabel_kennis_ervaring")
                )
              )
            ),
            
            card_footer(
            )
          )
        )
    )
      # New: Projecties
      bslib::nav_insert(
        id  = "tabset",
        target  = "panel_voorlopige_conclusie",
        position = "after",
        session = session,
        bslib::nav_panel(
          title = span(vertaler$t("projecties"), class = "custom-tab"),
          value = "panel_projecties",
          class = "p-4",
          
          card(
            card_header(              
              layout_columns(
                col_widths = breakpoints(
                  xs = c(12, 12, 12),     # stack on very small screens
                  sm = c(4, 4, 4),        # roughly even on small
                  md = c(3, 6, 3)         # desired 3-6-3 on medium and up
                ),
                
                actionButton(
                  class = "back_button",
                  inputId = 'jump_back_ToP_voorlopige_conclusie',
                  label = vertaler$t("vorige_pagina")
                ),
                
                NULL,
                
                actionButton(
                  class = "werk_button",
                  inputId = 'jumpToP_tabel',
                  label = vertaler$t("tabel")
                ),
                
                # empty slot to maintain the 3-6-3 proportion
                div()   # or tags$div() – can later become another button / spacer
              )
            ),
            card_body(
              h3(vertaler$t("projecties_in_rapport")),
              h4(vertaler$t("voor_toelichting_zie_rapport")),
              scenariosplot_module_UI(
                id = "scenariosplot_module",
                vertaler = vertaler
              )
            ),
            card_footer(
            )
          )
        )
      )

      # New: Tabel
      bslib::nav_insert(
        id  = "tabset",
        target  = "panel_projecties",
        position = "after",
        session = session,
        bslib::nav_panel(
          title = span(vertaler$t("tabel"), class = "custom-tab"),
          value = "panel_tabel",
          class = "p-4",
          
          card(
            card_header(
              layout_columns(
                col_widths = breakpoints(
                  xs = c(12, 12, 12),           # stack on very small screens
                  sm = c(4, 4, 4),              # roughly even on small
                  md = c(3, 6, 3)               # your desired 3-6-3 on medium+
                ),
                
                actionButton(
                  class = "back_button",
                  inputId = 'jump_back_ToP_projecties',
                  label = vertaler$t("vorige_pagina")
                ),
                
                NULL,
                
                actionButton(
                  class = "werk_button",
                  inputId = 'jumpToP_taal',
                  label = vertaler$t("ga_terug_naar_start")
                ),
                
                # empty slot or placeholder if you want symmetry / future button
                div()   # or tags$div() or another button if needed
              )
            ),
            
            card_body(
              h3(vertaler$t("tabel_in_rapport")),
              h4(vertaler$t("voor_toelichting_zie_rapport")),
              
              shinycssloaders::withSpinner(
                tableOutput("tabel_rapport"),
                color = euroblue
              )
            ),
            
            card_footer(
            )
          )
        )
      )
    }
  shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_voorlopige_conclusie"]')
  updateTabsetPanel(session, inputId = "tabset", selected = "panel_voorlopige_conclusie")
  geklikt_op_voorlopige_conclusie$geklikt <- 1
  })
  #===
  observeEvent(input$jumpToP_tabel, {
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_tabel"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_tabel")
  })
  observeEvent(input$jumpToP_projecties, {
    shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_projecties"]')
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_projecties")
  })
  #==


  #===
  #grafieken info1
  tags$head(
    tags$style(HTML("
    /* Forceer perfecte verticale + horizontale centering voor alle modals */
    .modal {
      display: flex !important;
      align-items: center !important;     /* Verticaal centreren */
      justify-content: center !important; /* Horizontaal centreren */
      min-height: 100vh !important;       /* Volledige schermhoogte */
      padding: 0 !important;
    }

    .modal-dialog {
      margin: 0 auto !important;          /* Geen automatische margins */
      max-width: 800px !important;        /* Jouw gewenste breedte */
      width: 90% !important;              /* Responsive op kleine schermen */
    }

    .modal-content {
      box-shadow: 0 5px 15px rgba(0,0,0,0.5);
    }
  "))
  )

  observeEvent(input$grafieken_info1, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,

      tags$div(
        style = "text-align: center; max-width: 800px; margin: 0 auto; color: #808080; font-size: 18px; padding: 30px;",
        # Logo bovenaan
        tags$div(
          style = "margin-bottom: 30px;",
          tags$img(src = "logo.png", width = "300px", style = "display: block; margin: 0 auto;")
        ),
        tags$h3(vertaler$t("toelichting_grafieken"), style = "margin-bottom: 20px;"),
        tags$p(
          vertaler$t("grafieken_toelichting_intro"), tags$br(),
          vertaler$t("getoonde_bedragen_in_toelichting_grafieken"),
          format_bedrag_tabellen(startvermogen(), big_mark(), decimal_mark()), ". ",
          vertaler$t("eventuele_so_buiten_beschouwing_gelaten"),
          tags$br(),
          vertaler$t("toelichting_grafieken_gunstig_ongunstig"),
          vertaler$t("toelichting_grafieken_grotere_spreiding")
        )
      ),
      # Footer met gecentreerde OK-knop (onderaan in het midden)
      footer = tags$div(
        style = "text-align: center; width: 100%;",
        actionButton(
          "modal_ok",
          #vertaler$t("ok_button"),
          "OK",
          style = "background-color: #94bcb2; color: white; border: none; padding: 10px 40px; font-size: 16px;"
        )
      )
    ))
  })

  # Sluit modal bij klik op OK, werkt voor alle dialoogjes
  observeEvent(input$modal_ok, {
    removeModal()
  })

  observeEvent(input$grafieken_info2, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,

      tags$div(
        style = "text-align: center; max-width: 800px; margin: 0 auto; color: #808080; font-size: 18px; padding: 30px;",
        # Logo bovenaan
        tags$div(
          style = "margin-bottom: 30px;",
          tags$img(src = "logo.png", width = "300px", style = "display: block; margin: 0 auto;")
        ),
        tags$h3(vertaler$t("toelichting_grafieken"), style = "margin-bottom: 20px;"),
        tags$p(
          vertaler$t("grafieken_toelichting_intro2"), tags$br(),
          vertaler$t("getoonde_bedragen_in_toelichting_grafieken"),
          format_bedrag_tabellen(startvermogen(), big_mark(), decimal_mark()), ". ",
          vertaler$t("eventuele_so_buiten_beschouwing_gelaten"),
          tags$br(),
          vertaler$t("toelichting_grafieken_gunstig_ongunstig"),
          vertaler$t("toelichting_grafieken_grotere_spreiding2")
        )
      ),
      # Footer met gecentreerde OK-knop (onderaan in het midden)
      footer = tags$div(
        style = "text-align: center; width: 100%;",
        actionButton(
          "modal_ok",
          #vertaler$t("ok_button"),
          "OK",
          style = "background-color: #94bcb2; color: white; border: none; padding: 10px 40px; font-size: 16px;"
        )
      )
    ))
  })


  #functie voor aantal punten die vraag x oplevert
  aantal_punten_vraag_x <- function(x) {
    punten_antwoord_vraag(x, eval(parse(text = str_c("input$", onderwerp_vraag(x)))))
  }

  aantal_punten_categorie <- function(categorie) {
   map(vraagnummers_categorie(categorie = categorie), aantal_punten_vraag_x) %>% unlist() %>% sum()
  }

  aantal_punten_beleggingsdoelstelling <- reactive({
    map(vraagnummers_categorie("beleggingsdoelstelling"), aantal_punten_vraag_x) %>% unlist() %>% sum()
  })
  aantal_punten_financiele_situatie <- reactive({
    map(vraagnummers_categorie("financiele_situatie"), aantal_punten_vraag_x) %>% unlist() %>% sum()
  })
  aantal_punten_kennis_ervaring <- reactive({
    map(vraagnummers_categorie("kennis_ervaring"), aantal_punten_vraag_x) %>% unlist() %>% sum()
  })
  aantal_punten_risicobereidheid <- reactive({
    map(vraagnummers_categorie("risicobereidheid"), aantal_punten_vraag_x) %>% unlist() %>% sum()
  })

  #vraag 3 in risicobereidheid gaat over beleggingshorizon
  horizon <- reactive(
    if(output_antwoord(vraagnummers_categorie("risicobereidheid")[3]) == opties_vraag(vraagnummers_categorie("risicobereidheid")[3])[2])
       5 else if(output_antwoord(vraagnummers_categorie("risicobereidheid")[3]) == opties_vraag(vraagnummers_categorie("risicobereidheid")[3])[3])
      10 else
      30
  )

  meer_vragen_duurzaamheid <- reactive({
    #is het antwoord op vraag 2 (+1 voor "Maak een keuze") ja, ik wil duurzaamheid
    output_antwoord(vraagnummers_categorie("duurzaamheid")[2]) == opties_vraag(vraagnummers_categorie("duurzaamheid")[2])[3]
  })

  private_beleggingen_uitvragen <- reactive({
    #uitvragen als
    #startvermogen >= 2000000 en horizon >= 5 en antw op "deel" is een kwart of minder OF
     (startvermogen() >= 2000000 & horizon() >= 5 &
        output_antwoord(vraagnummers_categorie("financiele_situatie")[3]) == opties_vraag(vraagnummers_categorie("financiele_situatie")[3])[2]) | #een hoger want 1e antw is "Maak een keuze"
    # #startvermogen >= 3000000 en horizon >= 5 en antw op "deel" is is ongeveer de helft of een kwart of minder OF
     (startvermogen() >= 3000000 & horizon() >= 5 &
        output_antwoord(vraagnummers_categorie("financiele_situatie")[3]) == opties_vraag(vraagnummers_categorie("financiele_situatie")[3])[2]) |
      (startvermogen() >= 3000000 & horizon() >= 5 &
        output_antwoord(vraagnummers_categorie("financiele_situatie")[3]) == opties_vraag(vraagnummers_categorie("financiele_situatie")[3])[3]) |
      #    (output_antwoord(vraagnummers_categorie("financiele_situatie")[3]) == opties_vraag(vraagnummers_categorie("financiele_situatie")[3])[3])) |
    # #startvermogen >= 5000000 en horizon >= 5
    (startvermogen() >= 5000000 & horizon() >= 5)
  })

  #tabellen maken aantal punten naar profielen, per categorie
  vertaaltabel_punten_profiel_categorie <- function(categorie) {
    min_punten <- max_min_punten_categorie(categorie, "min")
    max_punten <- max_min_punten_categorie(categorie, "max")
    tribble(~punten,
             seq(min_punten, max_punten, 1)) %>%
      unnest(cols = punten) %>%
      mutate(profiel_nummer =
               case_when(
                 punten < min_punten + (max_punten - min_punten)*1/5 ~ 1,
                 punten < min_punten + (max_punten - min_punten)*2/5 ~ 2,
                 punten < min_punten + (max_punten - min_punten)*3/5 ~ 3,
                 punten < min_punten + (max_punten - min_punten)*4/5 ~ 4,
                 TRUE ~ 5)
      ) %>%
      rowwise %>%
      mutate(profiel = profiel_levels[profiel_nummer]) %>%
      select(punten, profiel)
  }
  #profiel opzoeken bij aantal punten, per categorie
  profiel_categorie <- function(categorie, aantal_punten_categorie) {
    vertaaltabel_punten_profiel_categorie(categorie) %>%
      filter(punten == aantal_punten_categorie) %>%
      select(profiel) %>%
      pull()
  }

 #tabellen functie maken voor pagina conclusie
 vertaaltabel <- function(categorie) {
    vertaaltabel_punten_profiel_categorie(categorie) %>%
      kbl(caption = tolower(vertaler$t(categorie)), align = "c") %>%
      kable_styling(font_size = 12, "striped", full_width = FALSE, position = "float_left") %>%
      column_spec(column = 2, width = "4cm")
 }

 #gelegenheids
 vertaaltabel_categorie <- function(categorie) {

   punten_kolom_naam <- vertaler$t("punten")
   profiel_kolom_naam <- vertaler$t("profiel")

   tabel <-
     vertaaltabel_punten_profiel_categorie(categorie) %>%
     rename(!!punten_kolom_naam := punten,
            !!profiel_kolom_naam := profiel)

   tabel %>%
     kbl(caption = NULL, #tolower(vertaler$t(categorie)), 
         align = "c") %>%
     row_spec(which(tabel[[punten_kolom_naam]] == aantal_punten_categorie(categorie)),
              color = euroblue, background = "lightgray", bold = TRUE) %>%
     kable_styling(font_size = 12, "striped", full_width = FALSE, position = "float_left") %>%
     column_spec(column = 2, width = "4cm")
 }

 output$vertaaltabel_beleggingsdoelstelling <- reactive({
   vertaaltabel_categorie("beleggingsdoelstelling")
  })

 #tabellen maken voor pagina conclusie
  # output$vertaaltabel_beleggingsdoelstelling <- reactive({
  #   vertaaltabel("beleggingsdoelstelling") |>
  #   row_spec(which(vertaaltabel_punten_profiel_categorie("beleggingsdoelstelling")$punten == aantal_punten_categorie("beleggingsdoelstelling")),
  #                     color = euroblue, background = "lightgray", bold = TRUE)
  # })
  output$vertaaltabel_financiele_situatie <- reactive({
    vertaaltabel_categorie("financiele_situatie")
    #vertaaltabel("financiele_situatie") |>
 #   row_spec(which(vertaaltabel_punten_profiel_categorie("financiele_situatie")$punten == aantal_punten_categorie("financiele_situatie")),
#             color = euroblue, background = "lightgray", bold = TRUE)
  })
  output$vertaaltabel_kennis_ervaring <- reactive({
    vertaaltabel_categorie("kennis_ervaring") %>%
    #vertaaltabel("kennis_ervaring") |>
    #row_spec(which(vertaaltabel_punten_profiel_categorie("kennis_ervaring")$punten == aantal_punten_categorie("kennis_ervaring")),
    #         color = euroblue, background = "lightgray", bold = TRUE) |>
    column_spec(c(1,2), italic = TRUE)
  })
  output$vertaaltabel_risicobereidheid <- reactive({
    vertaaltabel_categorie("risicobereidheid")
    #vertaaltabel("risicobereidheid") |>
    #row_spec(which(vertaaltabel_punten_profiel_categorie("risicobereidheid")$punten == aantal_punten_categorie("risicobereidheid")),
    #         color = euroblue, background = "lightgray", bold = TRUE)
  })


  output$vertaaltabel_visueel <- function() {
    #visueel is verhaal apart
    #gewoon een tibble maken
    punten_kolom_naam <- vertaler$t("punten")
    profiel_kolom_naam <- vertaler$t("profiel")

    tabel <- tribble(~punten, ~profiel, 1, "RM100",  2, "RD30RM70", 3, "RD50RM50", 4, "RD70RM30", 5, "RD100") %>%
    rename(!!punten_kolom_naam := punten,
           !!profiel_kolom_naam := profiel)

    tabel %>%
      kbl(caption = NULL, #vertaler$t("visueel"), 
          align = "c") %>%
      kable_styling(font_size = 12, "striped", full_width = FALSE, position = "float_left") %>%
      column_spec(column = 2, width = "4cm") %>%
      row_spec(which(profiel_levels == keuze_visueel2()),
               color = euroblue, background = "lightgray", bold = TRUE)
  }

  profiel_beleggingsdoelstelling <- reactive({
    profiel_categorie("beleggingsdoelstelling", aantal_punten_beleggingsdoelstelling())
  })
  profiel_financiele_situatie <- reactive({
    profiel_categorie("financiele_situatie", aantal_punten_financiele_situatie())
  })
  profiel_kennis_ervaring <- reactive({
    profiel_categorie("kennis_ervaring", aantal_punten_kennis_ervaring())
  })
  profiel_risicobereidheid <- reactive({
    profiel_categorie("risicobereidheid", aantal_punten_risicobereidheid())
  })

  tabel_profielen_per_categorie <- reactive({
    tribble(~categorie, ~`aantal vragen`, ~punten, ~min, ~max, ~profiel,

            vertaler$t("beleggingsdoelstelling_kleine_letter"),
            length(vraagnummers_categorie("beleggingsdoelstelling"))-1, #minus de vraag zonder punten
            aantal_punten_beleggingsdoelstelling(),
            max_min_punten_categorie("beleggingsdoelstelling", "min"),
            max_min_punten_categorie("beleggingsdoelstelling", "max"),
            profiel_beleggingsdoelstelling(),

            vertaler$t("financiële_situatie_kleine_letter"),
            length(vraagnummers_categorie("financiele_situatie")),
            aantal_punten_financiele_situatie(),
            max_min_punten_categorie("financiele_situatie", "min"),
            max_min_punten_categorie("financiele_situatie", "max"),
            profiel_financiele_situatie(),

            vertaler$t("risicobereidheid_kleine_letter"),
            length(vraagnummers_categorie("risicobereidheid")),
            aantal_punten_risicobereidheid(),
            max_min_punten_categorie("risicobereidheid", "min"),
            max_min_punten_categorie("risicobereidheid", "max"),
            profiel_risicobereidheid(),

            vertaler$t("visueel"),
            2, #length(vraagnummers_categorie("visueel")),
            which(profiel_levels == keuze_visueel2()),
            1,
            5,
            keuze_visueel2(),

            vertaler$t("kennis_en_ervaring_telt_niet_mee"),
            length(vraagnummers_categorie("kennis_ervaring")),
            aantal_punten_kennis_ervaring(),
            max_min_punten_categorie("kennis_ervaring", "min"),
            max_min_punten_categorie("kennis_ervaring", "max"),
            profiel_kennis_ervaring()
            ) %>%
      rename(!!vertaler$t("categorie") := categorie ,
             !!vertaler$t("aantal_vragen") := `aantal vragen`,
             !!vertaler$t("punten") := punten,
             !!vertaler$t("profiel") := profiel)
  })

  output$check_pagina <- function() {

    profiel_kolom_naam <- vertaler$t("profiel")
    categorie_kolom_naam <- vertaler$t("categorie")

    tabel_profielen_per_categorie() %>%
      kbl(caption = "", align = c("l", rep("c", 5))) %>%
      kable_styling(font_size = 15, "striped", full_width = FALSE, position = "left") %>%
      row_spec(which(tabel_profielen_per_categorie()[[profiel_kolom_naam]] == profiel_conclusie()),
               color = euroblue, background = "lightgray", bold = TRUE) |>
      row_spec(which(tabel_profielen_per_categorie()[[categorie_kolom_naam]] == vertaler$t("kennis_en_ervaring_telt_niet_mee")),
               italic = TRUE)
  }

  profiel_ex_kennis_ervaring <- reactive({
      profiel_levels[
        min(which(profiel_levels == profiel_beleggingsdoelstelling()),
            which(profiel_levels == profiel_financiele_situatie()),
            which(profiel_levels == profiel_risicobereidheid()),
            which(profiel_levels == keuze_visueel2())
          )
      ]
  })

  profiel_conclusie <- reactive({
    profiel_ex_kennis_ervaring()
  })

  output$profiel_conclusie_tekst <- renderText({
    str_c(vertaler$t("de_voorlopige_conclusie_is_de_meest_voorzichtige"),
          profiel_conclusie(), ". ", vertaler$t("vertaling_punten_naar_profiel_uitleg_toelichting"))
  })

  #visuele keuzes
  profiel_keuzes <- function(level, keuze1, keuze2) {
      if(level == 1) {
        {c(profiel_levels[4], profiel_levels[2])}
      }
      else if(level == 2) {
        if(keuze1 == "de_linker_illustratie")
          {c(profiel_levels[5], profiel_levels[3])}
          else
        {c(profiel_levels[3], profiel_levels[1])}
        }
      else if(level == 3) {
        if(keuze1 == "de_linker_illustratie" & keuze2 == "de_linker_illustratie")
        {profiel_levels[5]}
        else if(keuze1 == "de_linker_illustratie" & keuze2 == "de_midden_illustratie")
        {profiel_levels[4]}
        else if(keuze1 == "de_linker_illustratie" & keuze2 == "de_rechter_illustratie")
        {profiel_levels[3]}
        else if(keuze1 == "de_rechter_illustratie" & keuze2 == "de_linker_illustratie")
        {profiel_levels[3]}
        else if(keuze1 == "de_rechter_illustratie" & keuze2 == "de_midden_illustratie")
        {profiel_levels[2]}
        else if(keuze1 == "de_rechter_illustratie" & keuze2 == "de_rechter_illustratie")
        {profiel_levels[1]}
      }
    }

  #NAAR MODULE
    #barplots per bm
    data_voor_barcharts <-
       index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr %>%
       filter(horizon == "3 jaar")
  #===
   observeEvent(input$onttrekking, {
     reset("bijstorting")
   })
   observeEvent(input$bijstorting, {
     reset("onttrekking")
   })
  bijstorting <- reactive({
    if(input$keuze_SO == "nee_onttrekken")
     {input$onttrekking * - 1}
     else if(input$keuze_SO == "ja_bijstorten")
    {input$bijstorting}
    else {0}
   })

  startvermogen <- reactive({
    input$startvermogen
  })

  inflatie_maandbasis <- (1 + inflatie)^(1/12) - 1

  df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros <- reactive({
    #portfolio waarde in alle scenarios bij alle profielen - er is er maar een nodig
    df_bijstortingen_onttrekkingen_params_compleet_met_gem %>%
      mutate(inflatie = (1 + inflatie_maandbasis)^(maand) * startvermogen(),
             index = (1 + index) * startvermogen() + bijstorting() * bijstortingen_index,
             q95 = (1 + q95) * startvermogen() + bijstorting() * bijstortingen_q95,
             mean = (1 + mean) * startvermogen() + bijstorting() * bijstortingen_mean,
             q05 = (1 + q05) * startvermogen() + bijstorting() * bijstortingen_q05,
             min = (1 + min) * startvermogen() + bijstorting() * bijstortingen_min,
             max = (1 + max) * startvermogen() + bijstorting() * bijstortingen_max)
    })


    naam <- reactive(input$naam)
    naam_entiteit <- reactive(input$naam_entiteit)
    entiteit <- reactive(input$entiteit)
    emailadres <- reactive(input$emailadres)

    de_org_aanduiding <- reactive({
      case_when(entiteit() == "bv" ~ vertaler$t("de_bv"),#de besloten vennootschap",
                entiteit() == "st" ~ vertaler$t("de_st"),#"de stichting",
                entiteit() == "ve" ~ vertaler$t("de_ve"),#"de vereniging",
                entiteit() == "ov" ~ vertaler$t("de_ov")) #"de organisatie")
    })

    output_antwoord <- function(id = 1) {
      str_c(eval(parse(text = str_c("input$", onderwerp_vraag(id)))))
    }

    #reactive voor wegschrijven
    keuze_visueel1 <- reactive(
      if(input$visuele_keuze1 == "de_linker_illustratie") {
        profiel_keuzes(level = 1)[1]
      } else {
        profiel_keuzes(level = 1)[2]
      })

    keuze_visueel2 <- reactive(
      if(input$visuele_keuze2 == "de_linker_illustratie")
        {profiel_keuzes(level = 2, keuze1 = input$visuele_keuze1)[1]}
        #1
      else if(input$visuele_keuze2 == "de_rechter_illustratie")
        {profiel_keuzes(level = 2, keuze1 = input$visuele_keuze1)[2]}
      else
        # {profiel_levels[which(profiel_levels ==
        #                          profiel_keuzes(level = 2, keuze1 = input$keuze1)[1]) + 1]}
        keuze_visueel1()
    )

    #data voor rapport
    #regel voor in tabel vwb bijstorting
    bijstorting_antwoord_tabel <- reactive(
      if(bijstorting() > 0) {str_c(vertaler$t("jaarlijkse_bijstorting_van"), format_bedrag_tabellen_bijstorting(bijstorting(), big_mark(), decimal_mark()))}
    else if(bijstorting() < 0) {str_c(vertaler$t("jaarlijkse_onttrekking_van"), format_bedrag_tabellen_bijstorting(abs(bijstorting()), big_mark(), decimal_mark()))}
    else {vertaler$t("geen_jaarlijkse_bijstortingen_of_onttrekkingen")}
    )

    #idee maak vraag, question en antwoord,
    # selecteer Vraan en Antwoord voor beleggingsvoorstel
    # selecteer question en antwoord voor rapport
    # en hernoem Antwoord met !!vertaler$t

    data_rapport <- reactive( #deze tabel gaat naat PDF
                tibble(Vraag = c(vertaler$t("uw_naam"),
                        vertaler$t("email_adres"),
                        vertaler$t("dit_beleggersprofiel_is_ingevuld_voor"),
                        if(entiteit() %in% c("bv", "ve", "st", "ov"))
                          {str_c(vertaler$t("naam_van"), " ", de_org_aanduiding())} else {NULL}, #alleen voor st ve ov en bv anders NULL
                        if(entiteit() %in% c("ve", "st", "ov"))
                           {vertaler$t("beleggingsstatuut")}
                          else if(entiteit() == "bv")
                           {vertaler$t("pensioenbrief")}
                          else {NULL}, #eigenlijk liever helemaal geen rij dus NULL
                        vertaler$t("tijdstip_van_opsturen"),
                        str_c("I - ", vertaler$t("beleggingsdoelstelling")),
                        map_chr(vraagnummers_categorie("beleggingsdoelstelling"), vraag_met_subnummer),
                        str_c("   ", vertaler$t("toelichting_beleggingsdoelstelling")),
                        vertaler$t("profiel_obv_beleggingsdoelstelling"),
                        str_c("II - ", vertaler$t("financiele_situatie")),
                        map_chr(vraagnummers_categorie("financiele_situatie"), vraag_met_subnummer),
                        vertaler$t("profiel_obv_financiele_situatie"),
                        str_c("III - ", vertaler$t("kennis_en_ervaring")),
                        map_chr(vraagnummers_categorie("kennis_ervaring"), vraag_met_subnummer),
                        vertaler$t("profiel_obv_kennis_en_ervaring"),
                        str_c("IV - ", vertaler$t("risicobereidheid")),
                        map_chr(vraagnummers_categorie("risicobereidheid"), vraag_met_subnummer),
                        vertaler$t("profiel_obv_risicobereidheid"),
                        str_c("V - ", vertaler$t("te_beleggen_vermogen_en_evt_so")),
                        str_c("1 - ", vertaler$t("te_beleggen_vermogen")),
                        str_c("2 - ", vertaler$t("jaarlijkse_bijstorting_of_onttrekking")),
                        str_c("VI - ", vertaler$t("keuzes_adhv_grafieken")),
                        str_c("1 - ", vertaler$t("eerste_keuze_visueel")),
                        str_c("2 - ", vertaler$t("tweede_keuze_visueel")),
                        " ",
                        vertaler$t("voorlopige_conclusie_alles_bijelkaar"), #Voorlopige conclusie o.b.v. uw beleggingsdoelstelling, financiële situatie, risicobereidheid en visuele keuzes samen",
                        " ",
                        str_c("VII - ", vertaler$t("duurzaamheid")),
                        map_chr(vraagnummers_categorie("duurzaamheid")[1:2], vraag_met_subnummer),
                        if(meer_vragen_duurzaamheid() == TRUE)
                        {map_chr(vraagnummers_categorie("duurzaamheid")[3:4], vraag_met_subnummer)} else {NULL}, # {c("", "")},
                        " ",
                        str_c("VIII - ", vertaler$t("overig")),
                        map_chr(vraagnummers_categorie("overig"), vraag_met_subnummer),
                        str_c("   ", vertaler$t("toelichting_eventuele_overige_zaken")),
                        if(private_beleggingen_uitvragen() == TRUE) {" "} else {NULL},
                        if(private_beleggingen_uitvragen() == TRUE) {vertaler$t("private_beleggingen")} else {NULL},
                        if(private_beleggingen_uitvragen() == TRUE) {map_chr(vraagnummers_categorie("private_beleggingen"), vraag_met_subnummer)} else {NULL}
                        ),
            Antwoord = c(naam(),
                        emailadres(),
                        entiteit_soort(),
                        #naam entiteit alleen als entiteit bv, st of ve is, anders nvt
                        if(entiteit() %in% c("bv" ,"ve", "st", "ov")) {
                          naam_entiteit()
                        } else {NULL},
                        #beleggingsstatuut alleen als entiteit st, ve of ov is, anders nvt
                        if(entiteit() %in% c("ve", "st", "ov")) {
                          if(str_detect(input$beleggingsstatuut, "ja_beleggingsstatuut"))
                            {vertaler$t("beleggingsstatuut_aanwezig")} else if
                          (str_detect(input$beleggingsstatuut, "nee_beleggingsstatuut"))
                            {vertaler$t("geen_beleggingsstatuut_aanwezig")}
                        } else if(entiteit() == "bv") {
                          if(str_detect(input$pensioenbrief, "ja_pensioenbrief"))
                          {vertaler$t("pensioenbrief_aanwezig")} else if
                          (str_detect(input$pensioenbrief, "nee_pensioenbrief"))
                          {vertaler$t("geen_pensioenbrief_aanwezig")}
                        } else {NULL},
                        download$download_tijdstip,
                        " ",
                        map_chr(vraagnummers_categorie("beleggingsdoelstelling"), output_antwoord),
                        if(str_detect(input$beleggingsdoelstelling_toelichting, "nee")) {vertaler$t("n_v_t")} else {input$doel_toelichting1},
                        profiel_beleggingsdoelstelling(),
                        " ",
                        map_chr(vraagnummers_categorie("financiele_situatie"), output_antwoord),
                        profiel_financiele_situatie(),
                        " ",
                        map_chr(vraagnummers_categorie("kennis_ervaring"), output_antwoord),
                        profiel_kennis_ervaring(),
                        " ",
                        map_chr(vraagnummers_categorie("risicobereidheid"), output_antwoord),
                        profiel_risicobereidheid(),
                        " ",
                        format_bedrag_tabellen(bedrag = startvermogen(), big_mark = big_mark(), decimal_mark = decimal_mark()),
                        bijstorting_antwoord_tabel(),
                        " ",
                        keuze_visueel1(),
                        keuze_visueel2(),
                        " ",
                        profiel_conclusie(),
                        " ",
                        " ",
                        map_chr(vraagnummers_categorie("duurzaamheid")[1:2], output_antwoord),
                        if(meer_vragen_duurzaamheid() == TRUE)
                          {map_chr(vraagnummers_categorie("duurzaamheid")[3:4], output_antwoord)} else {NULL}, # {c("", "")},
                        " ",
                        " ",
                        map_chr(vraagnummers_categorie("overig"), output_antwoord),
                        if(str_detect(input$overig, "nee")) {vertaler$t("n_v_t")} else {input$overig_toelichting},
                        if(private_beleggingen_uitvragen() == TRUE) {" "} else {NULL},
                        if(private_beleggingen_uitvragen() == TRUE) {" "} else {NULL},
                        if(private_beleggingen_uitvragen() == TRUE) {map_chr(vraagnummers_categorie("private_beleggingen"), output_antwoord)} else {NULL}
                        )
                  ) |>
                  mutate(Antwoord = str_remove(Antwoord, " - hieronder opent een tekstvakje waarin u een toelichting kunt schrijven"),
                         Antwoord = str_remove(Antwoord, " - zie ook de invulvelden hieronder"))
            )


    data_rapport_AIRS <- reactive({ #deze tabel gaat naar beide CSV's
      data <-
        data_rapport() |>
        # S/O op 0 indien daar geen sprake van is
        mutate(Antwoord =
                 case_when(#hier profiel_levels_AIRS in plaats van profiel_levels en
                           str_detect(Vraag, vertaler$t("profiel_obv_beleggingsdoelstelling")) ~ profiel_levels_AIRS[which(profiel_levels == profiel_beleggingsdoelstelling())],
                           str_detect(Vraag, vertaler$t("profiel_obv_financiele_situatie")) ~ profiel_levels_AIRS[which(profiel_levels == profiel_financiele_situatie())],
                           str_detect(Vraag, vertaler$t("profiel_obv_risicobereidheid")) ~ profiel_levels_AIRS[which(profiel_levels == profiel_risicobereidheid())],
                           str_detect(Vraag, vertaler$t("profiel_obv_kennis_en_ervaring")) ~ profiel_levels_AIRS[which(profiel_levels == profiel_kennis_ervaring())],
                           str_detect(Vraag, vertaler$t("eerste_keuze_visueel")) ~ profiel_levels_AIRS[which(profiel_levels == keuze_visueel1())],
                           str_detect(Vraag, vertaler$t("tweede_keuze_visueel")) ~ profiel_levels_AIRS[which(profiel_levels == keuze_visueel2())],
                           str_detect(Vraag, vertaler$t("voorlopige_conclusie_alles_bijelkaar")) ~ profiel_levels_AIRS[which(profiel_levels == profiel_conclusie())],
                           TRUE ~ Antwoord),
               Antwoord = ifelse(str_detect(Antwoord, vertaler$t("geen_jaarlijkse_bijstortingen_of_onttrekkingen")), 0, Antwoord),
               Antwoord = ifelse(Antwoord == vertaler$t("geen_beleggingsstatuut_aanwezig"), 0, Antwoord),
               Antwoord = ifelse(Antwoord == vertaler$t("beleggingsstatuut_aanwezig"), 1, Antwoord),
               Antwoord = ifelse(Antwoord == vertaler$t("geen_pensioenbrief_aanwezig"), 0, Antwoord),
               Antwoord = ifelse(Antwoord == vertaler$t("pensioenbrief_aanwezig"), 1, Antwoord),
               Antwoord = ifelse(str_detect(Antwoord, vertaler$t("n_v_t")), 0, Antwoord),
               #format_bedrag vervangen door bedrag in het geval van startvermogen, startvermogen(), en eventuele bijstortingen (abs(bijstorting()))
               Antwoord = ifelse(str_detect(Vraag, str_c("1 - ", vertaler$t("te_beleggen_vermogen"))),
                                 vctrs::vec_cast(startvermogen(), integer()), Antwoord),
               Antwoord = ifelse(str_detect(Vraag, str_c("2 - ", vertaler$t("jaarlijkse_bijstorting_of_onttrekking"))),
                                 vctrs::vec_cast(bijstorting(), integer()), Antwoord),
               #privé vervangen door prive
               across(everything(), ~str_replace_all(., "é", "e")),
               #financiële door financiele
               across(everything(), ~str_replace_all(., "ë", "e")),
               #underscore geeft problemen
               across(everything(), ~str_replace_all(., "_", " "))
               )

      #3 regels invoegen bij prive en enof, want de andere varianten hebben 3 regels voor naam_entiteit en beleggingsstatuut EN pensioenbrief, ook al is het of beleggingsstatuut/of pensioenbrief
      {if(!entiteit() %in% c("bv", "ve", "st", "ov")) data <- add_row(data, .before = which(data$Vraag == vertaler$t("tijdstip_van_opsturen")), Vraag = c("leeg", "leeg", "leeg"), Antwoord = c("0","0", "0")) else data}

      #als het een bv is dan 1 regel beleggingsstatuut = 0 invoegen voor de regel pensioenbrief
      {if(entiteit() == "bv") data <- add_row(data, .before = which(data$Vraag == vertaler$t("pensioenbrief")), Vraag = c("leeg"), Antwoord = c("0")) else data}

      #als het een st ve of ov is dan 1 regel pensioenbrief = 0 invoegen NA de regel beleggingsstatuut
      {if(entiteit() %in% c("ve", "st", "ov")) data <- add_row(data, .after = which(data$Vraag == vertaler$t("beleggingsstatuut")),
                                                               Vraag = c("leeg"), Antwoord = c("0")) else data}

      #2 regels invoegen als meer_vragen_duurzaamheid() == FALSE, voor 4, check dit in sheet AIRS_kolom
      {if(meer_vragen_duurzaamheid() == FALSE) data <- add_row(data, .before = which(data$Vraag == str_c("VIII - ", vertaler$t("overig"))),
                                                               Vraag = c("leeg", "leeg"), Antwoord = c("0","0")) else data}

      #1 regel toevoegen als het een bv is, de vraag naar de omvang van het eigen vermogen ontbreekt namelijk
      #vertaal opzoeken uit vragen_antwoorden_sheet_etc
      #HIER
      #{if(entiteit() == "bv") data <- add_row(data, .before =  which(data$Vraag == "1 - Het vermogen in de besloten vennootschap"), Vraag = "leeg", Antwoord = "0") else data}
      vraag1_eigen_vermogen_liquiditeit <- reactive({
        vragen_antwoorden() %>% filter(onderwerp == "eigen_vermogen_liquiditeit") %>% select(vraag) %>% head(1) %>% pull()
      })
      {if(entiteit() == "bv") data <- add_row(data, .before =  which(data$Vraag == str_c("1 - ", vraag1_eigen_vermogen_liquiditeit())),
                                              Vraag = "leeg", Antwoord = "0") else data}

      #3 lege regels private beleggingen uitvraag toevoegen na regel Vraag = "   Toelichting eventuele overige zaken" als private_beleggingen_uitvragen FALSE is
      {if(private_beleggingen_uitvragen() == FALSE) data <- add_row(data, .after = which(data$Vraag == str_c("   ", vertaler$t("toelichting_eventuele_overige_zaken"))),
                                                                    Vraag = c("leeg", "leeg", "leeg"), Antwoord = c("0", "0", "0")) else data}

      #regel AIRS id's toevoegen
      data <- add_row(data, .before = 1, Vraag = "AIRS ID", Antwoord = "")

      #kolom met AIRS headers toevoegen
      data <- data |> mutate(Vraag_AIRS = AIRS_en_Beleggersprofiel_NL_vragen_kolom$kolomnaam_AIRS,
                             Vraag_Beleggingsvoorstel_NL = AIRS_en_Beleggersprofiel_NL_vragen_kolom$kolomnaam_Beleggingsvoorstel_NL)

      #data <- add_column(data, .before = 1, Vraag = AIRS_en_Beleggersprofiel_NL_vragen_kolom$kolomnaam_AIRS)
      data <- data |> mutate(Antwoord = ifelse(Vraag == "AIRS ID", 0, Antwoord))

      data
    })

    vette_regels <- reactive({
      c(
        which(str_detect(data_rapport()[[1]], vertaler$t("beleggingsdoelstelling"))),
        which(str_detect(data_rapport()[[1]], vertaler$t("beleggingsdoelstelling"))),

      grep("I - Beleggingsdoelstelling |I - Investment |II - |V -|Voorlopige |Preliminary |Private",  data_rapport()$Vraag)
      )
      #hij pakt de eerste niet!?
    })

    #html tabel voor op de pagina Tabel, na verzenden van het rapport
    output$tabel_rapport <- function() {
      data_rapport() %>%
        as_tibble() %>%
        rename(!!vertaler$t("vraag") := Vraag,
               !!vertaler$t("antwoord") := Antwoord) %>%
        kbl() %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "center") %>%
        row_spec(vette_regels(), bold = TRUE)
    }

    #barplot module (zowel in rapport als in app)
    barplot_een_Server(
        id = "een",
        vertaler = vertaler,
        taal,
        data_voor_barcharts = data_voor_barcharts,
        anoniem = TRUE,
        startvermogen = startvermogen
      )

    barplot_twee_Server(
      id = "twee",
      vertaler = vertaler,
      taal,
      data_voor_barcharts = data_voor_barcharts,
      anoniem = TRUE,
      keuze_visueel1 = keuze_visueel1,
      startvermogen = startvermogen
      )

    #plot in rapport met anoniem = FALSE
    bm_3jaars_plot <-
        barplot_drie_Server(
        id = "barplot_drie_module",
        vertaler = vertaler,
        taal,
        data_voor_barcharts = data_voor_barcharts,
        anoniem = FALSE,
        profiel_conclusie = profiel_conclusie,
        startvermogen = startvermogen
        )

  #scenariosplot module (zowel in rapport als in app)
  scenariosplot <-
    scenariosplotServer(
      id = "scenariosplot_module",
      vertaler = vertaler,
      taal,
      df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros = df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros,
      horizon = horizon,
      profiel_conclusie = profiel_conclusie,
      startvermogen = startvermogen,
      bijstorting = bijstorting
    )

  rv <- reactiveValues(trigger_download = 0)   # NEW: to trigger real download after confirm



  observeEvent(input$start_rapport, {
    showModal(modalDialog(
      title = NULL,
      size = "l",
      easyClose = TRUE,
      tags$div(
        style = "text-align: center; max-width: 800px; margin: 0 auto; color: #808080; font-size: 18px; padding: 30px;",
        # Logo bovenaan
        tags$div(
          style = "margin-bottom: 30px;",
          tags$img(src = "logo.png", width = "300px", style = "display: block; margin: 0 auto;")
        ),
        tags$h3(vertaler$t("even_geduld"), style = "margin-bottom: 20px;"),
        tags$p(
          vertaler$t("het_maken_en_versturen_kan_even_duren"), tags$br()
        )
      ),
      # Footer met gecentreerde OK-knop (onderaan in het midden)
      footer = tags$div(
        style = "text-align: center; width: 100%;",
        actionButton("bevestig_rapport_maken",
                                      label = "OK",#vertaler$t("rapport_maken_en_versturen"),
                                      class = "button",
                                      icon = icon("check"))
       )
     )
    )
  })


  observeEvent(input$bevestig_rapport_maken, {
    removeModal()
    rv$trigger_download <- rv$trigger_download + 1
  })

    #waarde voor download teller op nul stellen
    #waarde voor tijdstip op nul stellen
    #beide worden in downloadHandler actueel gemaakt, waarna ook de knop "naar_voorlopige_conclusie" verschijnt
    download <- reactiveValues(aantal_downloads = 0,
                               download_tijdstip = NULL,
                               success_trigger = 0
                               )


    output$rapport_hidden <- downloadHandler(
      
      filename = function() {
        tijd <- if (taal() == "nl") {
          format(with_tz(Sys.time(), tzone = "Europe/Amsterdam"), "%d %b %Y %X") |> 
            maanden_naar_nl()
        } else {
          format(with_tz(Sys.time(), tzone = "Europe/Amsterdam"), "%B %d, %Y %X")
        }
        
        # Store timestamp for use in content() and emails
        download$download_tijdstip <- tijd
        
        str_c(vertaler$t("beleggersprofiel"), " ", naam_in_file_en_aanhef(), " - ", tijd, ".pdf")
      },
      
      content = function(file) {
        withProgress(
          message = vertaler$t("rapport_maken"), 
          detail = NULL, 
          value = 0.1, {
            
            subject <- str_c(vertaler$t("beleggersprofiel"), " ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip)
            pdf_filename <- str_c(subject, ".pdf")
            png_3jrs_name <- str_c("fig_3jaars_blprfl_", naam_in_file_en_aanhef(), "_", download$download_tijdstip, ".png")
            png_scen_name <- str_c("fig_scenarios_blprfl_", naam_in_file_en_aanhef(), "_", download$download_tijdstip, ".png")
            
            tmp_dir <- tempdir()
            tmp_logo <- file.path(tmp_dir, "logo.png")
            file.copy(from = "www/logo.png", to = tmp_logo, overwrite = TRUE)
            
            ggsave(filename = png_3jrs_name, plot = bm_3jaars_plot(), device = "png", width=8, height=6, dpi = 200)
            tmp_plot1 <- file.path(tmp_dir, png_3jrs_name)
            file.copy(from = png_3jrs_name, to = tmp_plot1, overwrite = TRUE)
            
            ggsave(filename = png_scen_name, plot = scenariosplot(), device = "png", width=8, height=6, dpi = 200)
            tmp_plot2 <- file.path(tmp_dir, png_scen_name)
            file.copy(from = png_scen_name, to = tmp_plot2, overwrite = TRUE)
            
            tmp_bp_rapport_pdf_Rmd <- file.path(tmp_dir, "bp_rapport_pdf.Rmd")
            file.copy(from = "bp_rapport_pdf.Rmd", to = tmp_bp_rapport_pdf_Rmd, overwrite = TRUE)
            
            setProgress(value = 0.3, message = vertaler$t("rapport_maken"))
            
            params <- list(
              plot1 = tmp_plot1,
              plot2 = tmp_plot2,
              logo = tmp_logo,
              table = data_rapport(),
              vette_regels = vette_regels(),
              naam = naam(),
              naam_in_file_en_aanhef = naam_in_file_en_aanhef(),
              entiteit = entiteit(),
              naam_entiteit = naam_entiteit(),
              tijdstip = download$download_tijdstip,
              inflatie = inflatie,
              ret_nl_obl = ret_nl_obl,
              ret_wereld_aandelen = ret_wereld_aandelen,
              sd_nl_obl = sd_nl_obl,
              sd_wereld_aandelen = sd_wereld_aandelen,
              cor_wereld_aandelen_nl_obl = cor_wereld_aandelen_nl_obl,
              profiel_conclusie = profiel_conclusie(),
              horizon = horizon(),
              bijstorting = bijstorting(),
              taal = taal(),
              translations_path = json_path
            )
            
            setProgress(value = 0.5, message = vertaler$t("rapport_maken"))
            
            rmarkdown::render(tmp_bp_rapport_pdf_Rmd,
                              output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
            
            setProgress(value = 0.6, message = vertaler$t("rapport_maken"))
            
            # CSV for AIRS
            write.table(
              data_rapport_AIRS() %>%
                mutate(Antwoord = ifelse(str_detect(Vraag, vertaler$t("tijdstip_van_opsturen")), 
                                         download$download_tijdstip, Antwoord)) %>%
                select(Vraag_AIRS, Antwoord) %>%
                t() %>%
                as_tibble(),
              file = str_c("Beleggersprofiel_voor_AIRS ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv"),
              sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE, eol = "\r\n"
            )
            
            # CSV for Beleggingsvoorstel
            write.table(
              data_rapport_AIRS() %>%
                mutate(Antwoord = ifelse(str_detect(Vraag, vertaler$t("tijdstip_van_opsturen")), 
                                         download$download_tijdstip, Antwoord)) %>%
                select(Vraag_Beleggingsvoorstel_NL, Antwoord) %>%
                t() %>%
                as_tibble(),
              file = str_c("Beleggersprofiel_voor_Beleggingsvoorstel ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv"),
              sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE, eol = "\r\n"
            )
            
            setProgress(value = 0.7, message = vertaler$t("rapport_gereed"))
            
            # Emails (your original code – adjust if/smtp_username logic as needed)
            message("Sending message to ", input$emailadres, ".")
            
            #versturen beleggersprofiel pdf rapport naar client
            #in testmodus vanuit pp anders vanuit ba
            verzenden <- if (test_modus()) smtp_pp else smtp_ba
            
            envelope(
              from = if(test_modus()) {smtp_username_pp} else {smtp_username_ba},
              to = unlist(str_split(str_replace_all(str_replace_all(input$emailadres, ";", ","), " ", ""), ",")),
              cc = if(test_modus()) {NULL} else {"maingay@bavandoorn.nl"},
              subject = subject
            ) %>%
              text(str_c(vertaler$t("beleggersprofiel"), " ", naam_in_file_en_aanhef())) %>%
              attachment(file, disposition = "attachment", name = pdf_filename) |>
              verzenden()
            
            setProgress(value = 0.9, message = vertaler$t("rapport_versturen"))
            
            #versturen beleggersprofiel pdf rapport naar client
            envelope(
              from = if(test_modus()) {smtp_username_pp} else {smtp_username_ba},
              to = if(test_modus()) {"pieterprins@yahoo.com"} else {"administratie@bavandoorn.nl"},
              subject = subject
            ) %>%
              text(str_c("Beleggersprofiel ", naam_in_file_en_aanhef(), " csv-, pdf- en png-bestanden")) %>%
              attachment(str_c("Beleggersprofiel_voor_AIRS ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv")) %>%
              attachment(str_c("Beleggersprofiel_voor_Beleggingsvoorstel ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv")) %>%
              attachment(file, disposition = "attachment", name = pdf_filename) |>
              attachment(file.path(tmp_dir, png_3jrs_name), disposition = "attachment", name = png_3jrs_name) %>%
              attachment(file.path(tmp_dir, png_scen_name), disposition = "attachment", name = png_scen_name) %>%
              verzenden()
            
            setProgress(value = 0.99)
            
            # Success!
            download$success_trigger <- download$success_trigger + 1
            download$aantal_downloads <- download$aantal_downloads + 1
            
            shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_start"]')
            
            setProgress(value = 1)
          })  # end withProgress
      },  # end content
      
      contentType = "application/pdf"
    )

    outputOptions(output, "rapport_hidden", suspendWhenHidden = FALSE)

    # 4. Auto-trigger hidden download button click after confirm
    observe({
      req(rv$trigger_download > 0)

      # Add a 300–800 ms delay to let Shiny bind the download link properly
      shinyjs::delay(
        500,  # try 300 first; increase to 800 if still HTML
        shinyjs::click("rapport_hidden")
      )
    })

    # 5. Your existing success modal (unchanged)
    # observeEvent(download$success_trigger, {
    #   req(download$success_trigger > 0)
    #   showModal(modalDialog(
    #     title = NULL,
    #     size = "l",
    #     easyClose = TRUE,
    #     tags$div(
    #       style = "text-align: center; max-width: 800px; margin: 0 auto; color: #808080; font-size: 18px; padding: 30px;",
    #       tags$div(
    #         style = "margin-bottom: 30px;",
    #         tags$img(src = "logo.png", width = "200px", style = "display: block; margin: 0 auto;")
    #       ),
    #       tags$h3(vertaler$t("dank_u_wel"), style = "margin-bottom: 20px;"),
    #       tags$p(
    #         vertaler$t("rapport_is_gemaakt"), tags$br()
    #       )
    #     ),
    #     footer = tags$div(
    #       style = "text-align: center; width: 100%;",
    #       actionButton(
    #         "modal_ok",
    #         "OK",
    #         style = "background-color: #94bcb2; color: white; border: none; padding: 10px 40px; font-size: 16px;"
    #       )
    #     )
    #   ))
    # })

    # Optional: cleanup on session end (you already have something similar – keep/adapt)
    session$onSessionEnded(function() {
      # your unlink logic for temp files / wildcards
      #unlink(c(png_3jrs_name, png_scen_name))  # note: these vars are local now → move if needed or use list.files
      # or keep your wildcard version
      wildcard_files1 <- list.files(pattern = "^Beleggersprofiel_.*", full.names = TRUE)
      wildcard_files2 <- list.files(pattern = "*.png", full.names = TRUE)
      unlink(c(wildcard_files1, wildcard_files2))
      cat("Sessie Einde\n")
    })

  } #server

# Run the application
shinyApp(ui = ui, server = server)
