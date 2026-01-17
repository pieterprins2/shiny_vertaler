

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

#==mail
library(emayili) #email
#library(polished) #email validate evt
library(shinyalert)

  #opzet vertaling vanuit spreadsheet
  df <- read_excel("translations/translations.xlsx", sheet = "Sheet1")
  #vqn excel sheet naar json
  # Build the JSON structure ---------------------------------------------
  json_structure <- list(
    cultural_date_format = "%d-%m-%Y",
    languages = colnames(df),
    translation = df
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

#test_modus <- naam = test_
#publish <- TRUE #publish naar shinyapps.io afhankelijk van te kiezen target .Renviron file

#publish_target <- "PP" #of "BAVD"
#shiny_path <- "/Users/pieter/Library/CloudStorage/OneDrive-Personal/R Projects/Beleggersprofiel/shiny/"
# 
# if(publish_target == "PP") {
#   file.copy(from = str_c(shiny_path,".Renviron_PP"), 
#             to = str_c(shiny_path,".Renviron"))
# } else if(publish_target == "BAVD") {
#   file.copy(from = str_c(shiny_path,".Renviron_BAVD"), 
#             to = str_c(shiny_path,".Renviron"))
# }


#==rsconnect variabelen, komt uit .Renviron, varieert per user
# if(publish) {
# #   library(rsconnect)
#    rsconnect::setAccountInfo(name=Sys.getenv("name"),
#                              token=Sys.getenv("token"),
#                              secret=Sys.getenv("secret"))
# # }
#    
   # rsconnect::setAccountInfo(name='pieter-prins',
   #                           token='9C216407260A270B90EFC3A79BED6A6C',
   #                           secret='tn8l5OvwBC/Rg4E/BrCS0fXGNv/Jr6ruDXBPwtxp')
   # 

# rsconnect::setAccountInfo(name='bavd',
#                           token='12B340AB28AC93D5F375A07AB5E240F6',
#                           secret='UiNW78d/WNwvKmKReBi8o4vWYJ1AeYIhqVEjxFro')

# 
#==mail variabelen, komt uit .Renviron, varieert per user

  smtp_server <- "smtp.gmail.com" #Sys.getenv("SMTP_SERVER")
  smtp_port <- "587" #Sys.getenv("SMTP_PORT")
  smtp_username <- "pieterprins2@gmail.com" #Sys.getenv("SMTP_USERNAME")
  IP <- content(GET("https://api.ipify.org?format=json"))$ip
  
  SMTP_PASSWORD="tnjjwjpocvslhfur"
  
  smtp <- emayili::server(
    host = smtp_server,
    port = smtp_port,
    username = smtp_username,
    password = Sys.getenv("SMTP_PASSWORD"),
    max_times = 1
  )

smtp_server <- "smtp.gmail.com" #Sys.getenv("SMTP_SERVER")
smtp_port <- "587" #Sys.getenv("SMTP_PORT")
smtp_username <- "pieterprins2@gmail.com" #Sys.getenv("SMTP_USERNAME")
IP <- content(GET("https://api.ipify.org?format=json"))$ip


# SMTP_PASSWORD="tnjjwjpocvslhfur"
# 
# smtp <- emayili::server(
#   smtp(smtp),
#   host = "smtp.gmail.com",
#   port = 587,                # TLS (STARTTLS) – most reliable for Gmail
#   username = Sys.getenv("SMTP_USERNAME"),
#   password = Sys.getenv("SMTP_PASSWORD"),
#   secure   = "starttls",
#   # Optional but recommended for Gmail:
#   use_ssl = FALSE,           # Use STARTTLS instead of implicit SSL
#   insecure = FALSE
#   # 
#   # host = smtp_server,
#   # port = smtp_port,
#   # username = smtp_username,
#   # password = Sys.getenv("SMTP_PASSWORD"),
#   # max_times = 1
# )

#colors
ba_color <- "#4E9080"
sim_lijntjes_kleur <- "#E59D00"
darkgray <- "#252525" #brewer.pal(n = 9, name = 'Greys')[8]
euroblue <- "#004494"
euroyellow <- "#ffd617"
usred <- "#bf0a30"

titel <- "Beleggersprofiel"

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


#bedragen formaat verschilt per taal
# current_lang <- vertaler$get_translation_language()
# big_mark <- if (current_lang == "nl") "." else ","  
# decimal_mark <- if (current_lang == "nl") "," else "."
# 
# #duplicaat code voorkomen 2 - schijnprecisie eruit
# format_bedrag_tabellen <- function(bedrag, big_mark, decimal_mark) {
#   paste0(" €", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
#                        format="f", digits=0, big.mark=big_mark, decimal.mark = decimal_mark))
# }
# #voor bijstorting tot 100 euro nauwkeurig
# format_bedrag_tabellen_bijstorting <- function(bedrag, big_mark, decimal_mark) {
#   paste0(" €", formatC(as.numeric(round(bedrag/100, 0) * 100),
#                        format="f", digits=0, big.mark=big_mark, decimal.mark = decimal_mark))
# }

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

vragen_antwoorden_database <- function(entiteit = "prive", taal = "nl") {
  #eenvoudige gevallen:
  if(entiteit %in% c("prive", "enof", "bv", "ve")) {
    vragen_antwoorden_start <-
      read_xlsx(vragen_antwoorden_excel_bestand, sheet = str_c("vragen_antwoorden_", entiteit, "_", taal))
  #overige een voor een, ivm voorkomen teveel onderhoud in additionele excel sheets
  } else if(taal == "nl" & entiteit == "st") {
    vragen_antwoorden_start <-
      read_xlsx(vragen_antwoorden_excel_bestand, sheet = str_c("vragen_antwoorden_", "ve", "_", "nl")) %>% 
      mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "vereniging", "stichting"))
  } else if(taal == "nl" & entiteit == "ov") {
    vragen_antwoorden_start <-
      read_xlsx(vragen_antwoorden_excel_bestand, sheet = str_c("vragen_antwoorden_", "ve", "_", "nl")) %>% 
      mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "vereniging", "organisatie"))
  } else if(taal == "en" & entiteit == "st") {
    vragen_antwoorden_start <-
      read_xlsx(vragen_antwoorden_excel_bestand, sheet = str_c("vragen_antwoorden_", "ve", "_", "en")) %>% 
      mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "association", "foundation"))
  } else if(taal == "en" & entiteit == "ov") {
    vragen_antwoorden_start <-
      read_xlsx(vragen_antwoorden_excel_bestand, sheet = str_c("vragen_antwoorden_", "ve", "_", "en")) %>% 
      mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "association", "organisation"))
  }
  
  vragen_antwoorden_ruw <- 
    vragen_antwoorden_start %>% 
    group_by(vraag) %>%
    uncount((1:n() == 1) + 1) %>% #voegt een regel toe
    mutate(n = n(),
           #nr = seq(n),
           nr = row_number(),
           antwoord_aanhef = vraag,
           #vult het eerste antwoord in, nl. "Maak een hieronder keuze:"
           antwoord = ifelse(nr == 1,  vertaler$t("maak_keuze"), antwoord),#Maak hieronder een keuze:", antwoord),
           selected = ifelse(nr == 1, NA, selected),
           punten = ifelse(nr == 1, 0, punten)) %>%
    select(-c(n, nr)) %>%
    ungroup()
    #vraagnummer maken (ipv in excel, is flexibeler)
    factor_vragen <-
      vragen_antwoorden_ruw %>%
      select(vraag) %>%
      unique() %>% pull()

    vragen_antwoorden_ruw %>%
      mutate(nummer = as.numeric(factor(vraag, levels = factor_vragen)))
}

# vragen_antwoorden_prive <- vragen_antwoorden_database(entiteit = "prive")
# vragen_antwoorden_enof <- vragen_antwoorden_database(entiteit = "enof")
# vragen_antwoorden_bv <- vragen_antwoorden_database(entiteit = "bv")
# vragen_antwoorden_ve <- vragen_antwoorden_database(entiteit = "ve")
#stichting en organisatie (overig) zijn alleen zoeken-vervangen van vereniging
# vragen_antwoorden_st <- vragen_antwoorden_ve |> 
#   mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "vereniging", "stichting"))
# vragen_antwoorden_ov <- vragen_antwoorden_ve |> 
#   mutate_at(vars(c(vraag, antwoord, selected)), ~ str_replace(., "vereniging", "organisatie"))
#===

# #vragen formulier
#1 selectizeInput01 --> verhuisd naar server, renderUI
#2 bedragen input, duplicaat code voorkomen
# bedragenInput <- function(inputId, label, value) {
#   shinyWidgets::autonumericInput(
#     inputId = inputId,
#     label = label,
#     value = value,
#     currencySymbolPlacement = "p",
#     currencySymbol = "€",
#     decimalPlaces = 0,
#     minimumValue = 0,
#     digitGroupSeparator = big_mark,
#     decimalCharacter = decimal_mark,
#     align = "left",
#     width = 800)
# }

# UI
ui <- 
  fluidPage(
    
  tags$head(
   tags$link(rel = "stylesheet", type = "text/css", href = "beleggersprofiel.css")
   ),
  # In UI:
  tags$head(
    tags$script(HTML("
    Shiny.addCustomMessageHandler('triggerDownload', function(message) {
      var link = document.createElement('a');
      link.href = message.href;           // the Shiny download endpoint
      link.download = message.filename;   // suggested name
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    });
  "))
  ),
  #===
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('tellertje_actief', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
  
  #===
  useShinyjs(),
  shiny.i18n::usei18n(vertaler),
  
   navbarPage(
    title = div(img(src='logo.png', #staat in www/
    style = "margin-top: -14px;
              padding-right: 10px;
              padding-bottom: 10px",
    height = 60)),
    windowTitle = "Beleggersprofiel - B.A. van Doorn",
    collapsible = TRUE,
    selected = NULL,
    id = "tabset",
         tabPanel(title = uiOutput("ui_tabset_taal_titel"), value = "panel_taal", id = "panel_taal",
         h4(uiOutput("ui_vul_taal_in")),
         actionButton(class = "button", 'jumpToP_start', vertaler$t("volgende_pagina"))
         ),
         tabPanel(title = "Start", value = "panel_start", id = "panel_start",
         h3(uiOutput("ui_intro_head")),
         h4(uiOutput("ui_intro_text")),
         br(),
         h4(uiOutput("ui_vul_naam_in")),
         #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
         tagQuery(textInput("naam", 
                            value = "testen",
                            placeholder = "...",
                            label = vertaler$t("uw_naam"), 
                            width = '500px'))$find("input")$addAttrs(autocomplete = "off")$allTags(),
         #h4("Nadat dit beleggersprofiel is ingevuld worden de resultaten ervan verwerkt in een rapport. Dit rapport wordt via e-mail naar u toegestuurd."),
         h4(uiOutput("ui_u_krijgt_rapport_text")),
         uiOutput("ui_vraag_emailadres"),
         br(),
         h4(uiOutput("ui_intro_vraag_entiteit")),
         #h4("Sommige vragen, of de formulering ervan, hangen af van het soort entiteit dat u vertegenwoordigt."),
         uiOutput("ui_vraag_entiteit"),
         uiOutput("ui_vraag_naam_entiteit"),
         uiOutput("ui_vraag_beleggingsstatuut"),
         textOutput('error_msg_start'),
         actionButton(class = "back_button", 'jump_back_ToP_taal', vertaler$t("vorige_pagina")),
         actionButton(class = "button", 'jumpToP_beleggingsdoelstelling', vertaler$t("volgende_pagina")),
        ),
        tabPanel(title = "I", value = "panel_beleggingsdoelstelling",
           #h3(str_c("I. ", vertaler$t("beleggingsdoelstelling"))),
           #h3("I. ", vertaler$use_js("beleggingsdoelstelling")),
           h3(tagList("I. ", vertaler$t("beleggingsdoelstelling"))),
           uiOutput("ui_aanhef_doelstelling"),
           br(),
           uiOutput("ui_selectize_beleggingsdoelstelling"),
           uiOutput("ui_cond_panel_toelichting_doelstelling"),
           textOutput('error_msg_beleggingsdoelstelling'),
           actionButton(class = "back_button", 'jump_back_ToP_start', vertaler$t("vorige_pagina")),
           actionButton(class = "button", 'jumpToP_financiele_situatie', vertaler$t("volgende_pagina"))
        ),
        tabPanel(title = "II", value = "panel_financiele_situatie",
           h3("II. ", vertaler$t("financiele_situatie")),
           uiOutput("ui_aanhef_financiele_situatie"),
           uiOutput("ui_selectize_financiele_situatie"),
           textOutput('error_msg_financiele_situatie'),
           actionButton(class = "back_button", 'jump_back_ToP_beleggingsdoelstelling', vertaler$t("vorige_pagina")),
           actionButton(class = "button", 'jumpToP_kennis_ervaring', vertaler$t("volgende_pagina"))
        ),
        tabPanel(title = "III", value = "panel_kennis_ervaring",
           h3("III. ", vertaler$t("kennis_en_ervaring")),
           uiOutput("ui_selectize_kennis_ervaring"),
           textOutput('error_msg_kennis_ervaring'),
           actionButton(class = "back_button", 'jump_back_ToP_financiele_situatie', vertaler$t("vorige_pagina")),
           actionButton(class = "button", 'jumpToP_risicobereidheid', vertaler$t("volgende_pagina"))
        ),
        tabPanel(title = "IV", value = "panel_risicobereidheid",
           h3("IV. ", vertaler$t("risicobereidheid")),
           uiOutput("ui_aanhef_risicobereidheid"),
           uiOutput("ui_selectize_risicobereidheid"),
           textOutput('error_msg_risicobereidheid'),
           actionButton(class = "back_button", 'jump_back_ToP_kennis_ervaring', vertaler$t("vorige_pagina")),
           actionButton(class = "button", 'jumpToP_startvermogen', vertaler$t("volgende_pagina"))
        ),
        tabPanel(title = "V", value = "panel_startvermogen",
           h3("V. ", vertaler$t("startvermogen_en_so")),#Startvermogen en eventuele bijstortingen of onttrekkingen"),
           br(h5(" ")),
           uiOutput("ui_input_startvermogen"),
           textOutput('error_msg_startvermogen'),
           uiOutput("ui_input_stortingen_onttrekkingen"),
             fluidRow(
             column(5,
             conditionalPanel(
               condition = "input.keuze_SO == `ja_bijstorten`",
               uiOutput("ui_input_bijstorting")
               ),
             conditionalPanel(
               condition = "input.keuze_SO == `nee_onttrekken`",
               uiOutput("ui_input_onttrekking")
              )
              )
            ),
            actionButton(class = "back_button", 'jump_back_ToP_risicobereidheid', vertaler$t("vorige_pagina")),
            actionButton(class = "button", 'jumpToP_visuele_keuze1', vertaler$t("volgende_pagina"))
            ),
         tabPanel(title = "VI-1", value = "panel_visuele_keuze1",
            h3("VI-1. ", vertaler$t("visuele_keuze_rd_rm_stap1")), #Visuele keuze Risicodragend/Risicomijdend, stap 1 (van 2)"),
            p(style = "text-align: justify;", vertaler$t("op_deze_pagina_twee_risicoprofielen")),
              #"Op deze pagina staan twee risicoprofielen. Voor nadere toelichting bij deze grafieken druk onderaan deze pagina op de knop."),
            uiOutput("ui_barplot_een_module"),
            #barplot_een_module_UI(id = "een"),
            uiOutput("ui_selectize_visuele_keuze1"),
            textOutput('error_msg_visuele_keuze1'),
            #volgende om de knoppen op een rijtje te houden
            div(
             div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_startvermogen', vertaler$t("vorige_pagina"))),
             div(style="display: inline-block; width: 205px ;", actionButton(class = "infobutton", inputId = "grafieken_info1", label = vertaler$t("toelichting_grafieken"))),
                 #Toelichting bij de grafieken")),
             #volgende button verschijnt pas na opbouw van de plot, wordt in de server gemaakt
             div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_visuele_keuze2'))
             )
            ),
        tabPanel(title = "VI-2", value = "panel_visuele_keuze2",
            h3("VI-2. ", vertaler$t("visuele_keuze_rd_rm_stap2")), #Visuele keuze Risicodragend/Risicomijdend, stap 2 (van 2)"),
            p(style = "text-align: justify;", vertaler$t("de_keuze_die_u_maakte_staat_in_het_midden")),
           uiOutput("ui_barplot_twee_module"),
           uiOutput("ui_selectize_visuele_keuze2"),
           textOutput('error_msg_visuele_keuze2'),
           #volgende om de knoppen op een rijtje te houden
           div(
             div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_visuele_keuze1', vertaler$t("vorige_pagina"))),
             div(style="display: inline-block; width: 205px ;", actionButton(class = "infobutton", "grafieken_info2", vertaler$t("toelichting_grafieken"))),
             #volgende button verschijnt pas na opbouw van de plot, wordt in de server gemaakt
             div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_duurzaamheid'))
            )
           ),
        tabPanel(title = "VII", value = "panel_duurzaamheid",
           h3("VII. ", vertaler$t("duurzaamheid")),
           p(style = "text-align: justify;", vertaler$t("intro_duurzaamheid")),
           p(style = "text-align: justify;", vertaler$t("uitleg_duurzaamheid")),
           p(style = "text-align: justify;", vertaler$t("ba_moet_uitvragen")),
           uiOutput("ui_selectize_duurzaamheid_1_en_2"),
           conditionalPanel(
              #hier ging het fout, tekst van deze conditie moet gelijk zijn voor de verschillende entiteiten
              condition = "input.duurzaamheid_bavd.includes('belangrijkste nadelige effecten') || input.duurzaamheid_bavd.includes('principal adverse impacts')", # "str_detect(input.duurzaamheid_bavd, `de belangrijkste nadelige effecten`)", #== `moet rekening gehouden worden met de belangrijkste nadelige effecten op duurzaamheidsfactoren zoals hierboven genoemd`",
              p(style = "text-align: justify;", vertaler$t("duurzame_beleggingen_zijn")),
              uiOutput("ui_selectize_duurzaamheid_3"),
              p(style = "text-align: justify;", vertaler$t("eu_taxonomie")),
              uiOutput("ui_selectize_duurzaamheid_4")
           ),
           column(12,
           textOutput('error_msg_duurzaamheid'),
           #onderstaand is om de buttons op een lijn te houden en om de volgende pagina knop pas te laten verschijnen als de pagina is geladen
           div(
             div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_visuele_keuze2', vertaler$t("vorige_pagina"))),
             #volgende button verschijnt pas na download, wordt in de server gemaakt
             div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_overig'))
             )
            )
          ),
      tabPanel(title = "VIII", value = "panel_overig",
             h3("VIII. ", vertaler$t("overig")),
             p(style = "text-align: justify;", vertaler$t("overige_aspecten_toelichten")),
             br(),
             uiOutput("ui_selectize_overig"),
             uiOutput("ui_cond_panel_toelichting_overig"),
             textOutput('error_msg_overig'),
             uiOutput("ui_panel_overig_buttons")
             # column(12,
             #        #onderstaand is om de buttons op een lijn te houden en om de volgende pagina knop pas te laten verschijnen als de pagina is geladen
             #        
             #        div(
             #          div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_duurzaamheid', 'Ga terug')),
             #          #volgende button verschijnt pas na download, wordt in de server gemaakt
             #          div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_rapport'))
             #        )
             # )
        ),
        tabPanel(title = vertaler$t("rapport"), value = "panel_rapport",
            h3(vertaler$t("belangrijke_informatie")),
            p(style = "text-align: justify;", vertaler$t("disclaimer")),
            # oude disclaimer met variabelen
            # str_c(
            # "De scenario's zijn gebaseerd op ervaringscijfers.
            # Voor de verwachte opbrengst van Nederlandse staatsobligaties is een waarde gebruikt van ", round(100*ret_nl_obl, 2),
            # "%, voor wereldwijde aandelen ", round(100*ret_wereld_aandelen, 1), "%, met een risico van respectievelijk ",
            # round(100*sd_nl_obl, 0), "% en ", round(100*sd_wereld_aandelen, 0), "% en een licht negatieve onderlinge correlatie van ",
            # round(cor_wereld_aandelen_nl_obl, 1), ". Gepoogd is weer te geven wat op die basis realistische scenario's kunnen zijn.
            # De Europese Centrale Bank streeft een inflatie na van ", round(100*inflatie, 0), "%.
            # De getoonde bedragen zijn nominaal en daarbij is dus geen rekening gehouden met inflatie.
            # Cijfers moeten worden geïnterpreteerd als schattingen.
            # Ze vormen geen garantie voor de toekomst.
            # De toekomst kan beter of slechter uitpakken dan de uitersten in de simulatie.
            # Er is geen rekening gehouden met kosten.")),
            br(),
            uiOutput("ui_cond_panel_vraag_opsturen_beleggingsstatuut"),
            h3(vertaler$t("rapport_maken_en_versturen")),
            p(style = "text-align: justify;", vertaler$t("als_u_op_onderstaande_knop_klikt")),
          
            #onderstaand is om de buttons op een lijn te houden
            div(
              div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_overig', vertaler$t("vorige_pagina"))),
              div(style="display: inline-block; width: 305px ;", actionButton(inputId = "start_rapport", 
                                                                              label = vertaler$t("rapport_maken_en_versturen"),   # or whatever your label is
                                                                              icon = icon("file-pdf"), 
                                                                              class = "button")),
              downloadButton(outputId = "rapport_hidden", "Hidden Download", style = "display: none;"),
              #volgende button verschijnt pas na download, wordt in de server gemaakt
              div(style="display: inline-block; width: 65px ;", uiOutput('ui_naar_voorlopige_conclusie'))
              )
            )
         )
      )
  #)
#}

server <- function(input, output, session) {

  taal <- reactive({
    #moet zo geinitialiseerd, anders Warning: Error in if: argument is of length zero
    input$taal %||% "nl"  # Use %||% from rlang or purrr, or if (is.null(input$taal)) "nl" else input$taal
  })
  
  output$ui_tabset_taal_titel <- renderUI({
    # vlag afh van taal
    flag <- if (taal() == "en") span("🇬🇧", style = "font-size: 1.75em;") else if (taal() == "nl") span("🇳🇱", style = "font-size: 1.75em;") else ""
    tagList(
      flag
    )
  })
  
  format_bedrag_tabellen <- function(bedrag, big_mark, decimal_mark) {
    paste0(" €", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
                         format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  }
  #voor bijstorting tot 100 euro nauwkeurig
  format_bedrag_tabellen_bijstorting <- function(bedrag, big_mark, decimal_mark) {
    paste0(" €", formatC(as.numeric(round(bedrag/100, 0) * 100),
                         format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  }
  
  test_modus <- reactive({
    naam() == "testen"
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
    # This print is just for demonstration
    print(paste("Language change!", input$taal))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$taal)
    #shiny.i18n::update_lang(session, input$taal)
  })
  
  big_mark <- reactive({
    if (taal() == "nl") "." else ","
  })
  
  decimal_mark <- reactive({
    if (taal() == "nl") "," else "."
  })
  
  #bedragen formaat verschilt per taal
  # current_lang <- vertaler$get_translation_language()
  # big_mark <- if (current_lang == "nl") "." else ","  
  # decimal_mark <- if (current_lang == "nl") "," else "."
  
  #duplicaat code voorkomen 2 - schijnprecisie eruit
  # format_bedrag_tabellen <- function(bedrag) {
  #   paste0(" €", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
  #                        format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  # }
  # #voor bijstorting tot 100 euro nauwkeurig
  # format_bedrag_tabellen_bijstorting <- function(bedrag) {
  #   paste0(" €", formatC(as.numeric(round(bedrag/100, 0) * 100),
  #                        format="f", digits=0, big.mark=big_mark(), decimal.mark = decimal_mark()))
  # }
  
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
      width = 800)
  }
  
  vragen_antwoorden <- reactive({
    vragen_antwoorden_database(entiteit = input$entiteit, taal = taal())
  })

  max_min_punten_categorie <- function(categorie, min_max) {
    vragen_antwoorden() %>%
      filter(antwoord != vertaler$t("maak_keuze"),
             categorie == !!categorie) %>%
      group_by(vraag) %>%
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
output$ui_intro_head <- reactive({vertaler$t("intro_head")})
output$ui_intro_text <- reactive({vertaler$t("intro_text")})
output$ui_vul_naam_in <- reactive({vertaler$t("vul_naam_in")})
output$ui_u_krijgt_rapport_text <- reactive({vertaler$t("u_krijgt_rapport_text")})

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

output$ui_vul_taal_in <- renderUI({
  selectInput(inputId = "taal",
              label = "English/Nederlands",
              choices = c(setNames("nl", "Nederlands"),
                          setNames("en", "English")),
              selected = "nl")
})

output$ui_vraag_emailadres <- renderUI({
  #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
  tagQuery(textInput(inputId = "emailadres", 
                     value = if(test_modus()) {"pieterprins@yahoo.com"} else {""}, 
                     placeholder = vertaler$t("email_placeholder"), 
                     label = vertaler$t("uw_emailadressen"),
                     width = '500px'))$find("input")$addAttrs(autocomplete = "off")$allTags()
})

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
              #label = "Voor wie vult u dit formulier in?",
              label = vertaler$t("voor_wie"),
              width = '500px')
})

output$ui_vraag_naam_entiteit <- renderUI({
  req(entiteit())
  if(entiteit() %in% c("bv", "st", "ve", "ov")) {
    p(
      h4(paste(vertaler$t("wat_is_de_naam_van"), de_org_aanduiding(), "?")),
      #paste voor een spatie
      #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
      tagQuery(textInput("naam_entiteit", value = "...",
                          label = paste(vertaler$t("de_naam_van"), de_org_aanduiding()), 
                          width = '400px'))$find("input")$addAttrs(autocomplete = "off")$allTags()
    )
  }
})

#UI element in de server
output$ui_vraag_beleggingsstatuut <- renderUI({
  req(entiteit())
  if(entiteit() %in% c("st", "ve", "ov")) {
    p(h4(paste(vertaler$t("intro_pensioenbrief1"), de_org_aanduiding(), vertaler$t("intro_pensioenbrief2"), vertaler$t("intro_pensioenbrief3"))), #,
              selectizeInput(inputId = "beleggingsstatuut", 
                     label = paste(vertaler$t("heeft"), de_org_aanduiding(), vertaler$t("een_beleggingsstatuut")), 
                     choices = c(vertaler$t("maak_keuze"), 
                                 setNames("ja_beleggingsstatuut", paste(vertaler$t("ja"), de_org_aanduiding(), vertaler$t("heeft_beleggingsstatuut"))), 
                                 setNames("nee_beleggingsstatuut", paste(vertaler$t("nee"), de_org_aanduiding(), vertaler$t("heeft_geen_beleggingsstatuut")))),
                     selected = vertaler$t("maak_keuze"),#Maak hieronder een keuze:",
                     width = "400px")
    )
  } else if (entiteit() == "bv") {
      #p(h4(str_c("Indien de besloten vennootschap een pensioenbrief heeft, kunt u dit mogelijk gebruiken bij het invullen van dit beleggersprofiel.")),
      p(h4(str_c(vertaler$t("intro_pensioenbrief_bv"))),
                     selectizeInput(inputId = "pensioenbrief", 
                     label = vertaler$t("heeft_bv_pensioenbrief"), 
                     choices = c(vertaler$t("maak_keuze"), 
                                 setNames("ja_pensioenbrief", str_c(vertaler$t("bv_heeft_pensioenbrief"))), #Ja, de besloten vennootschap heeft een pensioenbrief")), 
                                 setNames("nee_pensioenbrief", vertaler$t("bv_heeft_geen_pensioenbrief"))),
                     selected = vertaler$t("maak_keuze"),#Maak hieronder een keuze:",
                     width = "400px")
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
      tagQuery(
      textInput(inputId = "overig_toelichting", 
                label = vertaler$t("hier_nader_toelichten"),
                value = "...", width = '6500px',
                placeholder = "..."))$find("input")$addAttrs(autocomplete = "off")$allTags()
     )
    )
  })
  
  output$ui_panel_overig_buttons <- renderUI({
    column(12,
           #onderstaand is om de buttons op een lijn te houden en om de volgende pagina knop pas te laten verschijnen als de pagina is geladen
           if(private_beleggingen_uitvragen() == TRUE) 
           {div(
             div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_private_beleggingen', vertaler$t("vorige_pagina"))),
             #volgende button verschijnt pas na download, wordt in de server gemaakt
             div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_rapport'))
           )} else
           {div(
             div(style="display: inline-block; width: 95px ;", actionButton(class = "back_button", 'jump_back_ToP_duurzaamheid', vertaler$t("vorige_pagina"))),
             #volgende button verschijnt pas na download, wordt in de server gemaakt
             div(style="display: inline-block; width: 65px ;", uiOutput('ui_jumpToP_rapport'))
           )}
    )
  })
  
  #nadere toelichting doelstelling box
  output$ui_cond_panel_toelichting_doelstelling <- renderUI({
      conditionalPanel(
        condition = "input.beleggingsdoelstelling_toelichting.includes('graag') || input.beleggingsdoelstelling_toelichting.includes('please')",
        #condition = "input.beleggingsdoelstelling_toelichting == `ja graag - hieronder opent een tekstvakje waarin u een toelichting kunt schrijven`",
        list(
          #met autocomplete uitzetten, anders worden eerder ingevulde namen getoond
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
    selectizeInput(
      inputId = onderwerp_vraag(id),
      label = str_c(sub_nummer_vraag(id), ") ", vraag(id)),
      choices = opties_vraag(id),
      selected = if(test_modus()) {selected_vraag(id)} else {NULL},
      width = '6500px')
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

  #de volgende knoppen moeten pas verschijnen na de plot
  #daarom hier in de server gedefinieerd
  output$ui_jumpToP_visuele_keuze2 <- renderUI ({
    actionButton(class = "button", 'jumpToP_visuele_keuze2', vertaler$t("volgende_pagina"))
  })
  output$ui_jumpToP_duurzaamheid <- renderUI ({
    actionButton(class = "button", 'jumpToP_duurzaamheid', vertaler$t("volgende_pagina"))
  })
  output$ui_jumpToP_private_beleggingen <- renderUI ({
    actionButton(class = "button", 'jumpToP_private_beleggingen', vertaler$t("volgende_pagina"))
  })
  output$ui_jumpToP_rapport <- renderUI ({
    actionButton(class = "button", 'jumpToP_rapport', vertaler$t("volgende_pagina"))
  })
  output$ui_jumpToP_overig <- renderUI ({
    actionButton(class = "button", 'jumpToP_overig', vertaler$t("volgende_pagina"))
  })
  #===

  
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
    #alles uitschakelen, weer inschakelen na invullen (JumpTo etc) per ingevulde pagina
     shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_start"]')
    #
     shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_beleggingsdoelstelling"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_financiele_situatie"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_kennis_ervaring"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_risicobereidheid"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_startvermogen"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_visuele_keuze1"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_visuele_keuze2"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_duurzaamheid"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_private_beleggingen"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_rapport"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_voorlopige_conclusie"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_tabel"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_overig"]')
    shinyjs::disable(selector = '.navbar-nav a[data-value = "panel_projecties"]')
  }
  #alle panels uitschakelen - deze functie ook doen bij "Opnieuw beginnen"
  alle_panels_uitschakelen()

  #past ggplot aan aan overall theme
  thematic::thematic_shiny()

  #==== back_buttons
  my_backbutton_observeEvent <- function(panel) {
    observeEvent(input[[paste0("jump_back_ToP_", panel)]], {
      updateTabsetPanel(session, inputId = "tabset",
                        selected = str_c("panel_", panel))
    })
  }
  #=== terug springen naar ...
  pmap(panels, my_backbutton_observeEvent)

  #=== vanaf einde weer naar begin springen
  observeEvent(input$jumpToP_start, {
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
    updateTabsetPanel(session, inputId = "tabset", selected = "panel_start")
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
  # observeEvent(input$jumpToP_private_beleggingen, {
  #   output$error_msg_duurzaamheid <- renderText({
  #     if(meer_vragen_duurzaamheid()) {
  #       validate_need_functie("duurzaamheid") #alle vier vragen
  #     } else { #slechts twee vragen
  #       shiny::validate(
  #         shiny::need(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
  #                       c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
  #                         eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))),
  #                     'Maak een keuze bij alle bovenstaande vragen
  #                   '))
  #     }
  #   })
  #   if(meer_vragen_duurzaamheid()) {
  #     req_functie("duurzaamheid") #alle vier vragen
  #   } else { #slechts twee vragen
  #     shiny::req(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
  #                  c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
  #                    eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))))
  #   }
  #   shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_private_beleggingen"]')
  #   updateTabsetPanel(session, inputId = "tabset", selected = "panel_private_beleggingen")
  # })
  #===
  
  private_beleggingen_tab_geopend <- reactiveValues()
  private_beleggingen_tab_geopend$geopend <- 0
  
  observeEvent(input$jumpToP_overig, {
  
      output$error_msg_duurzaamheid <- renderText({
        if(meer_vragen_duurzaamheid()) {
          validate_need_functie("duurzaamheid") #alle vier vragen
        } else { #slechts twee vragen
          shiny::validate(
            shiny::need(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
                          c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
                            eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))),
                        vertaler$t("maak_keuze_alle_bovenstaande_vragen")))
        }
      })
      if(meer_vragen_duurzaamheid()) {
        req_functie("duurzaamheid") #alle vier vragen
      } else { #slechts twee vragen
        shiny::req(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
                     c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
                       eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))))
      }
      #shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_overig"]')
      #updateTabsetPanel(session, inputId = "tabset", selected = "panel_overig")
      
      if (private_beleggingen_uitvragen() == TRUE & private_beleggingen_tab_geopend$geopend == 0) {
         insertTab(inputId = "tabset",
                  tabPanel(title = span("Extra" , class = "custom2-tab"), value = "panel_private_beleggingen",
                           h3(uiOutput("ui_private_beleggingen")),
                           p(uiOutput("ui_private_beleggingen_zijn_intro")),
                           p(uiOutput("ui_private_beleggingen_zijn_heel_divers")),
                           p(uiOutput("ui_private_beleggingen_voordelen")),
                           p(uiOutput("ui_private_beleggingen_nadelen")),
                           p(uiOutput("ui_private_beleggingen_kosten")),
                           p(uiOutput("ui_private_beleggingen_duidelijkheid_horizon")),
                           p(uiOutput("ui_private_beleggingen_mogelijk_geschikt")),
                           p(uiOutput("ui_private_beleggingen_bavd_kan_helpen")),
                           p(uiOutput("ui_hieronder_vraag_interesse_private_beleggingen")),
                           uiOutput("ui_selectize_private_beleggingen"),
                           textOutput('error_msg_private_beleggingen'),
                           actionButton(class = "back_button", 'jump_back_ToP_duurzaamheid', vertaler$t("vorige_pagina")),
                           actionButton(class = "button", 'jumpToP_overig2', vertaler$t("volgende_pagina"))
                  ),
                           target = "panel_duurzaamheid", position = "after",
                           select = TRUE, session = getDefaultReactiveDomain()
                  )
        private_beleggingen_tab_geopend$geopend <- 1
      }
      if(private_beleggingen_uitvragen() == TRUE) {shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_private_beleggingen"]')} 
      else {shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_overig"]')}
      updateTabsetPanel(session, inputId = "tabset", selected = if(private_beleggingen_uitvragen() == TRUE) {"panel_private_beleggingen"} else {"panel_overig"})
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
  
  
  # observeEvent(input$jumpToP_rapport, {
  #   output$error_msg_duurzaamheid <- renderText({
  #     if(meer_vragen_duurzaamheid()) {
  #       validate_need_functie("duurzaamheid") #alle vier vragen
  #      } else { #slechts twee vragen
  #       shiny::validate(
  #         shiny::need(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
  #                       c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
  #                         eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))),
  #                     'Maak een keuze bij alle bovenstaande vragen
  #                   '))
  #       }
  #     })
  #   if(meer_vragen_duurzaamheid()) {
  #     req_functie("duurzaamheid") #alle vier vragen
  #   } else { #slechts twee vragen
  #     shiny::req(!opties_vraag(vraagnummers_categorie("duurzaamheid")[1])[1] %in%
  #                  c(eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[1])))),
  #                    eval(parse(text = str_c("input$", onderwerp_vraag(vraagnummers_categorie("duurzaamheid")[2]))))))
  #   }
  #     shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_rapport"]')
  #     updateTabsetPanel(session, inputId = "tabset",
  #                       selected = "panel_rapport")
  # })

  #===
#========
  
  
  # observeEvent(input$jumpToP_overig, {
  #   if (private_beleggingen_uitvragen() == TRUE & private_beleggingen_tab_geopend$geopend == 0) {
  #     
  #   insertTab(inputId = "tabset",
  #           tabPanel(title = "Private beleggingen", value = "panel_private_beleggingen",
  #                    h3("Private beleggingen"),
  #            #         actionButton(class = "back_button", 'jump_back_ToP_rapport', 'Ga terug'),
  #            #         actionButton(class = "werk_button", 'jumpToP_projecties', "Projecties"),
  #            #         shinycssloaders::withSpinner(
  #            #           p(style = "text-align: justify;", htmlOutput("check_pagina")),
  #            #           color = euroblue, #kleur van spinner
  #            #         ),
  #            #         h4(textOutput('profiel_conclusie_tekst')),
  #            #         h4("Vertaling punten naar profiel per categorie: het te behalen aantal punten boven het minimale aantal punten
  #            # wordt in vijf gelijke delen gesplitst. Het behaalde aantal punten valt in een van die vijf gebieden, die corresponderen met een profiel."),
  #            #         p(style = "text-align: justify;",
  #            #           htmlOutput("vertaaltabel_beleggingsdoelstelling"),
  #            #           htmlOutput("vertaaltabel_financiele_situatie"),
  #            #           htmlOutput("vertaaltabel_risicobereidheid"),
  #            #           htmlOutput("vertaaltabel_visueel"),
  #            #           htmlOutput("vertaaltabel_kennis_ervaring"))
  #            actionButton(class = "back_button", 'jump_back_ToP_duurzaamheid', 'Ga terug'),
  #            actionButton(class = "button", 'jumpToP_overig', "Ga naar de volgende pagina")
  #           ),
  #           target = "panel_duurzaamheid", position = "after",
  #           select = TRUE, session = getDefaultReactiveDomain())
  #   }
  #   private_beleggingen_tab_geopend$geopend <- 1
  #   shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_overig"]')
  #   updateTabsetPanel(session, inputId = "tabset", selected = "panel_overig")
  # })
  
  #========
  
  geklikt_op_voorlopige_conclusie <- reactiveValues()
  geklikt_op_voorlopige_conclusie$geklikt <- 0

  observeEvent(input$jumpToP_voorlopige_conclusie, {
    if (geklikt_op_voorlopige_conclusie$geklikt == 0) {
      insertTab(inputId = "tabset",
           tabPanel(title = span(vertaler$t("voorlopige_conclusie"), class = "custom-tab"), value = "panel_voorlopige_conclusie",
             h3(vertaler$t("resultaat_per_categorie")),
             actionButton(class = "back_button", 'jump_back_ToP_rapport', vertaler$t("vorige_pagina")),
             actionButton(class = "werk_button", 'jumpToP_projecties', vertaler$t("projecties")),
             shinycssloaders::withSpinner(
               p(style = "text-align: justify;", htmlOutput("check_pagina")),
               color = euroblue, #kleur van spinner
             ),
             h4(textOutput('profiel_conclusie_tekst')),
             h4(vertaler$t("vertaling_punten_naar_profiel_uitleg")),
             # Vertaling punten naar profiel per categorie: het te behalen aantal punten boven het minimale aantal punten
             # wordt in vijf gelijke delen gesplitst. Het behaalde aantal punten valt in een van die vijf gebieden, die corresponderen met een profiel."),
             p(style = "text-align: justify;gap: 50px;",
               htmlOutput("vertaaltabel_beleggingsdoelstelling"),
               htmlOutput("vertaaltabel_financiele_situatie"),
               htmlOutput("vertaaltabel_risicobereidheid"),
               htmlOutput("vertaaltabel_visueel"),
               htmlOutput("vertaaltabel_kennis_ervaring"))
            ),
        target = "panel_rapport", position = "after",
        select = TRUE, session = getDefaultReactiveDomain())
    insertTab(inputId = "tabset",
        tabPanel(title = span(vertaler$t("projecties"), class = "custom-tab"), value = "panel_projecties",
                 h3(vertaler$t("projecties_in_rapport")),
                 h4(vertaler$t("voor_toelichting_zie_rapport")),
                 actionButton(class = "back_button", 'jump_back_ToP_voorlopige_conclusie', vertaler$t("vorige_pagina")),
                 actionButton(class = "werk_button", 'jumpToP_tabel', vertaler$t("tabel")),
                 scenariosplot_module_UI(id = "scenariosplot_module", vertaler = vertaler)
        ),
        target = "panel_voorlopige_conclusie", position = "after",
        select = TRUE, session = getDefaultReactiveDomain())
    insertTab(inputId = "tabset",
      tabPanel(title = span(vertaler$t("tabel"), class = "custom-tab"), value = "panel_tabel",
               actionButton(class = "back_button", 'jump_back_ToP_projecties', vertaler$t("vorige_pagina")),
               #actionButton(class = "button", 'jumpToP_belangrijke_informatie', "Belangrijke informatie"),
               #actionButton(class = "back_button", 'jump_back_ToP_tabel', 'Ga terug'),
               actionButton(class = "werk_button", 'jumpToP_start', vertaler$t("ga_terug_naar_start")),
               h3(vertaler$t("tabel_in_rapport")),
               h4(vertaler$t("voor_toelichting_zie_rapport")),
               shinycssloaders::withSpinner(
                 tableOutput("tabel_rapport"),
                 color = euroblue, #kleur van spinner
               )
      ),
      target = "panel_projecties", position = "after",
      select = TRUE, session = getDefaultReactiveDomain())
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
  # observeEvent(input$jumpToP_belangrijke_informatie, {
  #   shinyjs::enable(selector = '.navbar-nav a[data-value = "panel_belangrijke_informatie"]')
  #   updateTabsetPanel(session, inputId = "tabset", selected = "panel_belangrijke_informatie")
  # })

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
          tags$img(src = "logo.png", width = "200px", style = "display: block; margin: 0 auto;")
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
  
  # Sluit modal bij klik op OK, werkt voor beide dialoogjes
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
          tags$img(src = "logo.png", width = "200px", style = "display: block; margin: 0 auto;")
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
  
  #wil niet meer escapen:
  # observeEvent(input$grafieken_info1, {
  #   popup_content <-
  #       tagList(
  #         tags$h4(vertaler$t("toelichting_grafieken")),
  #         
  #         tags$p(
  #           vertaler$t("grafieken_toelichting_intro"), 
  #           
  #           vertaler$t("getoonde_bedragen_in_toelichting_grafieken"),
  #           format_bedrag_tabellen(startvermogen()), ". ",
  #           vertaler$t("eventuele_so_buiten_beschouwing_gelaten"),
  #           tags$br(),
  #           vertaler$t("toelichting_grafieken_gunstig_ongunstig"),
  #           vertaler$t("toelichting_grafieken_grotere_spreiding")
  #         )
  #       )
  #   shinyalert(
  #     title = "",
  #     text = (popup_content),
  #     size = "m",
  #     closeOnEsc = TRUE,
  #     closeOnClickOutside = TRUE,
  #     html = TRUE,
  #     type = "",
  #     showConfirmButton = TRUE,
  #     showCancelButton = FALSE,
  #     confirmButtonText = "OK",
  #     confirmButtonCol = "#94bcb2",
  #     timer = 0,
  #     imageUrl = "logo.png",
  #     imageWidth = 200,
  #     #imageHeight = 100,
  #     animation = TRUE
  #   )})
  #grafieken info2
  # observeEvent(input$grafieken_info2, {
  #   shinyalert(
  #     title = "",
  #     text = str_c("
  #     <html>
  #     <h3>Toelichting bij de grafieken</h3>
  #     <p>
  #     In de grafieken worden drie risicoprofielen naast elkaar weergegeven.
  #     In het midden staat het profiel dat u koos op de vorige pagina. Rechts en links daarvan staan resp. een risicovoller en een minder risicovol profiel.
  #     De verticale as geeft het 3-jaars rendement weer en de horizontale as hoe vaak in duizend gevallen dit rendement kan voorkomen.
  #     De getoonde bedragen zijn bedragen na 3 jaar, uitgaande van het te beleggen vermogen van ", format_bedrag_tabellen(startvermogen()), ".
  #     Hierbij zijn eventuele stortingen of onttrekkingen buiten beschouwing gelaten.
  #     \"Gunstig\" wil zeggen beter dan 95% van de gevallen, \"ongunstig\" slechter dan 95% van de gevallen.
  #     Hoe groter de spreiding van de uitkomsten, hoe risicovoller het profiel.
  #     </p>
  #     </html>"),
  #     size = "m",
  #     closeOnEsc = TRUE,
  #     closeOnClickOutside = TRUE,
  #     html = TRUE,
  #     type = "",
  #     showConfirmButton = TRUE,
  #     showCancelButton = FALSE,
  #     confirmButtonText = "OK",
  #     confirmButtonCol = "#94bcb2",
  #     timer = 0,
  #     imageUrl = "logo.png",
  #     imageWidth = 200,
  #     #imageHeight = 100,
  #     animation = TRUE
  #   )})


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
     kbl(caption = tolower(vertaler$t(categorie)), align = "c") %>%
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
      kbl(caption = vertaler$t("visueel"), align = "c") %>%
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
      #tibble(!!vertaler$t("vraag") := c(vertaler$t("uw_naam"), kan met rename
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
      grep("I -|V -|Voorlopige conclusie -|Preliminary conclusion -|Private beleggingen -|Private investments",  data_rapport()$Vraag) #hij pakt de eerste niet!?
      #grep("[I|V|Voorlopige conclusie]",  data_rapport()$`Vraag`) #hij pakt de eerste niet!?
      # Build the regex pattern dynamically from translated strings
      # pattern <- paste(
      #   vertaler$t("roman-one-prefix"),          # e.g., "I -" in EN, "I -" in NL (roman numerals are often consistent)
      #   vertaler$t("roman-five-prefix"),         # e.g., "V -"
      #   vertaler$t("prelim-conclusion-nl"),      # e.g., "Voorlopige conclusie -" in NL
      #   vertaler$t("prelim-conclusion-en"),      # e.g., "Preliminary conclusion -" in EN
      #   vertaler$t("private-investments-nl"),    # e.g., "Private beleggingen -" in NL
      #   vertaler$t("private-investments-en"),    # e.g., "Private investments" in EN
      #   sep = "|"  # Alternation in regex
      # )
      # 
      # # Now use the built pattern
      # grep(pattern, data_rapport()$Vraag)
    })

    #html tabel voor op de pagina Tabel, na verzenden van het rapport
    output$tabel_rapport <- function() {
      data_rapport() %>%
        as_tibble() %>%
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
          tags$img(src = "logo.png", width = "200px", style = "display: block; margin: 0 auto;")
       ),
        title = "Titel",#vertaler$t("bevestig_genereer_rapport"),   # e.g. "Bevestig rapport genereren"
        size = "m",
        easyClose = FALSE
      ),
        footer = tagList(
          modalButton("annuleren"),#vertaler$t("annuleren")),
          actionButton("bevestig_rapport_maken",
                       label = "start rapport", #vertaler$t("ok_start_download"),
                       class = "button",
                       icon = icon("check"))
        ),
        tags$div(
          style = "text-align: center; padding: 20px;",
          tags$h4(vertaler$t("rapport_maken_en_versturen")),
          #tags$p(vertaler$t("na_ok_start_download")),   # customize these translations
          tags$p(style = "color: #666; font-size: 0.95em;",
                 vertaler$t("het_maken_en_versturen_kan_even_duren"))
        )
      ))
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
        
        str_c("Beleggersprofiel ", naam_in_file_en_aanhef(), " - ", tijd, ".pdf")
      },
      
      content = function(file) {
        withProgress(
          message = vertaler$t("rapport_maken"), 
          detail = NULL, 
          value = 0.1, {
            
            # ────────────────────────────────────────────────────────
            #   Your COMPLETE original content logic starts here
            #   (almost unchanged – just moved inside)
            # ────────────────────────────────────────────────────────
            
            subject <- str_c("Beleggersprofiel ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip)
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
            
            # envelope(
            #   from = "pieterprins2@gmail.com",
            #   to = unlist(str_split(str_replace_all(str_replace_all(input$emailadres, ";", ","), " ", ""), ",")),
            #   cc = if(test_modus()) {NULL} else {"maingay@bavandoorn.nl"},
            #   subject = subject
            # ) %>%
            #   text(str_c("Beleggersprofiel ", naam_in_file_en_aanhef())) %>%
            #   attachment(file, disposition = "attachment", name = pdf_filename) |>
            #   smtp()
            # 
            # setProgress(value = 0.9, message = vertaler$t("rapport_versturen"))
            # 
            # envelope(
            #   from = "pieterprins2@gmail.com",
            #   to = "pieterprins@yahoo.com",   # your test/admin logic here
            #   subject = subject
            # ) %>%
            #   text(str_c("Beleggersprofiel ", naam_in_file_en_aanhef(), " csv-, pdf- en png-bestanden")) %>%
            #   attachment(str_c("Beleggersprofiel_voor_AIRS ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv")) %>%
            #   attachment(str_c("Beleggersprofiel_voor_Beleggingsvoorstel ", naam_in_file_en_aanhef(), " - ", download$download_tijdstip, ".csv")) %>%
            #   attachment(file, disposition = "attachment", name = pdf_filename) |>
            #   attachment(file.path(tmp_dir, png_3jrs_name), disposition = "attachment", name = png_3jrs_name) %>%
            #   attachment(file.path(tmp_dir, png_scen_name), disposition = "attachment", name = png_scen_name) %>%
            #   smtp()
            # 
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
    observeEvent(download$success_trigger, {
      req(download$success_trigger > 0)
      showModal(modalDialog(
        title = NULL,
        size = "l",
        easyClose = TRUE,
        tags$div(
          style = "text-align: center; max-width: 800px; margin: 0 auto; color: #808080; font-size: 18px; padding: 30px;",
          tags$div(
            style = "margin-bottom: 30px;",
            tags$img(src = "logo.png", width = "200px", style = "display: block; margin: 0 auto;")
          ),
          tags$h3(vertaler$t("dank_u_wel"), style = "margin-bottom: 20px;"),
          tags$p(
            vertaler$t("rapport_is_gemaakt"), tags$br()
          )
        ),
        footer = tags$div(
          style = "text-align: center; width: 100%;",
          actionButton(
            "modal_ok",
            "OK",
            style = "background-color: #94bcb2; color: white; border: none; padding: 10px 40px; font-size: 16px;"
          )
        )
      ))
    })
    
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
