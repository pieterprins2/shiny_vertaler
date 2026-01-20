library(feather)
library(ggplot2)
library(gridExtra)
library(shinycssloaders)
library(shiny)
library(shiny.i18n)

ba_color <- "#4E9080"
usred <- "#bf0a30"

# Updated format_bedrag_grafieken (accepts marks as params)
format_bedrag_grafieken <- function(kwalificatie, bedrag, big_mark_local, decimal_mark_local) {
  paste0(kwalificatie,
         " € ", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
                        format="f", digits=0, big.mark = big_mark_local, decimal.mark = decimal_mark_local))
}

# Updated assen_pijlen_en_tekst_voor_bar_charts_custom (accepts marks)
assen_pijlen_en_tekst_voor_bar_charts_custom <- function(startvermogen, vertaler, horizon, max_y, mean, q95, q05, anoniem, big_mark_local, decimal_mark_local) {
  list(
    scale_y_continuous(limits = c(0, max_y)),
    scale_x_continuous(labels = scales::percent_format(accuracy = 2),
                       breaks = seq(-0.5, 0.5, 0.1),
                       limits = c(-.225, .325),
                       sec.axis = dup_axis()),
    annotate(geom = "label",
             x = (1 + mean)^(1/horizon) - 1,
             y = max_y * 3/6,
             label = str_c(
               format_bedrag_grafieken(kwalificatie = vertaler$t("gematigd"), bedrag = startvermogen * (1 + mean), big_mark_local = big_mark_local, decimal_mark_local = decimal_mark_local),
               " (",
               format(round(100 * ((1 + mean)^(1/horizon) - 1), 1), decimal.mark = decimal_mark_local, nsmall = 1),
               "%",
               if(horizon > 1) {str_c(" ", vertaler$t("per_jaar_afko"))} else {""},
               ")"),
             color = "gray30", fill = "white", size = 5, alpha = .9),
    annotate(geom = "label", x = (1 + q95)^(1/horizon) - 1, y = max_y * 3/6,
             label = str_c(
               format_bedrag_grafieken(kwalificatie = vertaler$t("gunstig"), bedrag = startvermogen * (1 + q95), big_mark_local = big_mark_local, decimal_mark_local = decimal_mark_local),
               " (",
               format(round(100 * ((1 + q95)^(1/horizon) - 1), 1), decimal.mark = decimal_mark_local, nsmall = 1),
               "%",
               if(horizon > 1) {str_c(" ", vertaler$t("per_jaar_afko"))} else {""},")"),
             color = ba_color, fill = "white", size = 5, alpha = .9),
    annotate(geom = "label", x = (1 + q05)^(1/horizon) - 1, y = max_y * 3/6,
             label = str_c(
               format_bedrag_grafieken(kwalificatie = vertaler$t("ongunstig"), bedrag = startvermogen * (1 + q05), big_mark_local = big_mark_local, decimal_mark_local = decimal_mark_local),
               " (",
               format(round(100 * ((1 + q05)^(1/horizon) - 1), 1), decimal.mark = decimal_mark_local, nsmall = 1),
               "%",
               if(horizon > 1) {str_c(" ", vertaler$t("per_jaar_afko"))} else {""},")"),
             color = usred, fill = "white", size = 5, alpha = .9),
    if(anoniem == TRUE)
    {annotate(geom = "label", y = max_y/2, x = .32,
              label = str_c(horizon, "-", vertaler$t("jaars_horizon"), ", ",
                            format_bedrag_grafieken(kwalificatie = vertaler$t("vermogen"), bedrag = startvermogen, big_mark_local = big_mark_local, decimal_mark_local = decimal_mark_local)),
              color = "gray30", size = 5)
    },
    if(anoniem == TRUE)
    {theme(strip.text.x = element_blank())
    }
  )
}

bar_charts_custom <-
  list(
    scale_x_continuous(labels = scales::percent_format(accuracy = 2),
                       breaks = seq(-0.5, 0.5, 0.1),
                       limits = c(-0.19, 0.29)),
    facet_wrap(~label, ncol = 2),
    geom_vline(xintercept = 0, linewidth = 1, color = "gray", linetype =2 ),
    scale_fill_manual(breaks = levels(cut), values = c(ba_color, usred)),
    scale_alpha_manual(values = c(0.5, 0.5)),
    stat_bin(breaks = seq(-0.5, 0.6, 0.02)),
    labs(x = "", y = ""),
    coord_flip(),
    theme_bw(),
    theme(legend.position = "none",
          axis.text = element_text(size = 15),
          strip.text.x = element_text(size = 15))
  )

#barchart met kleuren en alfa, nog zonder de bedragen en labels eroverheen
barplot <- function(benchmark) {
  ggplot(data = data_voor_barcharts %>% filter(benchmark == !!benchmark),
         aes(x = geom_ann_return, alpha = alpha, fill = fill)) +
    geom_histogram(aes(y = after_stat(count)/sum(after_stat(count))), bins = 30, col = "gray80")
}

#grootste bar size
max_y_barplot <- function(benchmark) {
  #max(ggplot_build(barplot(benchmark = benchmark))$data[[1]][["count"]])
  case_when(
    benchmark == "RM100" ~ 271*1.3,
    benchmark == "RD30RM70" ~ 255*1.3,
    benchmark == "RD50RM50" ~ 152*1.3,
    benchmark == "RD70RM30" ~ 152*1.3,
    benchmark == "RD100" ~ 152*1.3)
}

#max_y hangt niet per se af van benchmark, maar ook van de benchmark ernaast, dus de max van de 2
barplot_compleet <- function(benchmark, vertaler, max_y, anoniem, startvermogen, big_mark_local, decimal_mark_local) {
  barplot(benchmark = benchmark) +
    bar_charts_custom +
    assen_pijlen_en_tekst_voor_bar_charts_custom(
      startvermogen = startvermogen,
      vertaler = vertaler,
      horizon = 3,
      max_y = max_y,
      mean = data_voor_barcharts %>% filter(benchmark == !!benchmark) %>% select(mean) %>% tail(1) %>% pull(),
      q05 = data_voor_barcharts %>% filter(benchmark == !!benchmark) %>% select(q05) %>% tail(1) %>% pull(),
      q95 = data_voor_barcharts %>% filter(benchmark == !!benchmark) %>% select(q95) %>% tail(1) %>% pull(),
      anoniem = anoniem,
      big_mark_local = big_mark_local,
      decimal_mark_local = decimal_mark_local
    )
}

# Module UI Function
barplot_een_module_UI <- function(id, vertaler) {
  ns <- NS(id)
  fluidRow(column(12,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("barplot_een_module"),
                               height = "650",
                               width = "1300"),
                    color = "#4E9080")
  )
  )
}

# Module UI Function
barplot_twee_module_UI <- function(id, vertaler) {
  ns <- NS(id)
  fluidRow(column(12,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("barplot_twee_module"),
                               height = "650",
                               width = "1300"),
                    color = "#4E9080")
  )
  )
}

# Module UI Function
barplot_module_UI <- function(id, vertaler) {
  ns <- NS(id)
  fluidRow(column(12,
                  shinycssloaders::withSpinner(
                    plotOutput(outputId = ns("barplot_module"),
                               height = "650",
                               width = "1300"),
                    color = "#4E9080")
  )
  )
}

index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr <-
  read_feather("index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr_vijf_250.feather")

# data_voor_barcharts – in app gedefinieerd, but for completeness, add filter
data_voor_barcharts <-
  index_paden_360m_maal1000_params_12_36_60_120_360_geom_returns_met_bijst_onttr %>%
  filter(horizon == "3 jaar")

# Module Server Function
#===barPlot
barplot_een_Server <- function(id, vertaler, taal, data_voor_barcharts, anoniem, startvermogen) {
  
  moduleServer(id, module = function(input, output, session) {
    
    # Reactives for marks
    big_mark_local <- reactive({
      if (taal() == "nl") "." else ","
    })
    
    decimal_mark_local <- reactive({
      if (taal() == "nl") "," else "."
    })
    
    barplot_sbs_een <- reactive({
      
      data_voor_barcharts <- data_voor_barcharts
      anoniem <- anoniem
      startvermogen = startvermogen()
      
      max_y <- max_y_barplot("RD30RM70")
      plot1 <- barplot_compleet(benchmark = "RD70RM30", vertaler = vertaler, max_y = max_y_barplot("RD30RM70"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()) #max_y afgesteld op het laagste risico
      plot2 <- barplot_compleet(benchmark = "RD30RM70", vertaler = vertaler, max_y = max_y_barplot("RD30RM70"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
      grid.arrange(plot1 +
                     labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"),
                          y = vertaler$t("aantal_per_duizend_gevallen")) +
                     theme(axis.title.y.right = element_blank(),
                           axis.text.y.right = element_blank(),
                           axis.ticks.y.right = element_blank()) +
                     annotate(geom = "text", y = max_y/2, x = .29,
                              label = vertaler$t("profiel_met_hoger_risico"),
                              color = "gray30", size = 5),
                   plot2 +
                     labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"),
                          y = vertaler$t("aantal_per_duizend_gevallen")) +
                     theme(axis.title.y.left = element_blank(),
                           axis.text.y.left = element_blank(),
                           axis.ticks.y.left = element_blank()) +
                     annotate(geom = "text", y = max_y/2, x = .29,
                              label = vertaler$t("profiel_met_lager_risico"),
                              color = "gray30", size = 5),
                   ncol = 2)
    })
    output$barplot_een_module <- renderPlot({
      barplot_sbs_een()
    })
  })
}

barplot_twee_Server <- function(id, vertaler, taal, data_voor_barcharts, anoniem, keuze_visueel1, startvermogen) {
  
  moduleServer(id, module = function(input, output, session) {
    
    # Reactives for marks
    big_mark_local <- reactive({
      if (taal() == "nl") "." else ","
    })
    
    decimal_mark_local <- reactive({
      if (taal() == "nl") "," else "."
    })
    
    barplot_sbs_twee <- reactive({
      
      data_voor_barcharts <- data_voor_barcharts
      anoniem <- anoniem
      keuze_visueel1 = keuze_visueel1()
      startvermogen = startvermogen()
      
      if(keuze_visueel1 == "RD70RM30") {
        max_y <- max_y_barplot("RD30RM70")
        plot1 <- barplot_compleet(benchmark = "RD100", vertaler = vertaler, max_y = max_y_barplot("RD30RM70"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        plot2 <- barplot_compleet(benchmark = "RD50RM50", vertaler = vertaler, max_y = max_y_barplot("RD30RM70"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        plot3 <- barplot_compleet(benchmark = "RD70RM30", vertaler = vertaler, max_y = max_y_barplot("RD30RM70"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        grid.arrange(plot1 +
                       labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"),
                            y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.right = element_blank(),
                             axis.text.y.right = element_blank(),
                             axis.ticks.y.right = element_blank()) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("profiel_met_hoger_risico"),
                                color = "gray30", size = 5),
                     plot3 +
                       labs(y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.left = element_blank(),
                             axis.text.y.left = element_blank(),
                             axis.ticks.y.left = element_blank(),
                             axis.title.y.right = element_blank(),
                             axis.text.y.right = element_blank(),
                             axis.ticks.y.right = element_blank()) +
                       theme(plot.background = element_rect(color = ba_color, linewidth = 5)) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("keuze_vorige_pagina"),
                                color = "gray30", size = 5),
                     plot2 +
                       labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"),
                            y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.left = element_blank(),
                             axis.text.y.left = element_blank(),
                             axis.ticks.y.left = element_blank()) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("profiel_met_lager_risico"),
                                color = "gray30", size = 5),
                     ncol = 3)
      } else {
        max_y <- max_y_barplot("RM100")
        plot1 <- barplot_compleet(benchmark = "RD50RM50", vertaler = vertaler, max_y = max_y_barplot("RM100"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        plot2 <- barplot_compleet(benchmark = "RM100", vertaler = vertaler, max_y = max_y_barplot("RM100"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        plot3 <- barplot_compleet(benchmark = "RD30RM70", vertaler = vertaler, max_y = max_y_barplot("RM100"), anoniem = anoniem, startvermogen = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local())
        grid.arrange(plot1 +
                       labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"), #driejaars rendement op jaarbasis",
                            y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.right = element_blank(),
                             axis.text.y.right = element_blank(),
                             axis.ticks.y.right = element_blank()) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("profiel_met_hoger_risico"),
                                color = "gray30", size = 5),
                     plot3 +
                       labs(y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.left = element_blank(),
                             axis.text.y.left = element_blank(),
                             axis.ticks.y.left = element_blank(),
                             axis.title.y.right = element_blank(),
                             axis.text.y.right = element_blank(),
                             axis.ticks.y.right = element_blank()) +
                       theme(plot.background = element_rect(color = ba_color, linewidth = 5)) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("keuze_vorige_pagina"),
                                color = "gray30", size = 5),
                     plot2 +
                       labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"),
                            y = vertaler$t("aantal_per_duizend_gevallen")) +
                       theme(axis.title.y.left = element_blank(),
                             axis.text.y.left = element_blank(),
                             axis.ticks.y.left = element_blank()) +
                       annotate(geom = "text", y = max_y/2, x = .29,
                                label = vertaler$t("profiel_met_lager_risico"),
                                color = "gray30", size = 5),
                     ncol = 3)
      }
    })
    
    output$barplot_twee_module <- renderPlot({
      barplot_sbs_twee()
    })
  })
}

# barplot_drie_Server (updated with taal and marks)
barplot_drie_Server <- function(id, vertaler, taal, data_voor_barcharts, anoniem, profiel_conclusie, startvermogen) {
  moduleServer(id, function(input, output, session) {
    
    # Reactives for marks
    big_mark_local <- reactive({
      if (taal() == "nl") "." else ","
    })
    
    decimal_mark_local <- reactive({
      if (taal() == "nl") "," else "."
    })
    
    bm_3jaars_plot <- reactive({
      data_voor_barcharts <- data_voor_barcharts
      anoniem <- anoniem
      profiel_conclusie <- profiel_conclusie()
      startvermogen = startvermogen()
      
      barplot_compleet(benchmark = profiel_conclusie,
                       vertaler = vertaler,
                       max_y = max_y_barplot(benchmark = profiel_conclusie),
                       anoniem = anoniem,
                       startvermogen = startvermogen,
                       big_mark_local = big_mark_local(),
                       decimal_mark_local = decimal_mark_local()) +
        labs(x = vertaler$t("driejaars_rendement_op_jaarbasis"), y = vertaler$t("aantal_per_duizend_gevallen")) +
        annotate(geom = "label", x = 0.32, y = max_y_barplot(benchmark = profiel_conclusie)/2, 
                 label = str_c(vertaler$t("profiel"), " ", profiel_conclusie, ", horizon 3 ", vertaler$t("jaar")), 
                 colour = "gray30",fill = "white", alpha = .9, size = 6) +
        theme(axis.title.y.right = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank(),
              legend.position = "none",
              axis.text.x = element_text(size = 10, colour = "gray20"),
              axis.text.y = element_text(size = 10, colour = "gray20"),
              #strip.text.x = element_text(size = 10)
              #van stack overflow geplukt: oplossing voor verwijderen facet balk
              strip.background = element_blank(), 
              strip.text = element_text(color = "transparent"),
              panel.spacing.y = unit(-0.8, "lines")
        )
    })
    
    #output$barplot_drie_module <- renderPlot({
    # bm_3jaars_plot()
    #})
    
    # Return for use elsewhere (e.g., PDF)
    return(bm_3jaars_plot)
  })
}
