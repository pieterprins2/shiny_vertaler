
ba_color <- "#4E9080"
sim_lijntjes_kleur <- "#E59D00"
darkgray <- "#252525" #brewer.pal(n = 9, name = 'Greys')[8]
euroblue <- "#004494"
euroyellow <- "#ffd617"
usred <- "#bf0a30"

# parameters
parameters <-
  read_xlsx("parameters.xlsx", sheet = "Sheet1")
inflatie <- parameters %>% filter(parameter == "inflatie") %>% select(waarde) %>% pull()

# Module UI Function
scenariosplot_module_UI <- function(id, vertaler) {
  ns <- NS(id)
  fluidRow(column(12,
                  withSpinner(
                    plotOutput(outputId = ns("scenariosplot_module"),
                               height = "650px",
                               width = "900px"),
                    color = euroblue)
  )
  )
}

# Module Server Function
#===scenariosPlot
scenariosplotServer <- function(id,
                                vertaler,
                                taal,
                                df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros,
                                horizon,
                                profiel_conclusie,
                                startvermogen,
                                bijstorting) {
  
  moduleServer(id, module = function(input, output, session) {
    
    # Reactives for marks
    big_mark_local <- reactive({
      if (taal() == "nl") "." else ","
    })
    
    decimal_mark_local <- reactive({
      if (taal() == "nl") "," else "."
    })
    
    # Updated format_bedrag_grafieken (accepts marks as params)
    format_bedrag_grafieken_local <- function(kwalificatie, bedrag, big_mark_local, decimal_mark_local) {
      paste0(kwalificatie,
             " € ", formatC(as.numeric(round(bedrag/1000, 0) * 1000),
                            format="f", digits=0, big.mark = big_mark_local, decimal.mark = decimal_mark_local))
    }
    
    # Updated format_bedrag_grafieken_bijstorting (accepts marks as params)
    format_bedrag_grafieken_bijstorting_local <- function(kwalificatie, bedrag, big_mark_local, decimal_mark_local) {
      paste0(kwalificatie,
             " € ", formatC(as.numeric(round(bedrag/100, 0) * 100),
                            format="f", digits=0, big.mark = big_mark_local, decimal.mark = decimal_mark_local))
    }
    
    #portfolio waarde in alle scenarios
    scenariosplot <- reactive({
      # reactive values ophalen
      df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros()
      horizon <- horizon()
      profiel_conclusie <- profiel_conclusie()
      startvermogen <- startvermogen()
      bijstorting <- bijstorting()
      
      inflatie_vermogen <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
        filter(maand == horizon * 12, benchmark == profiel_conclusie) %>%
        select(inflatie) %>%
        unique() %>%
        pull()
      mean_vermogen <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
        filter(maand == horizon * 12, benchmark == profiel_conclusie) %>%
        select(mean) %>%
        unique() %>%
        pull()
      opti_vermogen <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
        filter(maand == horizon * 12, benchmark == profiel_conclusie) %>%
        select(q95) %>%
        unique() %>%
        pull()
      pessi_vermogen <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
        filter(maand == horizon * 12, benchmark == profiel_conclusie) %>%
        select(q05) %>%
        unique() %>%
        pull()
      max_vermogen <- df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
        filter(maand == horizon * 12, benchmark == profiel_conclusie) %>%
        select(max) %>%
        unique() %>%
        pull()
      #x = center of labels over vermogen
      if (horizon == 30) {x <- horizon * 10} else {x <- horizon * 12}
      #einde x schaal
      if (horizon == 30) {x_end <- horizon * 12}
      else if(horizon %in% c(1, 3) ) {x_end <- horizon * 12 * 2}
      else {x_end <- horizon * 12 * 1.5}
      #
      ggplot(data = df_bijstortingen_onttrekkingen_compleet_met_gem_start_ontt_euros %>%
               filter(maand <= min(horizon * 24, 360), benchmark == profiel_conclusie)) +
        geom_line(aes(group = id, y = index, x = maand, col = "#E59D00"), alpha = 0.15) +
        geom_line(aes(x = maand, y = inflatie), col = euroblue, linewidth = 1, linetype = 2, alpha = 0.7) +
        geom_line(aes(x = maand, y = q05), col = usred, linewidth = 1, alpha = 0.99) +
        geom_line(aes(x = maand, y = mean), col = ba_color, linewidth = 2, alpha = 0.85) +
        geom_line(aes(x = maand, y = q95), col = ba_color, linewidth = 1, alpha = 0.99) +
        scale_x_continuous(breaks =
                             if(horizon %in% c(1, 3, 5)) {c(0, 12, 24, 36, 60, 120, 240, 360)} else {c(0, 60, 120, 240, 360)},
                           labels = year(Sys.Date()) +
                             if(horizon %in% c(1, 3, 5)) {c(0, 12, 24, 36, 60, 120, 240, 360)/12} else {c(0, 60, 120, 240, 360)/12}, #12 maanden eruit bij langere hz
                           limits = c(0, x_end)) +
        scale_y_continuous(labels = scales::dollar_format(accuracy = 1, scale = 1, prefix = "€", suffix = "", big.mark = big_mark_local(), decimal.mark = decimal_mark_local()),
                           breaks = c(c(0, 0.5, 1, 1.5, 2, 3, 4, 5, 7.5, 10, 20, 50) * round(10^ceiling(log10(startvermogen/2))), startvermogen),
                           limits = c(0, opti_vermogen * 1.75)) +
        annotate(geom = "segment", x = 6, y = startvermogen, xend = horizon * 12, yend = startvermogen,
                 col = "gray", linetype = 2, linewidth = 1) +
        annotate(geom = "rect", xmin = horizon * 12, xmax = x_end , #min(horizon() * 24, 360),
                 ymin = 0,
                 ymax = opti_vermogen * 1.75,
                 fill = "white", alpha = 0.6) +
        geom_vline(xintercept = horizon * 12, col = "gray", linetype = 1, linewidth = 1) +
        annotate(geom = "label", x = x, y = inflatie_vermogen,
                 label = format_bedrag_grafieken_local(kwalificatie = str_c(round(100*inflatie, 0), "% ", vertaler$t("inflatie")), bedrag = inflatie_vermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()),
                 color = euroyellow, fill = euroblue, size = 5, alpha = .6) +
        annotate(geom = "label", x = x, y = mean_vermogen,
                 label = format_bedrag_grafieken_local(kwalificatie = vertaler$t("gematigd"), bedrag = mean_vermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()),
                 color = "gray20", fill = "white", size = 5, alpha = .9) +
        annotate(geom = "label", x = x, y = opti_vermogen,
                 label = format_bedrag_grafieken_local(kwalificatie = vertaler$t("gunstig"), bedrag = opti_vermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()),
                 color = ba_color, fill = "white", size = 5, alpha = .9) +
        annotate(geom = "label", x = x, y = pessi_vermogen,
                 label = format_bedrag_grafieken_local(kwalificatie = vertaler$t("ongunstig"), bedrag = pessi_vermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()),
                 color = usred, fill = "white", size = 5, alpha = .9) +
        #label in de grafiek mbt bm, startvermogen en bijstortingen
        annotate(geom = "label", x = 11 * horizon/2, y = opti_vermogen * 1.5,
                 label = if(bijstorting > 0) {
                   str_c(
                     vertaler$t("profiel"), " ", profiel_conclusie, "\n",
                     "horizon ", horizon, " ", vertaler$t("jaar"), "\n",
                     format_bedrag_grafieken_local(kwalificatie = vertaler$t("vermogen"), bedrag = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()), "\n",
                     format_bedrag_grafieken_bijstorting_local(kwalificatie = vertaler$t("jaarlijkse_bijstorting"), bedrag = bijstorting, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()))
                 }
                 else if(bijstorting < 0) {
                   str_c(
                     vertaler$t("profiel"), " ", profiel_conclusie, "\n",
                     "horizon ", horizon, " ", vertaler$t("jaar"), "\n",
                     format_bedrag_grafieken_local(kwalificatie = vertaler$t("vermogen"), bedrag = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()), "\n",
                     format_bedrag_grafieken_bijstorting_local(kwalificatie = vertaler$t("jaarlijkse_onttrekking"), bedrag = -bijstorting, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()))
                 }
                 else if(bijstorting == 0) {
                   str_c(
                     vertaler$t("profiel"), " ", profiel_conclusie, "\n",
                     "horizon ", horizon, " ", vertaler$t("jaar"), "\n",
                     format_bedrag_grafieken_local(kwalificatie = vertaler$t("vermogen"), bedrag = startvermogen, big_mark_local = big_mark_local(), decimal_mark_local = decimal_mark_local()))
                 },
                 colour = "gray20",fill = "white", alpha = .9, size = 6) +
        theme_bw() +
        theme(axis.text = element_text(size = 12)) +
        theme(legend.position = "none") +
        labs(x = " ", y = " ")
      #wat eruit komt
      #plot
    })
    # output plot voor gebruik elders (de scenarios-plot app)
    output$scenariosplot_module <- renderPlot({
      scenariosplot()
    })
    # output plot voor gebruik elders
    #(in dit geval in de input voor de pdf in de downloadhandler van de app, die het in de params doorgeeft naar de pdf)
    return(scenariosplot)
  })
}