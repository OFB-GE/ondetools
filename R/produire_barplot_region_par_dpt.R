## ---------------------------
##
## Script name : produire_barplot_region_par_dpt.R
##
## Purpose of script : Creer un barplot pour tous les departements de la region indiquée dans
## la variable "region_dr" afin de permettre une comparaison des situations entre les départements.
##
## Author : Julie Guéguen
##
## Date de création : 2026-06-24
## Date de dernière modification :
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------


#' Title
#'
#' @param mois_sel : Chaine de caracteres avec le numero du mois selectionné ex : "05"
#' @param annee_sel : Chaine de caracteres avec l'année selectionnée
#' @param region_dr : Chaine de caractere avec le nom de la région souhaité (attention à l’orthographe de la région)
#' @param referentiel_onde : Chaiende caracteres avec pour savoir s'il faut utiliser la
#' typologie nationale ou départementale. ('Typologie nationale' ou 'Typologie departementale')
#' @param complementaire : Booleen s'il s'agit d'une campagne complémentaire ou pas.
#'
#' @returns
#' @export
#'
#' @examples produire_barplot_region_par_dpt(mois_sel = "08", annee_sel = "2026", region_dr = 'Grand Est')
produire_barplot_region_par_dpt <- function(mois_sel,
                                            annee_sel,
                                            region_dr,
                                            referentiel_onde = 'Typologie nationale',
                                            complementaire = FALSE){


  if(complementaire == TRUE) {type_rapport <- "compl\u00e9mentaire"} else {type_rapport <- "usuelle"}

  if(referentiel_onde == 'Typologie nationale') {
    mod <- c("Ecoulement visible",
             "Ecoulement non visible",
             "Assec",
             "Observation impossible",
             "Donnée manquante")
  } else {
    mod <- c("Ecoulement visible acceptable",
             "Ecoulement visible faible",
             "Ecoulement non visible",
             "Assec",
             "Observation impossible",
             "Donnée manquante")
    }

  # recuperer les departements de la region
  dptRegion <- COGiter::regions %>%
            filter(NOM_REG == region_dr) %>%
            select(REG, NOM_REG) %>%
            left_join(COGiter::departements, by = join_by(REG))

  # recuperer les données concernees (departement, annee et mois)
  onde_df_R <- dptRegion$DEP %>%
                map(\(x) telecharger_donnees_onde_api_dates(dpt = x,
                                                            date_min = paste(annee_sel, mois_sel, "01",sep = "-"),
                                                            date_max =  paste(annee_sel, mois_sel, "31",sep = "-"))) %>%
                bind_rows()

  # mois_sel = "05"

  onde_df_RDMA <- onde_df_R %>%
    dplyr::mutate(Mois = format(as.Date(date_campagne), "%m")) %>%
    dplyr::filter(libelle_type_campagne == type_rapport) #%>%
    # dplyr::filter(Mois == mois_sel & Annee == annee_sel)


  # calculer les nombres de stations par modalite et par departement
  data_bilan_ecoulement <- onde_df_RDMA %>%
    calculer_bilan_ecoulement(onde_df = .,
                            mod = lib_ecoul,
                            mod_levels = mod,
                            referentiel_onde = referentiel_onde,
                            force_complementaire = complementaire) # %>%
                            #mutate(code_departement = as.factor(code_departement))

  # 08 -> 05_usue; 10 -> 05_usue2,... 88 -> 05_usue10
  # creer le barplot
  # produire_graph_type_ecoulement(data_bilan = data_bilan_ecoulement,
  #                                lib_ecoulement = lib_ecoul,
  #                                historique = F) +
  #   ggplot2::labs(title = glue::glue("Types d\'\u00e9coulements par d\u00e9partement au {glue::glue_collapse(c(mois_sel, annee_sel), sep = ' / ')} \n R\u00e9gion {region_dr}")) +
  #   # attention ici !!!
  #   # la variable en factor Mois_c nous met dans la merde pour le tri
  #   # scale_x_discrete(limits = as.character(Mois_c)) +
  #   facet_grid(
  #     labeller = labeller(
  #       yfacet = "another x label"
  #     )
  #   )

    # script copié de la fonction produire_graph_type_ecoulement
  # le 24/06/2026 version 0.1.2

  graph_barplot <- data_bilan_ecoulement %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = frq,
        x = forcats::fct_rev((code_departement)),
        fill = forcats::fct_rev(lib_ecoul),
        label = Label_p
      )
    ) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity",
      alpha = 0.7,
      colour = 'black',
      width = 0.7,
      linewidth = 0.01
    ) +
    # ggplot2::facet_grid(~Annee ) +
    ggrepel::geom_text_repel(
      size = 3,
      color = "black",
      fontface = 'bold.italic',
      hjust = 1,
      position = ggplot2::position_stack(vjust = 0.5),

    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Pourcentage (%)") +
    ggplot2::xlab(NULL) +
    ggplot2::labs(title = glue::glue("Types d\'\u00e9coulements par d\u00e9partement - Campagne {unique(onde_df_RDMA$libelle_type_campagne)} {unique(lubridate::month(onde_df_RDMA$date_campagne,label = T))} {unique(lubridate::year(onde_df_RDMA$date_campagne))} \n R\u00e9gion {region_dr}"),
                  subtitle = glue::glue('{unique(data_bilan_ecoulement$Typologie)}'),
                  x = "D\u00e9partements",
                  caption = paste("Source: ONDE (OFB)\n \u00a9OFB", format(Sys.time(), '%Y'), "- Date d\'\u00e9dition:", format(Sys.time(), '%d/%m/%Y'))) +
    ggplot2::scale_fill_manual(
      name = "Situation stations",
      values = c("Ecoulement visible" = "#4575b4",
                 "Ecoulement visible acceptable" = "#4575b4",
                 "Ecoulement visible faible" = "#bdd7e7",
                 "Assec" = "#d73027",
                 "Ecoulement non visible" = "#fe9929",
                 "Observation impossible" = "grey50",
                 "Donn\\u00e9e manquante" = 'grey')
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(size = 9, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9, face = 'italic'),
      legend.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9, face = 'bold'),
      axis.text.y = ggplot2::element_text(size = 9, colour = 'black'),
      axis.text.x = ggplot2::element_text(size = 9, colour = 'black'),
      strip.text.x = ggplot2::element_text(size = 9, color = "black", face = "bold"),
      strip.background = ggplot2::element_rect(
        color="black", fill="grey80", linewidth = 1, linetype="solid"
      ),
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_line(colour = NA),
      legend.position = "bottom",
      plot.background = ggplot2::element_blank(),
    )+
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
    )

  return(graph_barplot)

}
