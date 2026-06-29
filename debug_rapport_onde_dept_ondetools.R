## ---------------------------
##
## Script name:
##
## Purpose of script: Lancement des rapports automatisés à l'echelle des departements
##
## Author: Baptiste Roussel
##
## Date Created: 2025-06-27
## Date modification : 07/05/2026 : test si debug a faire
## ---------------------------
##
## Notes: Utilise les fonctions de ondetools
##
## ---------------------------
## Chargement des packages
# remotes::install_github("PascalIrz/ondetools")

# remotes::install_github("OFB-GE/ondetools")

remotes::install_github("OFB-GE/ondetools@test_graph_ecoulement_dpt_region")
#
# install.packages("C:\Users\julie.gueguen\Documents\3_Onde\debug_ondetools_rapportSD\ondetools", type = "source")
# devtools::install("C:\Users\julie.gueguen\Documents\3_Onde\debug_ondetools_rapportSD\ondetools")

require(tidyverse)
library(ondetools)

## 2) Choisir ses départements
mes_dpts <- as.character(c("08","10","51","52","54", "55","57", "67", "68", "88"))

mes_dpts <- as.character(c("52"))

## 2) Télécharger les données onde
onde_df <- telecharger_donnees_onde_api(dpt = mes_dpts)

table(onde_df$code_departement)

onde_df %>%
  filter(Annee == 2026 & month(date_observation) == 6) %>%
  group_by(code_departement) %>%
  summarise(nb_station = n())


produire_rapport_mensuel_dpt(
  onde_df = onde_df,
  code_departement = mes_dpts,
  annee_mois = "2026-06",
  region_dr = 'Grand Est', # attention orthographe très importante !
  complementaire = FALSE,
  dossier_sortie = "./output"
)

# erreur produire_rapport_mensuel_dpt à cause de produire carte statique lignes 156 - 163 -> ???


### dpt shape
dptFR_shp <-
  COGiter::departements_geo %>%
  dplyr::left_join(COGiter::departements %>%
                     dplyr::select(DEP, REG, NOM_DEP)) %>%
  sf::st_transform(crs = 2154)


### tableau pour le mois en cours selectionné
mois_sel <- format(lubridate::ym(annee_mois), "%m")
annee_sel <- format(lubridate::ym(annee_mois), "%Y")

onde_df_mois <-
  onde_df %>%
  dplyr::filter(code_departement == mes_dpts) %>%
  dplyr::mutate(Mois = format(as.Date(date_campagne), "%m")) %>%
  dplyr::filter(libelle_type_campagne == "usuelle") %>%
  dplyr::filter(Mois == mois_sel & Annee == annee_sel) %>%
  dplyr::filter(code_campagne == max(as.numeric(code_campagne)))


pays_front <- sf::read_sf(dsn = "https://gisco-services.ec.europa.eu/distribution/v2/countries/geojson/CNTR_RG_01M_2024_4326.geojson") %>%
  dplyr::filter(NAME_FREN %in% c("Italie", "Suisse", "Luxembourg", "Espagne", "Belgique", "Allemagne", "Andorre", "France"))

zaza <- produire_carte_statique(onde_df_mois = onde_df_mois,
                                    code_departement = mes_dpts,
                                    referentiel_onde = 'Typologie nationale',
                                    couleurs = onde_4mod,
                                    dptFR_shp = dptFR_shp)

# onde_14 <- telecharger_donnees_onde_api(dpt = c('02'))

# onde_14_recurrence <- calculer_recurrence_assecs(onde_df = onde_14)

# produire_graph_reccurrence_assecs(df_assecs = onde_14_recurrence)

## End(Not run)

# devtools::install_github("richaben/ondetools")

# Quitting from skeleton.Rmd:288-340 [historique]
# erreur le 22/06/26 - sans aucuns changements par rapport au lancement d'avant... (rapport mai 2026...)
# pas de pb sur part1, ni sur part B, ni sur part C (aie aie), ni sur plot_historique !!

onde_dpt <-
  onde_df %>%
  dplyr::filter(code_departement == mes_dpts) %>%
  dplyr::filter(etat_station == 'Active') %>%
  dplyr::mutate(
    Annee = as.numeric(Annee),
    Mois = format(as.Date(date_campagne), "%m"),
    Mois_campagne = lubridate::ym(paste0(Annee,Mois,sep="-"))
  ) %>%
  dplyr::mutate(
    lib_ecoul_dpt = dplyr::case_when(
      libelle_ecoulement == 'Ecoulement visible' ~ 'Ecoulement visible acceptable',
      TRUE ~ libelle_ecoulement
    )
  )

onde_dpt_propluvia <-
  ajouter_zones_propluvia(onde_df = onde_dpt,
                          propluvia_shape = propluvia_zone)

libelle_propluvia_dpt <- unique(onde_dpt_propluvia$libel)

for (j in 1:length(libelle_propluvia_dpt)){
  # section par zones d'alerte
  cat("### Zones d'alerte: ", libelle_propluvia_dpt[j], "\n")

  zones_df <-
    onde_dpt_propluvia %>%
    dplyr::filter(libel == libelle_propluvia_dpt[j])

  graphs_stations <-
    purrr::map(
      .x = unique(zones_df$code_station),
      .f = produire_graph_pour_une_station_v2,
      type_mod = lib_ecoul_dpt,
      onde_df = zones_df,
      mod_levels = c("Assec",
                     "Ecoulement\nnon visible",
                     "Ecoulement\nvisible\nfaible",
                     "Ecoulement\nvisible\nacceptable",
                     "Observation\nimpossible",
                     "Donnée\nmanquante"),
      mod_colors = onde_5mod
    ) %>%
    purrr::set_names(unique(zones_df$code_station))

  walk(.x = graphs_stations, ~ print(.x))

  # saut de page
  cat('\n')
  cat('\n')
}


######## construction preoduire barplot region

mes_dpts <- c("57", "67")

## 2) Télécharger les données onde
onde_df <- telecharger_donnees_onde_api(dpt = mes_dpts)
# c'est dommage de devoir telecharger toutes les dates
# il ne bloque pas l'annee pour avoir la possibilite de faire l'historique.

annee_mois = "2026-05"
region_dr = 'Grand Est'
complementaire = FALSE

