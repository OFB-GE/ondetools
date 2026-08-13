#' Télécharger les données ONDE pour un ou plusieurs départements et selection
#' des dates min et max
#'
#' Cette fonction permet de télécharger les données ONDE via l'API Ecoulement sur la plateforme
#'  HubEau (\url{https://hubeau.eaufrance.fr/page/api-ecoulement})
#'  pour un ou plusieurs départements.
#'
#' @seealso \url{https://hubeau.eaufrance.fr/page/api-ecoulement}
#'
#' @param dpt un vecteur comprenant le code insee du(des) département(s). ex: \code{c('27')}
#' @param date_min chaine de character avec la date minimale à prendre en compte pour les données ex : \code{"2012-01-01"}
#' par défault la valeur est le "2012-01-01"
#' @param date_max chaine de character avec la date minimale à prendre en compte pour les données ex : \code{"2024-09-06"}
#' par défault la valeur est la date du jour.
#'
#' @return un dataframe avec les données campagnes, observations et stations.
#' @export
#'
#' @importFrom dplyr mutate distinct left_join if_else arrange
#' @importFrom hubeau get_ecoulement_campagnes list_params get_ecoulement_stations get_ecoulement_observations
#' @importFrom lubridate as_date year
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#'
#' ## pour un département
#'
#' telecharger_donnees_onde_api_dates(dpt= c('14'), date_min = "2025-07-28")
#'
#' ## pour plusieurs départements
#'
#' telecharger_donnees_onde_api_dates(dpt= c('02', '60', '80'), date_max = "2025-07-28")
#' }

telecharger_donnees_onde_api_dates <- function(dpt = NULL, date_min = "2012-01-01",
                                               date_max = as.character(format(Sys.time(),"%Y-%m-%d"))){

  #### config dpt + date
  conf_dep <- dpt
  # date_jour <- as.character(format(Sys.time(),"%Y-%m-%d"))

  #### infos campagnes
  campagnes <- purrr::map_df(.x = conf_dep,
                             function(x) hubeau::get_ecoulement_campagnes(
                               list(
                                 code_departement = x,
                                 date_campagne_min = date_min,
                                 date_campagne_max = date_max
                               )
                             )) %>%
    dplyr::mutate(code_campagne = as.character(code_campagne)) %>%
    dplyr::distinct()

  #### infos stations
  param_stations <-
    hubeau::list_params(api = "ecoulement", endpoint = "stations") %>%
    toString() %>%
    gsub(pattern = " ",replacement = "") %>%
    paste0(",etat_station")

  stations <- purrr::map_df(.x = conf_dep,
                            function(x) hubeau::get_ecoulement_stations(
                              list(code_departement = x,
                                   fields = param_stations)
                            )) %>%
    dplyr::distinct()

  #### infos observations
  param_obs <-
    hubeau::list_params(api = "ecoulement", endpoint = "observations") %>%
    toString() %>%
    gsub(pattern = " ",replacement = "") %>%
    paste0(",date_observation")


  observations <- purrr::map_df(.x = conf_dep,
                                function(x) hubeau::get_ecoulement_observations(
                                  list(code_departement = x,
                                       date_observation_min = date_min,
                                       date_observation_max = date_max,
                                       fields = param_obs)
                                )) %>%
    dplyr::mutate(code_campagne = as.character(code_campagne)) %>%
    dplyr::distinct()

  #### Assemblage des données stations, observations, campagnes ----
  onde_df <- observations %>%
    dplyr::left_join(campagnes) %>%
    dplyr::left_join(stations) %>%
    dplyr::mutate(
      date_campagne = lubridate::as_date(date_campagne, format = "%Y-%m-%d")
    ) %>%
    dplyr::mutate(
      Annee = lubridate::year(date_campagne),
      libelle_ecoulement = dplyr::if_else(
        condition = is.na(libelle_ecoulement),
        true = "Donn\u00e9e manquante",
        false = libelle_ecoulement
      )
    ) %>%
    dplyr::arrange(code_station,
                   code_departement,
                   dplyr::desc(Annee))

  return(onde_df)
}


