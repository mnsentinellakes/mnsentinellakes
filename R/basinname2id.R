#' Basin Names to Basin Ids
#'
#' This function creates a data frame with the Basin Ids for all lakes with the input name, as well as the option to indicate
#' which Counties they are located in. If multiple Basins have the  same name, this will return a list of Basin Ids.
#' @param basinnames one or more basin names
#' @param mainbasin is the entered basinname the name of the main basin or a sub-basin. If the main basin name does not return
#' a result, set mainbasin to FALSE
#' @param counties one or more county names where the basins are located. Default is NULL.
#' @keywords basinid basinname dowlknum
#' @return a data.frame
#' @family Sentinel Lakes Tools
#' @importFrom dplyr %>%
#' @examples
#' basinname2id("Echo")
#'
#' @export

basinname2id = function(basinnames, mainbasin = TRUE, counties = NULL){

  #Select the column to extract the names
  namecol = if (mainbasin) "parent_basin_name" else "basin_name"

  #Select ids
  basinids = mnsentinellakes::basin_ids_names %>%
    dplyr::filter(.data[[namecol]] %in% basinnames)

  #Filter by county if not null
  if (!is.null(counties)){
    basinids = basinids %>%
      dplyr::filter(.data$county %in% counties)
  }

  #For joiining back in input order
  basinids = basinids %>%
    dplyr::mutate(basin_name = .data[[namecol]]) %>%
    dplyr::select(.data$basin_name,.data$basin_id)

  #Join to preserve order and ensure all names appear
  basinids_joined = dplyr::left_join(
    tibble::tibble(basin_name = basinnames),
    basinids,
    by = "basin_name"
  )

  #Warnings
  #List names with no matches
  no_match = basinids_joined %>%
    dplyr::filter(is.na(.data$basin_id)) %>%
    dplyr::pull(.data$basin_name) %>%
    unique()

  if (length(no_match) > 0){
    warning(
      "No Basin Ids found for: ",
      paste(no_match, collapse = ", "),
      if (mainbasin) ". Try setting mainbasin = FALSE"
    )
  }

  #Check for Names with multiple matches
  multi_match = basinids_joined %>%
    dplyr::count(.data$basin_name) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::pull(.data$basin_name)

  if (length(multi_match) > 0){
    warning(
      "Multiple Basin Ids found for: ",
      paste(multi_match,collapse = ", "),
      ". Use the counties aregument to narrow results"
    )
  }

  return(basinids_joined$basin_id)

}
