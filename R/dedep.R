
#' Get Approximate Duplicate IDs
#'
#' @param d data
#' @param id_cols id_cols
#' @param uidName  uid name
#' @param customIdName custom name
#' @param max_dist max dist
#' @param asList as list
#'
#' @return
#' @export
#' @importFrom dplyr left_join select filter
get_approx_dup_ids <- function(d, id_cols, uidName = NULL, customIdName = NULL,
                               max_dist = 0.1, asList = FALSE){
  d1 <- create_idcols(d, id_cols)
  if(max_dist == 0){
    dd <- dplyr::left_join(d1,d1 %>% dplyr::select(.custom_id))


  }
  dist <- str_dist(d1$.custom_id)
  dist_replace <- dist %>%
    dplyr::filter(distance <= max_dist) %>% dplyr::select(from = item1, to = item2)
  idx <- match_replace(d1$.row_id, dic = dist_replace, force = FALSE)
  d1$.new_id <- d1$.custom_id[idx]
  if(!is.null(customIdName))
    names(d1)[2] <- uidName
  if(!is.null(uidName))
    names(d1)[3] <- uidName
  d1
}

#' exclusive_ids
#' Find if two ids are exclusive
#' @name exclusive_ids
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' @importFrom dplyr group_by_ mutate n rename_ arrange left_join
#' @importFrom purrrlyr slice_rows by_slice
exclusive_ids <- function(d, ids, .row_id = NULL, keepCols = TRUE){
  d <- add_row_id(d, id = .row_id)
  g <- d %>%
    purrrlyr::slice_rows(ids) %>%
    purrrlyr::by_slice(function(x) x$.row_id, .to = ".row_id")
  g <- g %>%
    dplyr::group_by_(.dots = ids[1]) %>%
    dplyr::mutate(.id_1 = ifelse(dplyr::n()>1,FALSE,TRUE)) %>%
    dplyr::group_by_(.dots = ids[2]) %>%
    dplyr::mutate(.id_2 = ifelse(dplyr::n()>1,FALSE,TRUE)) %>%
    dplyr::mutate(.exc_id = .id_1 & .id_2)
  g <- g %>%
    dplyr::rename_(.dots = setNames(c(".id_1",".id_2"),
                             paste0(".exc_",ids)))
  g <- unnest(g) %>% dplyr::arrange(.row_id)
  if(keepCols)
    g <- dplyr::left_join(d,g)
  g <- g %>%
    move_first(c(".row_id",names(g)[grep("^.exc_",names(g))]))
  if(!is.null(.row_id))
    g$.row_id <- NULL
  g
}


#' add_unique_id
#' Find possible duplicates
#' @name add_unique_id
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' @importFrom dplyr distinct right_join
add_unique_id <- function(d, col, uidName = NULL, uidPrefix = NULL){
  uidName <- uidName %||% ".unique_id"
  dic <- d[col] %>% dplyr::distinct()
  dic[uidName] <- 1:nrow(dic)
  if(!is.null(uidPrefix))
    dic[uidName] <- paste0(uidPrefix,dic[uidName])
  x <- dplyr::right_join(d,dic, by = col)
  move_first(x,uidName)
}


#' create_idcols
#' Find possible duplicates
#' @name create_idcols
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' @importFrom dplyr bind_cols select rename_
create_idcols <- function(d, id_cols = NULL, idName = NULL,
                          keepCols = FALSE, noAccents = TRUE, lowerCase = TRUE){
  if(!all(id_cols %in% names(d)))
    stop("All id_cols must be in d")

  d1 <- d[id_cols]
  d1[is.na(d1)] <- ""
  if(noAccents)
    d1 <- map_df(d1,remove_accents)
  if(lowerCase)
    d1 <- map_df(d1,tolower)
  d1 <- unite_(d1,".custom_id",id_cols)
  d <- add_row_id(d)
  d2 <- dplyr::bind_cols(d[".row_id"],d1)
  if(keepCols)
    d2 <- dplyr::bind_cols(d2,d %>% dplyr::select(-.row_id))
  if(!is.null(idName))
    d2 <- d2 %>% dplyr::rename_(.dots = setNames(".custom_id", idName))
  d2
}



