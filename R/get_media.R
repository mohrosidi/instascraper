#' Return media and media metadata
#'
#' \code{get_media} return media and media metadata based on search configuration.
#'
#' @param username  String vector. Specify a username account
#' @param tag String. Scrapes the specified hashtag for media.
#' @param location String. Scrapes the specified instagram location-id for media.
#' @param following_input Logical value.  Use profiles followed by login-user as input
#' @param media_type String vector. Specify media types to scrape. Valid values are image, video, story (story-image & story-video), broadcast or none. Stories require a your_username and your_password to be defined.
#' @param your_username  String. Instagram login user
#' @param your_password String. Instagram login password
#' @param max Maximum number of items to scrape.
#' @param include_location Logical value. Includes location metadata when saving media metadata.
#' @param interactive Enables interactive login challenge solving. Has 2 modes: SMS and Email
#' @param filter String vector.  Scrapes the specified hashtag within a user's media.
#' @param filter_location String vector.Filter scrape queries by command line location(s) ids
#' @param retry_download Logical values. Retry download attempts endlessly when errors are received
#'
#' @importFrom purrr pluck map_lgl map_dfr map
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr  mutate select rename %>%
#' @importFrom janitor clean_names
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect
#' @importFrom tidyr unnest
#' @return A tibble and profile metadata
#'
#' @examples
#' \dontrun{
#' get_media(following_input = TRUE, your_username = "xxxx", your_password = "123")
#' get_media(username = "ABC", max = 10, media = "image")
#' }
#'
#' @export
get_media <- function(username = NULL,
                      tag = NULL,
                      location = NULL,
                      following_input = FALSE,
                      media_type = "none",
                      your_username = NULL,
                      your_password = NULL,
                      max = NULL,
                      include_location = FALSE,
                      interactive = NULL,
                      filter = NULL,
                      filter_location = NULL,
                      retry_download = FALSE){

  id <- NULL
  text <- NULL
  dimensions <- NULL
  edge_media_to_comment <- NULL
  edge_media_preview_like <- NULL
  edge_media_to_caption <- NULL
  taken_at_timestamp <- NULL
  . <- NULL

  # Check all arguments ----
  if(all(map_lgl(list(username,tag, location, your_username, your_password, max, include_location, interactive, filter, filter_location), is.null)))
    stop(paste("Please specify at least a username, tag or location"))

  ## Check username ----
  if(!is.null(username)){
    if(!is.character(username))
      stop(paste("username must be character"))
  }

  ## Check tag ----
  if(!is.null(tag)){
    if(!is.character(tag))
      stop(paste("tag must be character"))
  }

  ## Check location ----
  if(!is.null(location)){
    if(!is.numeric(location))
      stop(paste("location must be character"))
  }

  ## Check max ----
  if(!is.null(max)){
    if(max <= 0 | !is.numeric(max) | length(max)>1)
      stop(paste("max must be greater than 0 and a numeric value"))
  }

  ## Check filter_location
  if(include_location & !is.null(filter_location)){
    stop(paste("Please specify include_location"))
  }

  ## Check media type
  if(c("story") %in% media_type & (is.null(your_username) | is.null(your_password)))
    stop(paste("Please specify your_username and your_password"))

  ## Check your_username and your_password
  if((!is.null(your_username) & is.null(your_password)) | (!is.null(your_password) & is.null(your_username)))
    stop(paste("You must specify both your username and password"))

  ## Check following_input
  if(following_input & (is.null(your_username)|is.null(your_password)))
    stop(paste("Please specify your_username and your_password"))

  # Download the data ----
  ## Command configuration
  if(!is.null(username)){
    username2 <- ifelse(username > 1,
                        paste(username, collapse = ","),
                        username)[[1]]
  }

  if(!is.null(filter)){
    filter2 <- ifelse(filter > 1,
                               paste(filter, collapse = ","),
                               filter)[[1]]
  }

  if(!is.null(filter_location)){
    filter_location2 <- ifelse(filter_location > 1,
                        paste(filter_location, collapse = ","),
                        filter_location)[[1]]
  }

  media_type2 <- ifelse(media_type > 1,
                   paste(media_type, collapse = ","),
                   media_type)[[1]]

  query <- list(ifelse(is.null(username), NA, username2),
                ifelse(is.null(tag), NA, paste("--tag", tag)),
                ifelse(is.null(location), NA, paste("--location", location)),
                ifelse(is.null(your_username), NA, paste("-u", your_username)),
                ifelse(is.null(your_password), NA, paste("-p", your_password)),
                ifelse(following_input, paste("--followings-input"), NA),
                ifelse(is.null(max), NA, paste("-m", max)),
                paste("--media_types", media_type2),
                ifelse(include_location, paste("--include-location"), NA),
                ifelse(interactive, paste("--interactive", interactive), NA),
                ifelse(is.null(filter), NA, paste("--filter_location", filter2)),
                ifelse(is.null(filter_location), NA, paste("--filter_location", filter_location2)),
                ifelse(retry_download,paste("--retry-forever"), NA))

  query <- unlist(query) %>% .[!is.na(.)] %>% paste(., collapse = " ")


  cmd <- paste("instagram-scraper", query, "--media-metadata")


  ## Download the data
  system(cmd)

  # Parse the data ----
  if(!is.null(username)){

    df <- map_dfr(username, function(x){

      jsonlite::fromJSON(paste0(x, "/", x, ".json")) %>%
        pluck(1) %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        mutate(height = pluck(dimensions,1),
               width = pluck(dimensions, 2)) %>%
        select( -dimensions) %>%
        mutate(edge_media_to_comment = pluck(edge_media_to_comment, 1),
               edge_media_preview_like = pluck(edge_media_preview_like,1),
               edge_media_to_caption = pluck(edge_media_to_caption,1,1),
               taken_at_timestamp = as.POSIXct(taken_at_timestamp, origin = "2020-01-30")) %>%
        mutate(edge_media_to_caption = map(edge_media_to_caption, unique)) %>%
        unnest(edge_media_to_caption) %>%
        rename(n_like = edge_media_preview_like,
               post_id = id,
               n_comment = edge_media_to_comment,
               caption = text)

    })


  }else if(following_input){

    ls <- fs::dir_ls(all = TRUE, recurse = TRUE, type = "file")
    ls <- ls[stringr::str_detect(ls, pattern = ".json")]

    df <- map_dfr(ls, function(x){

      jsonlite::fromJSON(x) %>%
        pluck(1) %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        mutate(height = pluck(dimensions,1),
               width = pluck(dimensions, 2)) %>%
        select( -dimensions) %>%
        mutate(edge_media_to_comment = pluck(edge_media_to_comment, 1),
               edge_media_preview_like = pluck(edge_media_preview_like,1),
               edge_media_to_caption = pluck(edge_media_to_caption,1,1),
               taken_at_timestamp = as.POSIXct(taken_at_timestamp, origin = "2020-01-30")) %>%
        mutate(edge_media_to_caption = map(edge_media_to_caption, unique)) %>%
        unnest(edge_media_to_caption) %>%
        rename(n_like = edge_media_preview_like,
               post_id = id,
               n_comment = edge_media_to_comment,
               caption = text)

    })

  }else if(!is.null(tag)){

    df <- jsonlite::fromJSON(paste0(tag, "/", tag, ".json")) %>%
        pluck(1) %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        mutate(height = pluck(dimensions,1),
               width = pluck(dimensions, 2)) %>%
        select( -dimensions) %>%
        mutate(edge_media_to_comment = pluck(edge_media_to_comment, 1),
               edge_media_preview_like = pluck(edge_media_preview_like,1),
               edge_media_to_caption = pluck(edge_media_to_caption,1,1),
               taken_at_timestamp = as.POSIXct(taken_at_timestamp, origin = "2020-01-30")) %>%
        mutate(edge_media_to_caption = map(edge_media_to_caption, unique)) %>%
        unnest(edge_media_to_caption) %>%
        rename(n_like = edge_media_preview_like,
               post_id = id,
               n_comment = edge_media_to_comment,
               caption = text)

  }else if(!is.null(location)){

    df <- jsonlite::fromJSON(paste0(location, "/", location, ".json")) %>%
      pluck(1) %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      mutate(height = pluck(dimensions,1),
             width = pluck(dimensions, 2)) %>%
      select( -dimensions) %>%
      mutate(edge_media_to_comment = pluck(edge_media_to_comment, 1),
             edge_media_preview_like = pluck(edge_media_preview_like,1),
             edge_media_to_caption = pluck(edge_media_to_caption,1,1),
             taken_at_timestamp = as.POSIXct(taken_at_timestamp, origin = "2020-01-30")) %>%
      mutate(edge_media_to_caption = map(edge_media_to_caption, unique)) %>%
      unnest(edge_media_to_caption) %>%
      rename(n_like = edge_media_preview_like,
             post_id = id,
             n_comment = edge_media_to_comment,
             caption = text)


  }else{
    NULL
  }

  return(df)
}
