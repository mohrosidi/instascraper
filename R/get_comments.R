#' Return media and media metadata
#'
#' \code{get_media} return media and media metadata based on search configuration.
#'
#' @param username  String vector. Specify a username account
#' @param tag String. Scrapes the specified hashtag for media.
#' @param location String. Scrapes the specified instagram location-id for media.
#' @param following_input Logical value.  Use profiles followed by login-user as input
#' @param your_username  String. Instagram login user
#' @param your_password String. Instagram login password
#' @param max Maximum number of items to scrape.
#' @param include_location Logical value. Includes location metadata when saving media metadata.
#' @param interactive Enables interactive login challenge solving. Has 2 modes: SMS and Email
#' @param filter String vector.  Scrapes the specified hashtag within a user's media.
#' @param filter_location String vector.Filter scrape queries by command line location(s) ids
#' @param raw_data Logical value. if TRUE, clean metadata.
#' @param retry_download Logical values. Retry download attempts endlessly when errors are received
#'
#' @importFrom purrr pluck map_lgl map_dfr map
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble
#' @importFrom dplyr  mutate select rename filter %>%
#' @importFrom janitor clean_names
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect
#' @importFrom tidyr unnest unnest_wider
#' @return A tibble and profile metadata
#'
#' @examples
#' \dontrun{
#' get_media(following_input = TRUE, your_username = "xxxx", your_password = "123")
#' get_media(username = "ABC", max = 10, media = "image")
#' }
#'
#' @export
get_comments <- function(username = NULL,
                         tag = NULL,
                         location = NULL,
                         your_username = NULL,
                         your_password = NULL,
                         following_input = FALSE,
                         max = NULL,
                         include_location = FALSE,
                         interactive = NULL,
                         filter = NULL,
                         filter_location = NULL,
                         raw_data = TRUE,
                         retry_download = FALSE){

  id <- NULL
  owner_id <- NULL
  edge_media_to_comment_data <- NULL
  comments <- NULL
  created_at <- NULL
  owner <- NULL
  text <- NULL
  id...4 <- NULL
  id...5 <- NULL
  username...2 <- NULL
  username...8 <- NULL
  username...6 <- NULL
  . <- NULL
  comments_data <- NULL
  id...6 <- NULL

  # Check parameters ----
  if(all(map_lgl(list(username,tag, location, your_username, your_password, max, include_location, interactive, filter, filter_location), is.null))){
    stop(paste("Please specify at least a username, tag or location"))
  }

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
      stop(paste("location must be numeric"))
  }

  ## Check filter_location
  if(include_location & !is.null(filter_location)){
    stop(paste("Please specify include location"))
  }

  ## Check max ----
  if(!is.null(max)){
    if(max <= 0 | !is.numeric(max) | length(max)>1)
      stop(paste("max must be greater than 0 and a numeric value"))
  }

  ## Check username and password
  if((!is.null(your_username) & is.null(your_password)) | (!is.null(your_password) & is.null(your_username)))
    stop(paste("You must specify both your username and password"))

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

  query <- list(ifelse(is.null(username), NA, username2),
                ifelse(is.null(tag), NA, paste("--tag", tag)),
                ifelse(is.null(location), NA, paste("--location", location)),
                ifelse(following_input, paste("--followings-input"), NA),
                ifelse(is.null(your_username), NA, paste("-u", your_username)),
                ifelse(is.null(your_password), NA, paste("-p", your_password)),
                ifelse(is.null(max), NA, paste("-m", max)),
                ifelse(include_location, paste("--include-location"), NA),
                ifelse(interactive, paste("--interactive", interactive), NA),
                ifelse(is.null(filter), NA, paste("--filter_location", filter2)),
                ifelse(is.null(filter_location), NA, paste("--filter_location", filter_location2)),
                ifelse(retry_download,paste("--retry-forever"), NA))

  query <- unlist(query) %>% .[!is.na(.)] %>% paste(., collapse = " ")


  cmd <- paste("instagram-scraper", query, "--media-types none --comments")



  ## Download the data
  system(cmd)

  # Parse the data ----
  if(!is.null(username)){

    df <- map_dfr(username, function(x){

      fromJSON(paste0(x, "/", x, ".json")) %>%
        pluck(1) %>%
        jsonlite::flatten()  %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        select(post_id = id,
               username,
               owner_id,
               comments = comments_data) %>%
        filter(purrr::map_lgl(comments, ~!rlang::is_empty(.x))) %>%
        unnest_wider(comments) %>%
        unnest(c(created_at, id, owner, text), names_repair = "unique") %>%
        rename(username = username...2,
               comment_username = username...8,
               comment_id =  id...5,
               comment_username_id = id...6) %>%
        mutate(created_at = as.POSIXct(created_at, origin = "2020-01-30"))

    })

  }else if(!is.null(tag)){

    df <- fromJSON(paste0(tag, "/", tag, ".json")) %>%
      pluck(1) %>%
      jsonlite::flatten()  %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      select(post_id = id,
             owner_id,
             comments = edge_media_to_comment_data) %>%
      filter(purrr::map_lgl(comments, ~!rlang::is_empty(.x))) %>%
      unnest_wider(comments) %>%
      unnest(c(created_at, id, owner, text), names_repair = "unique") %>%
      rename(comment_username = username,
             comment_id =  id...4,
             comment_username_id = id...5) %>%
      mutate(created_at = as.POSIXct(created_at, origin = "2020-01-30"))


  }else if(!is.null(location)){

    df <- fromJSON(paste0(location, "/", location, ".json")) %>%
      pluck(1) %>%
      jsonlite::flatten()  %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      select(post_id = id,
             owner_id,
             comments = edge_media_to_comment_data)  %>%
      filter(purrr::map_lgl(comments, ~!rlang::is_empty(.x))) %>%
      unnest_wider(comments) %>%
      unnest(c(created_at, id, owner, text), names_repair = "unique") %>%
      rename(comment_username = username,
             comment_id =  id...4,
             comment_username_id = id...5) %>%
      mutate(created_at = as.POSIXct(created_at, origin = "2020-01-30"))


  }else if(following_input){

    ls <- fs::dir_ls(all = TRUE, recurse = TRUE, type = "file")
    ls <- ls[stringr::str_detect(ls, pattern = ".json")]

    df <- map_dfr(ls, function(x){
      fromJSON(paste0(x)) %>%
        pluck(1) %>%
        jsonlite::flatten()  %>%
        as_tibble() %>%
        janitor::clean_names() %>%
        select(post_id = id,
               username,
               owner_id,
               comments = comments_data) %>%
        filter(purrr::map_lgl(comments, ~!rlang::is_empty(.x))) %>%
        unnest_wider(comments) %>%
        unnest(c(created_at, id, owner, text), names_repair = "unique") %>%
        rename(username = username...2,
               comment_username = username...8,
               comment_id =  id...5,
               comment_username_id = id...6) %>%
        mutate(created_at = as.POSIXct(created_at, origin = "2020-01-30"))
    })


  }else{
    NULL
  }

  # Clean raw data ----

  if(!raw_data){
    if(!is.null(username)){

      map(username, function(x){
        unlink(x, recursive = TRUE)
      })

    }else if(!is.null(tag)){

      map(tag, function(x){
        unlink(x, recursive = TRUE)
      })

    }else if(!is.null(location)){

      map(location, function(x){
        unlink(x, recursive = TRUE)
      })

    }else if(following_input){

      ls <- fs::dir_ls(all = TRUE, recurse = TRUE, type = "file")
      ls <- ls[stringr::str_detect(ls, pattern = ".json")]

      map(ls, function(x){
        unlink(x, recursive = TRUE)
      })

    }else{
      NULL
    }


  }

  return(df)

}

