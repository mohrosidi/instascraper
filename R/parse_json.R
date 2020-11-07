#' Parse a json file
#'
#' \code{parse_json} parse a json file based on specific configuration.
#'
#' @param path  String that specify file location
#' @param type  String. Specify the parsing type. Possible values: comments (a json file comes from \code{get_comments}), media (a json file comes from \code{get_media}), or profiles (a json file comes from \code{get_profiles}).
#' @param input_argument String. Specify the first argument you use in function \code{get_comments}, \code{get_media}, and \code{get_profiles}. Possible values: username, tag, location, or none.
#'
#' @importFrom purrr pluck map_lgl map
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter mutate rename %>%
#' @importFrom tidyr  unnest_wider unnest
#' @importFrom rlang is_empty
#' @importFrom janitor clean_names
#' @return A tibble
#'
#' @examples
#' \dontrun{
#' parse_json("x/x.json", type = "profiles", input_argument = "none")
#' }
#'
#' @export
parse_json <- function(path = NULL,
                       type = NULL,
                       input_argument = NULL){

  id <- NULL
  username <- NULL
  owner_id <- NULL
  comments_data <- NULL
  comments <- NULL
  created_at <- NULL
  owner <- NULL
  text <- NULL
  username...2 <- NULL
  username...8 <- NULL
  id...5 <- NULL
  id...6 <- NULL
  id...4 <- NULL
  dimensions <- NULL
  created_at <- NULL
  edge_media_to_comment <- NULL
  edge_media_preview_like <- NULL
  edge_media_to_caption <- NULL
  edge_media_to_comment_data <- NULL
  taken_at_timestamp <- NULL

  if(all(map_lgl(list(path, type, input_argument), is.null)))
    stop(paste("Please specify path, type, and input_argument"))

  if(!is.null(type)){
    if(!type %in% c("comments", "media", "profiles"))
      stop(paste("Please specify comments, media, or profiles"))

  }

  if(!is.null(input_argument)){
    if(!input_argument %in% c("username", "tag", "location", "none"))
      stop(paste("Please specify username, tag, location, or none"))
  }

  if((type == "profiles" & input_argument %in% c("username", "tag", "location"))|(type == "media" & input_argument %in% c("username", "tag", "location")))
    stop(paste("Please specify input_agument = 'none'"))

  if(type == "comments"){
    if(input_argument == "username"){

      df <- fromJSON(path) %>%
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

    }else if(input_argument == "tag"){

      df <- fromJSON(path) %>%
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


    }else if(input_argument == "location"){

      df <- fromJSON(paste) %>%
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
    }
  }else if(type == "media"){

    df <- jsonlite::fromJSON(path) %>%
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


  }else if(type == "profiles"){
     temp <- jsonlite::fromJSON(path) %>%
        pluck(1)

     info <- temp %>% pluck(2)

     created_time <- temp %>% pluck(1)

     username <- temp %>% pluck(3)

     df <- as_tibble(info) %>%
        bind_cols(created_time = created_time, username = username) %>%
        mutate(created_time = as.POSIXct(created_time, origin = "2020-01-21"))

  }

  return(df)

}

