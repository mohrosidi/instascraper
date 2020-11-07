#' Return user profile informations
#'
#' \code{get_profiles} return profile informations from username input.
#'
#' @param username  String vector. Specify  username accounts
#' @param your_username  String. Instagram login user
#' @param your_password String. Instagram login password
#' @param following_input Logical value.  Use profiles followed by login-user as input
#' @param retry_download Logical values. Retry download attempts endlessly when errors are received
#'
#' @importFrom purrr pluck map_lgl map_dfr
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr  mutate bind_cols %>%
#' @importFrom fs dir_ls
#' @importFrom stringr str_detect
#' @return A tibble and profile metadata
#'
#' @examples
#' \dontrun{
#' get_profiles(following_input = TRUE, your_username = "xxxx", your_password = "123")
#' }
#'
#' @export
get_profiles <- function(username = NULL,
                         your_username = NULL,
                         your_password = NULL,
                         following_input = FALSE,
                         retry_download = FALSE){

  . <- NULL

  # Check parameters ----
  if(all(map_lgl(list(username, your_username, your_password), is.null))){
    stop(paste("Please specify at least a username, tag or location"))
  }

  ## Check username ----
  if(!is.null(username)){
    if(!is.character(username))
      stop(paste("username must be character"))
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


  query <- list(ifelse(is.null(username), NA, username2),
                ifelse(following_input, paste("--followings-input"), NA),
                ifelse(is.null(your_username), NA, paste("-u", your_username)),
                ifelse(is.null(your_password), NA, paste("-p", your_password)),
                ifelse(retry_download,paste("--retry-forever"), NA))

  query <- unlist(query) %>% .[!is.na(.)] %>% paste(., collapse = " ")


  cmd <- paste("instagram-scraper", query, "-m 1 --media-types none --profile-metadata")



  ## Download the data
  system(cmd)

  # Parse the data ----
  if(!is.null(username)){

    df <- map_dfr(username, function(x){

      temp <- jsonlite::fromJSON(paste0(x, "/", x, ".json")) %>%
        pluck(1)

      info <- temp %>% pluck(2)

      created_time <- temp %>% pluck(1)

      username <- temp %>% pluck(3)

      as_tibble(info) %>%
        bind_cols(created_time = created_time, username = username) %>%
        mutate(created_time = as.POSIXct(created_time, origin = "2020-01-21"))


    })
  }else if(following_input){

    ls <- fs::dir_ls(all = TRUE, recurse = TRUE, type = "file")
    ls <- ls[stringr::str_detect(ls, pattern = ".json")]

    df <- map_dfr(ls, function(x){

      temp <- jsonlite::fromJSON(x) %>%
        pluck(1)

      info <- temp %>% pluck(2)

      created_time <- temp %>% pluck(1)

      username <- temp %>% pluck(3)

      as_tibble(info) %>%
        bind_cols(created_time = created_time, username = username) %>%
        mutate(created_time = as.POSIXct(created_time, origin = "2020-01-21"))

    })

  }else{
    NULL
  }


  return(df)

}
