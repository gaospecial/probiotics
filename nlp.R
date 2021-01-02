# request an access token of Baidu NLP service
baidu_access_token <- function(){
  base_url <- "https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials"
  client_id <- yaml::read_yaml("api-key.yaml")$baidu_nlp_api_key
  client_secret <- yaml::read_yaml("api-key.yaml")$baidu_nlp_api_secret
  url <- paste0(base_url, "&client_id=", client_id)
  url <- paste0(url, "&client_secret=", client_secret)
  data <- rjson::fromJSON(file = url)
  return(data)
}

#' Use Baidu Cloud word segmentation service
#'
#' @param text 
#' @param mode Using 0, the web content model, or 1 the Baidu query model.
#'
#' @return 
#' @export
#'
#' @examples  baidu_segment("高春辉是个好同志吗？")
baidu_segment <- function(text, mode = 0){
  base_url <- "https://aip.baidubce.com/rpc/2.0/nlp/v1/depparser?"
  access_token <- request_access_token()$access_token
  url <- paste0(base_url, "&charset=UTF-8")
  url <- paste0(url, "&access_token=", access_token)
  require(httr)
  d <- list(text = text, mode = mode)
  response <- POST(url = url,
       body = d,
       encode = "json")
  data <- content(response)
  structure(data, class = "baidu_segment")
}


