read_wos_tsv <- function(file){
  data <- read.delim(file = file, header = TRUE, comment.char = "(", encoding = "UTF-8")
  colnames <- colnames(data)
  field <- colnames[[1]]
  colnames(data) <- c("Group","Record","Percent")
  list(data = data, field = field)
}

plot.ly <- function (g, tooltip = c("text"), ...) {
  require(plotly)
  ggplotly(g, tooltip = tooltip, ...) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("sendDataToCloud", "pan2d", 
                                      "select2d", "lasso2d", "toggleSpikelines", 
                                      "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(xaxis = list(fixedrange = TRUE)) %>% layout(yaxis = list(fixedrange = TRUE))
}

hbarplot <- function(d, n=NULL){
  require("forcats")
  require("ggplot2")
  if (ncol(d) != 2) stop("data must have only two columns.")
  if(is.numeric(n)) d <- head(d,n)
  colnames(d) <- c("name","value")
  d <- d %>%
    mutate(no=row_number()) %>%
    mutate(no=factor(no, levels = rev(no)))
  v <- max(d$value)/2
  ggplot(d, aes(no, value)) + geom_col() +
    scale_y_continuous(expand = expansion(mult = c(0,.02)))+
    geom_text(aes(label = name, y = value - 1), size = 3,
              vjust = 0.3, hjust = 1, data = function(d) d[d$value > v, ], color = "white", fontface = "bold") +
    geom_text(aes(label = name, y = value + 1), size = 3,
              vjust = 0.5, hjust = 0, data = function(d) d[d$value <= v, ]) +
    coord_flip() +
    theme_light() +
    theme(axis.text.y = element_text(face = "bold",
                                     margin = margin(t=1,r=0,b=0,l=0,unit = "pt")))
}

is.part_of_china <- function (x) {
  require(stringr)
  str_detect(x, 
             pattern = stringr::regex("CHINA|TAIWAN|HONG KONG|MACAO", 
                                      ignore_case = T))
}

caption_download <- function(...){
  x <- paste0(...," [ppt](", fig_path(".pptx"),")")
  return(x)
}

graph2pptx <- function(..., file = fig_path(".pptx")){
  require(export)
  export::graph2ppt(..., file = file)
}

simplify_document_type <- function(x){
  article_review <- regex("^Review|Article$", ignore_case = TRUE)
  x[!str_detect(x, article_review)] <- "Others"
  return(x)
}

geocode <- function(address, service = c("bing","google")){
  service <- match.arg(service)
  address <- trimws(address)
  address <- gsub(address,pattern=" +",replacement="+")
  switch(service,
         bing = {
           bing_geocode(address)
         },
         google = {
           google_geocode(address)
         }
         )
}

bing_geocode <- function(address){
  base_url <- "http://dev.virtualearth.net/REST/v1/Locations?"
  api_key <- "AhPeESl2pMKtNb6XkummhqtlmEidL_W-rTX1X9lqijDrgZpJ34TO_lom8WDvdbDV"
  url <- paste0(base_url,"q=",address,"&key=",api_key)
  url <- URLencode(enc2utf8(url))
  data <- rjson::fromJSON(file = url)
  structure(data, class = "bingGeocode")
}


google_geocode <- function(address){
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json?"
  api_key <- "AIzaSyCcTKF_kF6QnZnNFWr4aVx6mAWfYVDKEIQ"
  url <- paste0(base_url,"address=",address,"&key=",api_key)
  url <- URLencode(enc2utf8(url))
  data <- rjson::fromJSON(file = url)
  structure(data, class="googleGeocode")
}

geocode_country <- function(obj){
  # for Google geocode result
  if (inherits(obj, "googleGeocode")){
    components <- obj$results[[1]]$address_components
    found <- lapply(components, function(x){
      types <- x$types
      any(types %in% "country")
    })
    id <- which(unlist(found)==TRUE)
    if (length(id)<1) return(NA)
    return(components[[id]]$long_name)
  }
  
  # for Bing Geocode result
  if (inherits(obj, "bingGeocode")){
    return(obj$resourceSets[[1]]$resources[[1]]$address$countryRegion)
  }
}

geocode_province <- function(obj){
  # for Google geocode result
  if (inherits(obj, "googleGeocode")){
    components <- obj$results[[1]]$address_components
    found <- lapply(components, function(x){
      types <- x$types
      any(types %in% "administrative_area_level_1")
    })
    id <- which(unlist(found)==TRUE)
    if (length(id)<1) return(NA)
    return(components[[id]]$long_name)
  }
  
  # for Bing Geocode result
  if (inherits(obj, "bingGeocode")){
    return(obj$resourceSets[[1]]$resources[[1]]$address$adminDistrict)
  }
}

geocode_state <- geocode_province
