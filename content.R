# content analysis functions


# 整合多个关键词为一个关键词
keywords_from <- function(..., list = NULL, name = "primary"){
  keyword_list <- list(...)
  if (!is.null(list)) keyword_list <- c(keyword_list, list)
  result <- lapply(keyword_list, function(x){
    x[[name]]
  })
  paste0(unlist(result), collapse = "|")
}




# 提取历史引证网络中的论文
extract_from_hist_graph <- function(M=NULL, g=NULL){
  name <- V(g)$name
  doi <- str_extract_all(name, "10\\.[0-9]+\\/\\S+")
  doi <- unlist(doi)
  M %>% filter(DI %in% doi)
}


