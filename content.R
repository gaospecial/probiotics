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

# 根据检索词对文献进行分类
#' Tag record by regular expression search
#'
#' @param x 
#' @param name the name of new column
#' @param pattern 
#' @param pattern.names 
#' @param sep default is ";"
#'
#' @return
#' @export
#'
#' @examples
tag_by_regex <- function(x, pattern, pattern.names = names(pattern), sep = ";"){
  require(stringr)
  nRecord <- length(x)
  result <- vector("list", length = nRecord)
  nPattern <- length(pattern)
  for (i in 1:nRecord){
    this_record <- x[[i]]
    idx <- vector(length = nPattern)
    for (j in 1:nPattern){
      this_pattern <- pattern[[j]]
      if (!is.na(this_record) & str_detect(this_record, this_pattern)) idx[[j]] <- TRUE
    }
    result[[i]] <- paste0(pattern.names[idx], collapse = sep)
  }
  return(unlist(result))
}


# 将排在后面的项目合并成 others
shrink_lvl <- function(x, sort = TRUE, keep = 9, other = "Other"){
  if (is.factor(x)) x <- as.character(x)
  lvl <- names(sort(table(x), decreasing = TRUE))
  lvl_new <- head(lvl, n = keep)
  ret <- rep(other, times = length(x))
  idx <- x %in% lvl_new
  ret[idx] <- x[idx]
  return(ret)
}
