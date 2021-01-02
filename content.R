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

# 对问卷中的选择题进行可视化
# 适用于问卷星的导出数据
summarise_plot <- function(data, 
                           question = "Q1", 
                           plot = c("pie","bar"), 
                           multi_choice = FALSE, 
                           row_split = "┋"  ){
  plot <- match.arg(plot)
  if (multi_choice) data <- data %>% separate_rows(question,sep = row_split)
  x <- data[[question]]
  x <- str_wrap(x, width = 24, exdent = 4)
  if (plot == "pie") p <- ggpie(x, sort = FALSE) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.text = element_text(margin = margin(t=2,b=2)))
  if (plot == "bar") p <- hbarplot(x, show = "name", sort = FALSE)
  return(p)
}


# 搜索问题，根据问题的类型自动选择绘制饼图或条形图
# 适用于蜂鸟问卷的导出数据
plot_this_question <- function(data, question, 
                               multi_choice = "auto", 
                               plot = c("auto","pie","bar"), ...){
  plot <- match.arg(plot)
  colname <- colnames(data)
  idx <- which(str_detect(colname, question))
  if (multi_choice == "auto"){
    multi_choice <- FALSE
    if (length(idx) > 2) multi_choice <- TRUE
    if (any(str_detect(colname[idx], "多选"))) multi_choice <- TRUE
  }
  if (multi_choice) {
    x <- unlist(data[idx]) %>% as.character()
  } else {
    x <- data[[idx[[1]]]]
  }
  
  if (plot == "auto"){
    if (multi_choice) {
      plot <- "bar"
    } else {
      plot <- "pie"
    }
  }
  if (plot == "pie") p <- ggpie(x, ...)
  if (plot == "bar") p <- hbarplot(x, ...)
  return(p)
}


# 搜索问题，获得问题题干
# 适用于蜂鸟问卷的导出数据
get_question_name <- function(data, question){
  colname <- colnames(data)
  idx <- which(str_detect(colname, question))
  name <- str_extract(colname[idx][[1]], "([^\\.]+\\？)")
  return(name)
}
