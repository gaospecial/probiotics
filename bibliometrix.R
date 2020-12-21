authorProdOverTime2 <- function (M, k = 10, graph = TRUE) {
  # 修改 bibliometrix 的函数
  M$TC = as.numeric(M$TC)
  M$PY = as.numeric(M$PY)
  AU = names(tableTag(M, "AU"))
  k = min(k, length(AU))
  AU = AU[1:k]
  Y = as.numeric(substr(Sys.time(), 1, 4))
  if (!("DI" %in% names(M))) {
    M$DI = "NA"
  }
  
  list <- lapply(1:length(AU), function(i){
    ind = which(regexpr(AU[i], M$AU, fixed = TRUE) > -1)
    TCpY = M$TC[ind] / (Y - M$PY[ind] + 1)
    tibble( Author = rep(AU[i], length(ind)),
            year = M$PY[ind],
            TI = M$TI[ind],
            SO = M$SO[ind],
            DOI = M$DI[ind],
            TC = M$TC[ind],
            TCpY = TCpY )
  })
  df <- do.call("rbind", list)
  df2 <- dplyr::group_by(df, .data$Author, .data$year) %>%
    dplyr::summarise(
      freq = length(.data$year),
      TC = sum(.data$TC),
      TCpY = sum(.data$TCpY)
    )
  df2 = as.data.frame(df2)
  df2$Author = factor(df2$Author, levels = AU[k:1])
  g <- ggplot(df2, aes(year,Author)) +
    geom_point(aes(alpha = TCpY, size = freq),
               color = "dodgerblue4") +
    scale_size(range = c(2, 6)) +
    scale_alpha(range = c(0.3, 1)) +
    scale_x_continuous(breaks = function(x) seq(min(x),max(x),by = 2)) +
    guides(size = guide_legend(order = 1,
                               "N.Articles"),
           alpha = guide_legend(order = 2,
                                "TC/Year")) +
    labs(title = "Top-Authors' Production over the Time") + 
    geom_line(
           aes(group = Author),
           size = 1,
           color = "firebrick",
           alpha = 0.3
         )
  df$DOI = as.character(df$DOI)
  res <- list(dfAU = df2,
              dfPapersAU = df,
              graph = g)
  if (isTRUE(graph)) {
    return(g)
  } else{
    return(res)
  }
  
}


tableTag <- function (M, Tag = "CR", sep = ";") {
  if (Tag %in% c("AB", "TI")) {
    M = termExtraction(M, Field = Tag, stemming = F, verbose = FALSE)
    i = dim(M)[2]
  }
  else {
    i <- which(names(M) == Tag)
  }
  if (Tag == "C1") {
    M$C1 = gsub("\\[.+?]", "", M$C1)
  }
  Tab <- unlist(strsplit(as.character(M[, i]), sep))
  # Tab <- trimws(trimES(gsub("\\.|\\,", " ", Tab)))
  Tab <- Tab[Tab != ""]
  Tab <- sort(table(Tab), decreasing = TRUE)
  return(Tab)
}


# 提取历史引证网络中的论文
extract_from_hist_graph <- function(M=NULL, g=NULL){
  require("stringr")
  name <- V(g)$name
  doi <- str_extract_all(name, "10\\.[0-9]+\\/\\S+")
  doi <- unlist(doi)
  M %>% filter(DI %in% doi)
}

# 输出部分内容
DT_output <- function(M, caption = "", 
                      filename = fig_path(), 
                      column = c("TI","DI","TC")){
  columns <- colnames(M)
  if ("SR" %in% columns) rownames(M) <- M$SR
  M$link <- permanent_link(type = "html",id=M$DI, title = M$TI, alt = "")
  DT::datatable(M %>% select(link,TC),
                colnames = c("文章标题","被引频次"),
                escape = FALSE,
                rownames = TRUE,
                filter = "top",
                width = "95%",
                caption = caption,
                extensions = c("Buttons"),
                options=list(dom = 'Bfrtip',
                             pageLength = 10,
                             buttons=list(
                               'pageLength',
                               list(extend='copy'),
                               list(extend="excel",
                                    filename=filename,
                                    header=TRUE,
                                    title="")
                             ),
                             columnDefs=list(
                               list(width="80%",targets=1),
                               list(width="100px",targets="_all")
                             ),
                             lengthMenu=list(c(10,20,50,100,200,-1),
                                             c("10","20","50","100","200","All")),
                             autoWidth=TRUE)) %>%
    DT::formatStyle(fontSize="9pt",columns = 0:4)
}

permanent_link <- function(base_url="https://doi.org/",
                           type=c("markdown","html"),
                           id=NULL,title=NULL,alt=NULL){
  type <- match.arg(type)
  if (type == "markdown") s <- paste0("[",title,"](",base_url,id," \"",alt,"\")")
  if (type == "html") s <- paste0('<a href="',base_url,id,'" alt="',alt,'">',title,'</a>')
  return(s)
}

# 在指定项中查找
grep_in_field <- function(M, pattern, Field = "C1"){
  
}