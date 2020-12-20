
# horizontal bar plot
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


#' tableTag downstream plot
#'
#' @param M bibliometrix dataframe
#' @param Tag passed to tableTag()
#' @param n top n
#'
#' @return
#' @export
#'
#' @examples
#' library(bibliometrix)
#' data("garfield")
#' tableTag_barplot(garfield, Tag = "AU")
tableTag_barplot <- function(M, Tag="AU",n=30){
  require("forcats")
  require("tibble")
  require("ggplot2")
  require("dplyr")
  d <- tableTag(M,Tag = Tag) %>% enframe() %>%
    dplyr::filter(name != "NA") %>%
    mutate(name=as_factor(as.character(name))) %>%
    head(n)
  v <- max(d$value)/2
  title <- paste0("文章数量Top",n)
  hbarplot(d) +
    labs(x = NULL, y = "No. of record",title=title)
}

# 对一批论文进行四维分析，显示国家、机构、人员和期刊
four_dimension_barplot <- function(M, tags = c("AU","AU_CO_NR","AU_UN_NR","J9")){
  require(cowplot)
  plots <- lapply(tags, function(x){
    tableTag_barplot(M,Tag = x, n=10) + labs(title="")
  })
  plot_grid(plotlist = plots, labels = "AUTO")
}