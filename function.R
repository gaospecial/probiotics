# read WoS export analysis summary
read_wos_tsv <- function(file){
  data <- read.delim(file = file, header = TRUE, comment.char = "(", encoding = "UTF-8")
  colnames <- colnames(data)
  field <- colnames[[1]]
  colnames(data) <- c("Group","Record","Percent")
  list(data = data, field = field)
}

# just a optimized ggplotly()
plot.ly <- function (g, tooltip = c("text"), ...) {
  require(plotly)
  ggplotly(g, tooltip = tooltip, ...) %>% 
    config(displaylogo = FALSE, 
           modeBarButtonsToRemove = c("sendDataToCloud", "pan2d", 
                                      "select2d", "lasso2d", "toggleSpikelines", 
                                      "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(xaxis = list(fixedrange = TRUE)) %>% layout(yaxis = list(fixedrange = TRUE))
}

# determine wheter it is part of China
is.part_of_china <- function (x) {
  require(stringr)
  str_detect(x, 
             pattern = stringr::regex("CHINA|TAIWAN|HONG KONG|MACAO", 
                                      ignore_case = T))
}

# show file download links for figure
caption_download <- function(...){
  x <- paste0(...," [ppt](", fig_path(".pptx"),"),")
  x <- paste0(x, " [pdf](", fig_path(".pdf"),")")
  return(x)
}

# export current plot to .pptx
graph2pptx <- function(..., file = fig_path(".pptx")){
  require(export)
  export::graph2ppt(..., file = file)
}

# only keep Article and Review in DT field
simplify_document_type <- function(x){
  article_review <- regex("^Review|Article$", ignore_case = TRUE)
  x[!str_detect(x, article_review)] <- "Others"
  return(x)
}

source("geocode.R")
source("hbarplot.R")
source("network.R")