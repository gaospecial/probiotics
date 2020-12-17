read_wos_tsv <- function(file){
  data <- read.delim(file = file, header = TRUE, comment.char = "(", encoding = "UTF-8")
  colnames <- colnames(data)
  field <- colnames[[1]]
  colnames(data) <- c("Group","Record","Percent")
  list(data = data, field = field)
}

plot.ly <- mrgut::plot.ly
