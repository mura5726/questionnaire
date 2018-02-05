ctg_dup = function(data, split){
  setwd(data_path)
  gax = read.csv(data, header = F) %>% as.tibble()
  gax_ctg = NULL
  for(k in 1:dim(gax)[1]){
    gax_ctg = c(gax_ctg, (gax[k,2] %>% as.matrix() %>% strsplit(split) %>% unlist())[1])
  }
  d = gax_ctg[!duplicated(gax_ctg)] %>% tibble()
  colnames(d) = "ctg"
  d
}
