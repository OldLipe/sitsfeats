library(disk.frame)

#a <- matrix(data = 1:10000000*24, nrow = 10000000, ncol = 24)
a <- matrix(data = 1:10000*24, nrow = 10000, ncol = 24)
#system.time({sitsfeats::polar_balance(a)})
bb <- as.data.frame(a)
b <- disk.frame::as.disk.frame(
  bb,
  nchunks = disk.frame::recommend_nchunks(bb, conservatism = 8),
  compress = 100,
  outdir = "/home/rstudio/teste_fst/",
  overwrite = TRUE)

#aa <- disk.frame::imap(b, function(x){
#  mat <- as.matrix(x)
#  sitsfeats::polar_balance()
#})


disk.frame::setup_disk.frame(workers = 4)

df_mat <- disk.frame::delayed(b, function(x){
  mat <- as.matrix(x)
  sitsfeats::polar_balance(mat)
})


system.time({disk.frame::collect_list(df_mat, parallel = TRUE)})
col_a <- dplyr
