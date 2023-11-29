#analysis

load("bh_data.RData")
load("rk_data.RData")
load("pt_data.RData")
load("hs_data.RData")

bh_waic <- c()
rk_waic <- c()
pt_waic <- c()
hs-waic <- c()

for (i in 1:num_cols){
  log_lik1 <- extract_log_lik(bh_fit[[i]])
  waic1 <- loo::waic(log_lik1)
  waic1
}




#### PLOTS ####
for (i in 1:num_cols){
  jpeg(file = paste("./plots/", as.character(i), "_bh_r.jpeg", sep = ''),
       width = 600, height = 600)
  plot(bh_data[[i]], show_density = TRUE, pars = c("r"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_bh_k.jpeg", sep = ''),
       width = 600, height = 600)
  plot(bh_data[[i]], show_density = TRUE, pars = c("k"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_bh_sigma_y.jpeg", sep = ''),
       width = 600, height = 600)
  plot(bh_data[[i]], show_density = TRUE, pars = c("sigma_y"))
  dev.off()
  
  jpeg(file = paste("./plots/", as.character(i), "_rk_r.jpeg", sep = ''),
       width = 600, height = 600)
  plot(rk_data[[i]], show_density = TRUE, pars = c("r"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_rk_k.jpeg", sep = ''),
       width = 600, height = 600)
  plot(rk_data[[i]], show_density = TRUE, pars = c("k"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_rk_sigma_y.jpeg", sep = ''),
       width = 600, height = 600)
  plot(rk_data[[i]], show_density = TRUE, pars = c("sigma_y"))
  dev.off()
  
  jpeg(file = paste("./plots/", as.character(i), "_pt_r.jpeg", sep = ''),
       width = 600, height = 600)
  plot(pt_data[[i]], show_density = TRUE, pars = c("r"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_pt_k.jpeg", sep = ''),
       width = 600, height = 600)
  plot(pt_data[[i]], show_density = TRUE, pars = c("k"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_pt_sigma_y.jpeg", sep = ''),
       width = 600, height = 600)
  plot(pt_data[[i]], show_density = TRUE, pars = c("sigma_y"))
  dev.off()
  
  jpeg(file = paste("./plots/", as.character(i), "_hs_r.jpeg", sep = ''),
       width = 600, height = 600)
  plot(hs_data[[i]], show_density = TRUE, pars = c("r"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_hs_k.jpeg", sep = ''),
       width = 600, height = 600)
  plot(hs_data[[i]], show_density = TRUE, pars = c("k"))
  dev.off()
  jpeg(file = paste("./plots/", as.character(i), "_hs_sigma_y.jpeg", sep = ''),
       width = 600, height = 600)
  plot(hs_data[[i]], show_density = TRUE, pars = c("sigma_y"))
  dev.off()
}
