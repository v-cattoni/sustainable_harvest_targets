#analysis

load("./mcmc_data/bh_data.RData")
load("./mcmc_data/rk_data.RData")
load("./mcmc_data/pt_data.RData")
load("./mcmc_data/hs_data.RData")

load("./mcmc_data/bh_ml.RData")
load("./mcmc_data/rk_ml.RData")
load("./mcmc_data/pt_ml.RData")
load("./mcmc_data/hs_ml.RData")


#### HISTOGRAMS ####

hs_vs_bh_prob <- hs_ml / (hs_ml + bh_ml)
hs_vs_rk_prob <- hs_ml / (hs_ml + rk_ml)
hs_vs_pt_prob <- hs_ml / (hs_ml + pt_ml)

num_hs_vs_bh <- 0
num_hs_vs_rk <- 0
num_hs_vs_pt <- 0

for (i in 1:284){
  if(hs_vs_bh_prob[i] >= 0.5){
    num_hs_vs_bh = num_hs_vs_bh + 1
  }
  if(hs_vs_rk_prob[i] >= 0.5){
    num_hs_vs_rk = num_hs_vs_rk + 1
  }
  if(hs_vs_pt_prob[i] >= 0.5){
    num_hs_vs_pt = num_hs_vs_pt + 1
  }
}

cat("prop hs more likely than bh = ", num_hs_vs_bh/284)
cat("prop hs more likely than rk = ", num_hs_vs_rk/284)
cat("prop hs more likely than pt = ", num_hs_vs_pt/284)

jpeg(file = paste("./plots/hs_vs_bh_prob.jpeg"),
     width = 600, height = 600)
hist(hs_vs_bh_prob)
dev.off()

jpeg(file = paste("./plots/hs_vs_rk_prob.jpeg"),
     width = 600, height = 600)
hist(hs_vs_rk_prob)
dev.off()

jpeg(file = paste("./plots/hs_vs_pt_prob.jpeg"),
     width = 600, height = 600)
hist(hs_vs_pt_prob)
dev.off()




#### DISTRIBUTION PLOTS ####
# for (i in 1:num_cols){
#   jpeg(file = paste("./plots/", as.character(i), "_bh_r.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(bh_data[[i]], show_density = TRUE, pars = c("r"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_bh_k.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(bh_data[[i]], show_density = TRUE, pars = c("k"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_bh_sigma_y.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(bh_data[[i]], show_density = TRUE, pars = c("sigma_y"))
#   dev.off()
#   
#   jpeg(file = paste("./plots/", as.character(i), "_rk_r.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(rk_data[[i]], show_density = TRUE, pars = c("r"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_rk_k.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(rk_data[[i]], show_density = TRUE, pars = c("k"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_rk_sigma_y.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(rk_data[[i]], show_density = TRUE, pars = c("sigma_y"))
#   dev.off()
#   
#   jpeg(file = paste("./plots/", as.character(i), "_pt_r.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(pt_data[[i]], show_density = TRUE, pars = c("r"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_pt_k.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(pt_data[[i]], show_density = TRUE, pars = c("k"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_pt_sigma_y.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(pt_data[[i]], show_density = TRUE, pars = c("sigma_y"))
#   dev.off()
#   
#   jpeg(file = paste("./plots/", as.character(i), "_hs_r.jpeg", sep = ''),
#        width = 600, height = 600)
#   plot(hs_data[[i]], show_density = TRUE, pars = c("r"))
#   dev.off()
#   jpeg(file = paste("./plots/", as.character(i), "_hs_k.jpeg", sep = ''),
#       width = 600, height = 600)
#  plot(hs_data[[i]], show_density = TRUE, pars = c("k"))
#  dev.off()
#  jpeg(file = paste("./plots/", as.character(i), "_hs_sigma_y.jpeg", sep = ''),
#       width = 600, height = 600)
#  plot(hs_data[[i]], show_density = TRUE, pars = c("sigma_y"))
#  dev.off()
#}

#### WAIC ####
# bh_waic <- c()
# rk_waic <- c()
# pt_waic <- c()
# hs-waic <- c()
# 
# for (i in 1:num_cols){
#   log_lik1 <- extract_log_lik(bh_fit[[i]])
#   waic1 <- loo::waic(log_lik1)
#   waic1
# }