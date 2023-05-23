setwd('C:/Users/Adam/OneDrive/Projekty/AGH_KNMF_konferencja')

rm(list = ls())
library(ghyp)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(quantmod)
library(tidyquant)

new_data = TRUE


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# IR
IR <- read.csv("https://datahub.io/core/bond-yields-us-10y/r/monthly.csv", stringsAsFactors = F) %>% 
  mutate(Date = as.Date(Date, format = '%Y-%m-%d')+1)

if(new_data == T){
path <- "https://query1.finance.yahoo.com/v7/finance/download/%5ETNX?period1=-252374400&period2=1684800000&interval=1d&events=history&includeAdjustedClose=true"
IR <- read.csv(path) %>%
  select(Date, Rate = Adj.Close) %>% mutate(Date = as.Date(Date), Rate = as.numeric(Rate))
}
IR %>% plot(type = 'l')

# SPX
SPX <- read.csv2('SPX500.csv', stringsAsFactors = F)[-1,]
SPX[,'Date'] <- NA
SPX[1,'Date'] <- '01-04-2021'

SPX <-
  SPX %>% 
  mutate(Date = as.Date(Date, format = ' %d-%m-%Y')) %>% 
  select(Date, SPX = Close)

for (i in 1:(dim(SPX)[1]-1)){
  SPX[i+1,1] <- SPX[i,1] %m-% months(1)
}

if(new_data == T){
SPX <- read.csv("https://query1.finance.yahoo.com/v7/finance/download/%5ESPX?period1=-1325635200&period2=1684800000&interval=1d&events=history&includeAdjustedClose=true") %>%
  select(Date, SPX = Adj.Close) %>% mutate(Date = as.Date(Date))
}


# HPI
HPI <- read.csv2('HPI_PO_monthly_hist.csv', stringsAsFactors = F) %>% 
  mutate(Date = as.Date(Month, format = '%d.%m.%Y')) %>% select(Date, HPI = USA)

joined <- IR %>% full_join(SPX) %>% #full_join(HPI) %>%
  arrange(Date) #%>% filter(Date >= '2007-01-01') 
#joined[c(809:816),2] <- c(0.678,0.678,0.857,0.929,0.917,1.078,1.426,1.676)

combined <- joined %>% filter(Date  >= '1985-01-02') %>% filter(WEEKDAY(Date) == 3) %>% group_by(substr(Date, 1, 7)) %>% mutate(n = 1:n()) %>%
  filter(n == max(n)) %>% ungroup %>% select(Date, Rate, SPX) %>%
  mutate(
         SP500_return = (SPX - lag(SPX,3))/ lag(SPX,3),
         SP500_scaled = (SPX/first(SPX))*100,
         IR_return_scaled = (Rate/first(Rate))*100,
         IR_return = (Rate - lag(Rate,3))/ lag(Rate,3),
         IR_return = lead(IR_return,2),
         #HPI_return = (HPI - lag(HPI,3))/ lag(HPI,3),
         #HPI_return_n = scale(HPI_return)[,1],
         SP500_return_n = scale(SP500_return)[,1],
         IR_return_n = scale(IR_return)[,1]) %>%
  filter(is.na(SP500_return) == F) %>% filter(Date  >= '2000-01-01') %>% filter(is.na(IR_return) == F)
  
(cor(combined$IR_return,combined$SP500_return))

combined_crisis <- combined %>% filter(as.numeric(substr(Date,1,4)) >= 2007 &
                                         as.numeric(substr(Date,1,4)) <= 2008) 
options(scipen = 5)

IR_plot <- ggplot(combined, aes(x = Date)) + 
  geom_line(aes(y = Rate, col = 'IR'), col = 'black') +
  ylab('Interest Rate')

png('IR.png', width = 1920*2, height = 1080*2,res = 200*2)
print(IR_plot)
dev.off()

SPX_plot <-   ggplot(combined, aes(x = Date)) + 
  geom_line(aes(y = SP500_scaled, col = 'IR'), col = 'black') +
  ylab('S&P 500 Index')

png('SPX.png', width = 1920*2, height = 1080*2,res = 200*2)
print(SPX_plot)
dev.off()

png('SPX_IR.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(SPX_plot,IR_plot, nrow = 2)
dev.off()

plot(combined$HPI_return_n,combined$IR_return_n, xlim = c(-4,4), ylim = c(-4,4), pch = 19)
points(combined_crisis$SP500_return_n,combined_crisis$IR_return_n, pch = 19, col = 'red')
grid()

plot(combined$SP500_return,combined$IR_return, pch = 19)
points(combined_crisis$SP500_return,combined_crisis$IR_return_n, pch = 19, col = 'red')
grid()


IR_NIG <- fit.NIGuv(combined$IR_return, silent = T)
SP500_NIG <- fit.NIGuv(combined$SP500_return, silent = T)

combined %>% select(IR_return) %>% as.matrix %>% density %>% plot(main = 'YR1 log return probability density function',lwd = 2)
qghyp(seq(from=0, to =1, by = 0.001),IR_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topright',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2),lwd = 2, col = c('black','cadetblue3'))

combined %>% select(SP500_return) %>% as.matrix %>% density %>% plot(main = 'EQ WIG log return probability density function',lwd = 2, ylim = c(0,7))
qghyp(seq(from=0, to =1, by = 0.001),SP500_NIG) %>% density %>% lines(col = 'cadetblue3', lwd = 2, lty = 2)
grid()
legend('topleft',c('empirical distibution', 'fitted NIG distibution'), lty = c(1,2), lwd = 2, col = c('black','cadetblue3'))

# transformation into uniform vectors - since in that implementation copulas are fitted on uniformly distibuted margins
# we are using previously performed distribution fitting
uniform_IR_1 <- combined %>% select(IR_return) %>% pghyp(IR_NIG)
uniform_WIG <- combined %>% select(SP500_return) %>% pghyp(SP500_NIG)
hist(uniform_IR_1) # since we are transforming empirical data - it will be as good as our distibution fitting
hist(uniform_WIG)

library(VineCopula)
library(gridExtra)
# selecting most appropriate copula based on AIC criterion
(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_WIG))
#(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_WIG, familyset = 4))

# since rotated Gumbel copula was choosen we will simulated from this type of copula to get intuition on its shape
simulated_rotated_Gumbel_copula <- BiCopSim(2000,family = fitted_copula$family, par = 0.66) %>% qnorm %>% data.frame
plot(simulated_rotated_Gumbel_copula, xlim = c(-4,4), ylim = c(-4,4), pch = 19, alpha = 0.6)
grid()

A <-
  ggplot(simulated_rotated_Gumbel_copula) + 
  geom_point(aes(x = X1, y = X2, colour = 'Joy Copula'), pch = 19, size = 1) +
  theme(legend.position = "none") + 
  scale_fill_gradient2('Contour levels',mid="mediumaquamarine", high="deepskyblue4") +
  scale_colour_manual('',values = 'brown2') +
  ylim(-4,4) + xlim(-4,4) +
  ggtitle('Simulated observations')

B <- 
  ggplot(simulated_rotated_Gumbel_copula) +
  stat_density2d(aes(x = X1, y = X2, fill = ..level..), 
               geom = 'polygon', colour = 'black', bins = 10, contour = T, h = rep(1.5, 2)) +
  ylim(-4,4) + xlim(-4,4) +
  theme(legend.position = "none") +
  ggtitle('Contour')


png('Joy_copula_2.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(B,A, ncol = 2)
dev.off()







# simulating copula based of choosen copula and its parameter
simulated_from_fitted_copula<- BiCopSim(3000,family = fitted_copula$family, par= fitted_copula$par, par2 =fitted_copula$par2)
simulated_IR <- simulated_from_fitted_copula[,1] %>% qghyp(IR_NIG)
simulated_SP500 <- simulated_from_fitted_copula[,2] %>% qghyp(SP500_NIG)




# ploting both empirical data and margins simulated from fitted copula
plot(simulated_IR,simulated_SP500, xlab = 'log returns of IR month to month', ylab = 'log returns of equity index WIG month to month')
points(combined$IR_return, combined$SP500_return, col = 'red', pch = 19)
grid()
legend('bottomright', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))

plot_df <- data.frame(simulated_IR,simulated_SP500) %>%
  mutate(simulated_IR_n = scale(simulated_IR),
         simulated_SP500_n = scale(simulated_SP500))

ggplot(plot_df) + stat_density2d(aes(x = simulated_IR, y = simulated_SP500, fill = ..density..), geom = 'tile', contour = FALSE) + 
  ylim(-0.2,0.2) + xlim(-0.2,0.2)# +
#scale_fill_gradient2(low="#44aa00", mid="#ffcc00", high="#502d16")

ggplot(plot_df) + stat_density2d(aes(x = simulated_IR, y = simulated_SP500, fill = ..level..), geom = 'polygon', colour = 'black', bins = 20) + 
  ylim(-0.2,0.2) + xlim(-0.2,0.2)

ggplot(plot_df) + 
  #stat_density2d(aes(x = simulated_IR, y = simulated_SP500, fill = ..level..), geom = 'polygon') +
  ylim(-0.4,0.4) + xlim(-0.4,0.4) + 
  geom_point(aes(x = simulated_IR, y = simulated_SP500), col = 'black', alpha = 0.3) + 
  geom_point(data = combined, aes(x = IR_return, y = SP500_return), pch = 19, col = 'red')

A <-
ggplot(plot_df) + 
  stat_density2d(aes(x = simulated_IR_n, y = simulated_SP500_n, fill = ..level..), 
                 geom = 'polygon', colour = 'black', bins = 30, contour = T, h = rep(2, 2)) +
  ylim(-4.5,4.5) + xlim(-4.5,4.5) + 
  #geom_point(aes(x = simulated_IR_n, y = simulated_SP500_n), col = 'black', alpha = 0.3) + 
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n, colour = 'Historical observations'), pch = 19, size = 1.3) +
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n), pch = 21, fill =gg_color_hue(1)[1],  size = 1.3) +
  theme(legend.position = "bottom") + 
  xlab('Interest Rate - returns - normalized') + ylab('SP500 - returns - normalized') +
  scale_fill_gradient2('Contour levels',mid="mediumaquamarine", high="deepskyblue4",guide=FALSE) +
  scale_colour_manual('',values = 'brown2') 


B <-
ggplot(plot_df %>% sample_n(2000)) +
  ylim(-4.5,4.5) + xlim(-4.5,4.5) + 
  geom_point(aes(x = simulated_IR_n, y = simulated_SP500_n, colour = 'Simulated observations'), alpha = 0.3) + 
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n, colour = 'Historical observations'), pch = 19, size = 1) +
  #geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n), pch = 21, fill =gg_color_hue(1)[1],  size = 1.2) +
  xlab('Interest Rate - returns - normalized') + ylab('SP500 - returns - normalized') +
  theme(legend.position = "bottom") +
  scale_colour_manual('',values = c('brown2','black'))





png('Results_copula_main_2.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(A,B, ncol = 2)
dev.off()



IR_normal <- fit.gaussuv(combined[,c('IR_return')])
SP_normal <- fit.gaussuv(combined[,c('SP500_return')])


multivariat_normal <- fit.gaussmv(combined[,c('IR_return','SP500_return')])
multivariat_normal_sim <- rghyp(50000, multivariat_normal) %>% data.frame() %>%
  mutate(SP500_return_n = scale(SP500_return)[,1],
         IR_return_n = scale(IR_return)[,1])
  

mnorm_plot <-
ggplot(plot_df) +
  stat_density2d(data = multivariat_normal_sim, aes(x = IR_return_n, y = SP500_return_n, fill = ..level..), 
                 geom = 'polygon', colour = 'black', bins = 8, contour = T, h = rep(1, 2)) +
  ylim(-4.5,4.5) + xlim(-4.5,4.5) +
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n, colour = 'Historical observations'), pch = 19, size = 1.3) +
  #geom_point(data = sample(multivariat_normal_sim,1000), aes(x = IR_return_n, y = SP500_return_n, colour = 'Multivariat normal'), pch = 19, size = 1) +
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n), pch = 21, fill = gg_color_hue(1)[1],  size = 1.3) +
  xlab('Interest Rate - returns - normalized') + ylab('SP500 - returns - normalized') +
  theme(legend.position = "bottom") +
  scale_fill_gradient2('Contour levels',mid="mediumaquamarine", high="deepskyblue4",guide=FALSE) +
  scale_colour_manual('',values = 'brown2') +
  ggtitle('Multivariat normal distribution fitted')

hist_plot <-
ggplot(plot_df) +
  ylim(-4.5,4.5) + xlim(-4.5,4.5) +
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n, colour = 'Historical observations'), pch = 19, size = 1) +
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n), pch = 21, fill = gg_color_hue(1)[1],  size = 1.3) +
  xlab('Interest Rate - returns - normalized') + ylab('SP500 - returns - normalized') +
  theme(legend.position = "bottom") +
  scale_colour_manual('',values = 'brown2') +
  ggtitle('Historically observed relation')
  
png('comparision_of_fitted_copulas.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(A + ggtitle('Fitted Clayton copula'),mnorm_plot, ncol = 2)
dev.off()

png('Multivariat_normal2.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(hist_plot,mnorm_plot, ncol = 2)
dev.off()

data_for_plot_sim <- data.frame(SP = qghyp(seq(0,1,0.001),SP_normal),
                                IR = qghyp(seq(0,1,0.001),IR_normal),
                                SP_NIG = qghyp(seq(0,1,0.001),SP500_NIG),
                                IR_NIG = qghyp(seq(0,1,0.001),IR_NIG))


SP_normal_plot <-
  ggplot(combined, aes(x = SP500_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = SP500_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  geom_line(data = data_for_plot_sim, aes(x = SP, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('S&P500 quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  scale_color_manual('',values = gg_color_hue(4)[c(3)]) +
  theme(legend.position = 'bottom') +
  xlim(c(-0.35, 0.3))

IR_normal_plot <-
  ggplot(combined, aes(x = IR_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = IR_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  geom_line(data = data_for_plot_sim, aes(x = IR, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('IR quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  scale_color_manual('',values = gg_color_hue(4)[c(3)]) +
  theme(legend.position = 'bottom')+
  xlim(c(-0.6, 0.75))


png('SP500_IR_fitted_normal.png', width = 1920*2.2, height = 1080*2.2,res = 250*2.2)
grid.arrange(SP_normal_plot,IR_normal_plot, ncol = 2)
dev.off()


SP_normal_plot <-
  ggplot(combined, aes(x = SP500_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = SP500_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  #geom_line(data = data_for_plot_sim, aes(x = SP, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('S&P500 quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  #scale_color_manual('',values = gg_color_hue(4)[c(1)]) +
  theme(legend.position = 'bottom') +
  xlim(c(-0.35, 0.3))

IR_normal_plot <-
  ggplot(combined, aes(x = IR_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = IR_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  #geom_line(data = data_for_plot_sim, aes(x = IR, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('IR quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  #scale_color_manual('',values = gg_color_hue(4)[c(1)]) +
  theme(legend.position = 'bottom') +
  xlim(c(-0.6, 0.75))


png('SP500_IR_empirical.png', width = 1920*2.2, height = 1080*2.2,res = 250*2.2)
grid.arrange(SP_normal_plot,IR_normal_plot, ncol = 2)
dev.off()


SP_NIG_plot <-
  ggplot(combined, aes(x = SP500_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = SP500_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  #geom_line(data = data_for_plot_sim, aes(x = SP, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  geom_line(data = data_for_plot_sim, aes(x = SP_NIG, colour = 'Fitted NIG dist'), lwd = 1.5, stat="density", lty = 5)+
  geom_line(data = data_for_plot_sim, aes(x = SP, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('S&P500 quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  scale_color_manual('',values = gg_color_hue(4)[c(4,3)]) +
  theme(legend.position = 'bottom')+
  xlim(c(-0.35, 0.3))

IR_NIG_plot <-
  ggplot(combined, aes(x = IR_return)) + 
  geom_histogram(aes(y = ..density.., fill = 'Histogram'), colour = 'black') +
  #geom_line(aes(x = IR_return, colour = 'Empirical dist'), adjust = 1, lwd = 1.5, stat="density", lty = 5) +
  #geom_line(data = data_for_plot_sim, aes(x = IR, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  geom_line(data = data_for_plot_sim, aes(x = IR, colour = 'Fitted normal dist'), lwd = 1.5, stat="density", lty = 5)+
  geom_line(data = data_for_plot_sim, aes(x = IR_NIG, colour = 'Fitted NIG dist'), lwd = 1.5, stat="density", lty = 5)+
  xlab('IR quarterly returns')+
  scale_fill_manual('',values = gg_color_hue(4)[2]) +
  scale_color_manual('',values = gg_color_hue(4)[c(4,3)])+
  theme(legend.position = 'bottom') +
  xlim(c(-0.6, 0.75))

png('SP500_IR_fitted_NIG.png', width = 1920*2, height = 1080*2,res = 200*2)
grid.arrange(SP_NIG_plot,IR_NIG_plot, ncol = 2)
dev.off()




ggplot(plot_df) + 
  stat_density2d(aes(x = simulated_IR_n, y = simulated_SP500_n, fill = ..level..), geom = 'polygon', colour = 'black', bins = 60) +
  ylim(-5,5) + xlim(-5,5) + 
  #geom_point(aes(x = simulated_IR_n, y = simulated_SP500_n), col = 'black', alpha = 0.3) + 
  geom_point(data = combined, aes(x = IR_return_n, y = SP500_return_n), pch = 19, col = 'red', alpha = 0.3)



# 3D density plot
library(rgl); library(MASS)
open3d()
mfrow3d(1, 2)
den3d_emp <- kde2d(combined$SP500_return, combined$IR_return)
persp3d(den3d_emp,col="lightblue", box = T, ticktype = 'detailed',xlim=c(-0.2,0.2),ylim =c(-0.2,0.2), zlim = c(0,30))
next3d()
den3d_sim <- kde2d(simulated_IR, simulated_SP500)
persp3d(den3d_sim,col="chartreuse2",add = F,xlim=c(-0.2,0.2),ylim =c(-0.2,0.2), zlim = c(0,30))

open3d()
mfrow3d(1, 2)
den3d_emp <- kde2d(combined$SP500_return, combined$IR_return)
persp3d(den3d_emp,col="lightblue", box = T, ticktype = 'detailed',xlim=c(-0.35,0.35),ylim =c(-0.35,0.35), zlim = c(0,30))
next3d()
den3d_sim <- kde2d(multivariat_normal_sim$SP500_return, multivariat_normal_sim$IR_return)
persp3d(den3d_sim,col="chartreuse2",add = F,xlim=c(-0.35,0.35),ylim =c(-0.35,0.35), zlim = c(0,30))
