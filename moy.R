#function.path = "D:/caps/function_code.R"
#source(file = function.path)
load("moy.RData")
#mon of year screening
#use function: obs.rank (create)
#level set plot for moy (alpha = 0.05)
#-------------------------------
time <- "Month"
month <- obs.rank(2015, 71, time)
tag <- c("jan","feb","mar","apr","may","jun","jul", "aug",
         "sep","oct","nov","dec")
alpha <- 0.05


#create separate data sets based on the setting "Line 8-12"
#generate data: jan ~ dec
rank.time.dt(data = month, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(jan.lvl, feb.lvl, mar.lvl, apr.lvl, may.lvl, jun.lvl, 
             jul.lvl, aug.lvl, sep.lvl, oct.lvl, nov.lvl, dec.lvl)



hline.dat <- data.frame(Month = out$time, hl = out$thres)

point.dat <- data.frame(rbind(jan[which(jan$p>=out$thres[1]), ],
                              feb[which(feb$p>=out$thres[2]), ],
                              mar[which(mar$p>=out$thres[3]), ],
                              apr[which(apr$p>=out$thres[4]), ],
                              may[which(may$p>=out$thres[5]), ],
                              jun[which(jun$p>=out$thres[6]), ],
                              jul[which(jul$p>=out$thres[7]), ],
                              aug[which(aug$p>=out$thres[8]), ],
                              sep[which(sep$p>=out$thres[9]), ],
                              oct[which(oct$p>=out$thres[10]), ],
                              nov[which(nov$p>=out$thres[11]), ],
                              dec[which(dec$p>=out$thres[12]), ]))

ggplot(data = month, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(Month~.)



#KS tests: on level sets & rank for MOY data (% crashes vs. size of alpha)
#------------------------------------------------------------------------
#tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#         "sep","oct","nov","dec")

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
ks1 <- origin.pair(ref = tag, compare = tag, sig = 0.3, opt = FALSE)
#------------------------------------------------------------------------




#segments binnings with size 1 and 0.5
#-------------------------------------------------------------
time <- "Month"
tag <- c("jan","feb","mar","apr","may","jun","jul","aug", 
         "sep","oct","nov","dec")
xlim <- c(0, ceiling(max(month$Logmile))+1)

#binnings using level sets with size 1
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.moybin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#binnings using level sets with size 0.5
#-------------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))
ks.moybin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-------------------------------------------------------------



#use "may", "jun", "jul" as reference to test other days
#test % matched segments under level sets
#--------------------------------------------------------
tag <- c("jan","feb","mar","apr","may","jun","jul","aug", 
         "sep","oct","nov","dec")
rank.time.dt(data = month, tag = tag, time = time)
lvl.set.time(tag = tag, alpha = alpha)

moy5.mod <- data2pred(ref = tag[5], compare = tag[-5])
moy6.mod <- data2pred(ref = tag[6], compare = tag[-6])
moy7.mod <- data2pred(ref = tag[7], compare = tag[-7])

par(mfrow=c(2,2),family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "May", xlab = expression(alpha), ylab = "Matched rate")
matlines(x = moy5.mod$alpha, y = moy5.mod[,-1], col = 1:11, type = "s")
box(which = "plot")
legend("bottomright", tag[-5], col = 1:11, lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Jun", xlab = expression(alpha), ylab = "Matched rate")
matlines(x = moy6.mod$alpha, y = moy6.mod[,-1], col = 1:11, type = "s")
box(which = "plot")
legend("bottomright", tag[-6], col = 1:11, lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Jul", xlab = expression(alpha), ylab = "Matched rate")
matlines(x = moy7.mod$alpha, y = moy7.mod[,-1], col = 1:11, type = "s")
box(which = "plot")
legend("bottomright", tag[-7], col = 1:11, lty = 1, bty = "n")
#-------------------------------------------------------------





#test % matched segments under level sets
#size 1, "aug" as model ----------------------------------------------------
tag <- c("jan","feb","mar","apr","may","jun","jul","aug", 
         "sep","oct","nov","dec")
xlim <- c(0, ceiling(max(month$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
dow8.mod <- data2pred(ref = tag[8], compare = tag[-8], var = "breaks")

par(mfrow=c(1,2),family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 8)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("dow",i,".mod",sep=""))$alpha,
           y = get(paste("dow",i,".mod",sep=""))[,-1], col = 1:11, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:11, lty = 1, bty = "n") 
}

#size 0.5, "aug" as model --------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)

lvl.set.time(tag = tag, alpha = alpha)
dow8.mod <- data2pred(ref = tag[8], compare = tag[-8], var = "breaks")

for (i in 8)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste("dow",i,".mod",sep=""))$alpha,
           y = get(paste("dow",i,".mod",sep=""))[,-1], col = 1:11, type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], col = 1:11, lty = 1, bty = "n") 
}