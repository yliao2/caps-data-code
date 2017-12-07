function.path = "function_code.R"
source(file = function.path)

#level set setting (weekend & weekdays)
#--------------------------------------
z <- obs.rank(2015, 71, "Weekend")
z1 <- z %>% filter(Weekend==1) %>% arrange(rank)
z2 <- z %>% filter(Weekend==2) %>% arrange(rank)



#single alpha = 0.05
#-------------------
alpha = 0.05
t1 <- data.frame(lvl.set(z1, alpha))
t2 <- data.frame(lvl.set(z2, alpha))

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    mfrow = c(1, 2), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, max(z$p)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(a)",xlab="Logmile", ylab="Crash rate", cex = 0.7)
segments(x0 = z1$Logmile, y0 = z1$p, y1 = 0)
points(x = z1[which(z1$p>=t1$thres), ]$Logmile, y = z1[which(z1$p>=t1$thres), ]$p, cex = 0.8)
abline(h = t1$thres, col = "darkgray")
box(which = "plot")
text(x = 32, y = t1$thres, labels = round(t1$thres, 4), pos = 3)

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, max(z$p)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(b)", xlab="Logmile", ylab="Crash rate", cex = 0.7)
segments(x0 = z2$Logmile, y0 = z2$p, y1 = 0)
points(x = z2[which(z2$p>=t2$thres), ]$Logmile, y = z2[which(z2$p>=t2$thres), ]$p, cex = 0.8)
abline(h = t2$thres, col = "darkgray")
box(which = "plot")
text(x = 32, y = t2$thres, labels = round(t2$thres, 4), pos = 3)

fig.des <- expression(paste("Figure 2.1: weekdays (a) and weekend (b) with ", alpha, " = 0.05", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#sequence of alphas (alpha = seq(0, 1, length = 101))
#==========================================================
#-----------fix allocation based surveillance-------------#
#----------------------------------------------------------
#surveillance plot (type 1) (size of distance vs. % events)
#----------------------------------------------------------
z3 <- z1 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()
z4 <- z2 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()

plot(x = cumsum(z3$log.size), y = cumsum(z3$log.rate), type = "s", 
     xlab = "size of distance", ylab = "%crashes")
lines(x = cumsum(z4$log.size), y = cumsum(z4$log.rate), type = "s", col = 4)

h <- 0.18*sum(z3$log.size)
abline(v = h, col = "darkgrey")

p1 <- max(cumsum(z3[which(cumsum(z3$log.size)<=h),]$log.rate))
p2 <- max(cumsum(z4[which(cumsum(z4$log.size)<=h),]$log.rate))
text(x = c(h, h), y = c(p1, p2), labels = round(c(p1, p2),4), pos = 2, col = c(1, 4))

#surveillance plot (type 2) (level alpha vs. % events in HS)
#-----------------------------------------------------------
alpha <- seq(0, 1, by = 0.01)
z1.lvl <- data.frame(lvl.set(z1, alpha))
z2.lvl <- data.frame(lvl.set(z2, alpha))

thres <- 0.18

plot(x = z1.lvl$alpha, y = z1.lvl$pct.events, type = "s", 
     xlab = expression(alpha), ylab = "%events in HS")
lines(x = z2.lvl$alpha, y = z2.lvl$pct.events, type = "s", col = 4)
abline(v = thres, col = "darkgrey")
p3 <- z1.lvl[alpha == thres,]$pct.events
p4 <- z2.lvl[alpha == thres,]$pct.events
text(x = c(thres, thres), y = c(p3, p4), labels = round(c(p3, p4),4), 
     pos = 3, col = c(1, 4))
legend("bottomright", c("wk", "wkd"), col = c(1, 4), lty = 1)



#--------------------------
#markdown figure format use
#--------------------------
par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    mfrow = c(1, 2), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(a)",xlab="Size of Logmile", ylab="Crash rate", cex = 0.7)
lines(x = cumsum(z3$log.size), y = cumsum(z3$log.rate), type = "s")
lines(x = cumsum(z4$log.size), y = cumsum(z4$log.rate), type = "s", col = 4)
abline(v = h, col = "darkgrey")
box(which = "plot")
text(x = h, y = c(p1, p2, 0), labels = round(c(p1, p2, h), 4), pos = 4, col = c(1, 4, 1))
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(main="(b)",xlab=expression(alpha), ylab="Crash rate", cex = 0.7)
lines(x = z1.lvl$alpha, y = z1.lvl$pct.events, type = "s")
lines(x = z2.lvl$alpha, y = z2.lvl$pct.events, type = "s", col = 4)
abline(v = 0.18, col = "darkgrey")
box(which = "plot")
text(x = thres, y = c(p3, p4, 0), labels = round(c(p3, p4, thres),4), pos = 4, col = c(1, 4, 1))
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")

fig.des <- expression(paste("Figure 2.2: surveillance plots for weekdays and weekend under distinct perspectives ", alpha, " levels", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#-------------threshold based surveillance----------------#
#----------------------------------------------------------
u <- unique(z$p)
thr <- seq(0, round(max(u)+0.005, 2), by = 0.0001)

z1.thres <- thres.out(data = z1, var = "p", thr = thr)
z2.thres <- thres.out(data = z2, var = "p", thr = thr)

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = rev(c(0, max(thr))), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(xlab="Threshold", ylab=expression(alpha), cex = 0.7)
lines(x = thr, y = z1.thres$size, type = "s")
lines(x = thr, y = z2.thres$size, type = "s", col = 4)

v <- 0.0019
abline(v = v, col = "darkgrey")
box(which = "plot")
p5 <- z1.thres[which(z1.thres$thr==v),]$size
p6 <- z2.thres[which(z2.thres$thr==v),]$size
text(x = v, y = c(p5+0.01, p6+0.04, 0), labels = round(c(p5, p6, v), 4), pos = 2, col = c(1, 4, 1))
legend("topleft", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")
mtext("Figure 2.3: threshold based surveillance for weekdays and weekend", 
      side = 1, adj = 0.5, outer = TRUE)


#ks test for surveillance plot (not significant)
#-----------------------------------------------
t <- ks.results(s1 = z1.lvl$pct.events, s2 = z2.lvl$pct.events, alpha = 0.05)
max.d.pt <- data.frame(cbind(alpha, d = abs(t$d))) %>% filter(d == t$max.d)

plot(x = alpha, y = abs(t$d), xlab = expression(alpha), ylab = "distance")
abline(v = max.d.pt$alpha, h = 0, col = "darkgray")

#percentage of crashes in a size/alpha (%) of highest crash rate segments
#ex. alpha = 0.3, proportion of segments id as HS is around 30%, 100% of
#    total crashes happened in these segments during weekend, where as
#    0.9433 of total crashes happened in these segments during weekdays
#
#ks test shows no significance between distributions for weekend & weekdays
#max distance between two distributions happends when alpha = 0.11,
#with max(D) = 0.0851
#--------------------------------------------------------------------------




#proportion/size of highest segements id as HS vs. threshold
#-----------------------------------------------------------
#z1.lvl: level set setting for weekdays (alpha/size, thres, ...)
#z2.lvl: level set setting for weekend (alpha/size, thres, ...)

plot(x = z1.lvl$thres, y = z1.lvl$level, type = "s", 
     xlab = "threshold", ylab = "%segments id as HS", 
     xlim = c(0, max(z1.lvl$thres, z2.lvl$thres)))
lines(x = z2.lvl$thres, y = z2.lvl$level, type = "s", col = 4)
abline(v = 0.007, col = "darkgrey")
text(x = c(0.007, 0.007), y = c(z1.lvl[alpha == 0.01,]$level, z2.lvl[alpha == 0.01,]$level),
     labels = round(c(z1.lvl[alpha == 0.01,]$level, z2.lvl[alpha == 0.01,]$level),4), 
     col = c(1, 4), pos = c(1,3))
legend("topright", c("wk", "wkd"), col = c(1, 4), lty = 1)

#ex. under the same threshold = 0.007, % of segments id as HS (true level)
#    is 0.0098 for weekdays, whereas % of segments id as HS is 0.0082
#    for weekend
#-------------------------------------------------------------------------







#% segments id HS are matched for weekdays & weekend
#---------------------------------------------------
#z1: crash summary weekend==1 in ordered rank of %crashes (weekdays)
#z2: crash summary weekend==2 in ordered rank of %crashes (weekend)
#z : crash summary for both (rbind(z1,z2))
#z1.lvl: level set setting for weekdays (alpha/size, thres, ...)
#z2.lvl: level set setting for weekend (alpha/size, thres, ...)

m1 <- NULL
  for (i in 1:nrow(z1.lvl))
{
  max <- length(unique(c(z1[1:z1.lvl$nseg[i],]$Logmile, 
                         z2[1:z2.lvl$nseg[i],]$Logmile)))
  
  cond <- z1[1:z1.lvl$nseg[i],]$Logmile %in% 
          z2[1:z2.lvl$nseg[i],]$Logmile
  
  m1 <- c(m1, sum(cond)/max)
}

plot(x = alpha, y = m1, type = "s", 
     xlab = expression(alpha), ylab = "% matched segments")
abline(v = c(0.1, 0.2), col = "darkgrey")
text(x = c(0.1, 0.2), y = m1[c(11, 21)], labels = round(m1[c(11, 21)],4), pos = 3)


#jaccard index plot
#-------------------------------------------------------------------------
par(family = "serif", cex.axis = 0.7, mar = c(3.5, 2.5, 1, 1), 
    las = 1, oma = c(2, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), ylab = "% matched segments")
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
lines(x = alpha, y = m1, type = "s")
v1 <- c(0.1, 0.2)
abline(v = v1, col = "darkgrey")
box(which = "plot")
text(x = v1, y = m1[v1*length(alpha)+1], 
     labels = round(m1[v1*length(alpha)+1],4), pos = 3)
mtext("Figure 2.4: Jaccard Index for weekdays and weekend sets", 
      side = 1, adj = 0.5, outer = TRUE)

#ex. under the same alpha/size/(%segments id HS) = 0.1, 47.93% of segments
#    for weekdays and weekend are matched. if alpha = 0.2, 59.03% of segments
#    given different time period are matched.
#----------------------------------------------------------------------------





#relative distribution plot (compare to unif)
#--------------------------------------------
#z1: crash summary weekend==1 in ordered rank of %crashes (weekdays)
#z2: crash summary weekend==2 in ordered rank of %crashes (weekend)
#
#r <- z1.lvl$pct.events/z2.lvl$pct.events
#n <- sum(is.finite(r))
#plot(x = alpha, y = r, type = "s", 
#     xlab = expression(alpha), 
#     ylab = "pct crashes (weekdays)/pct crashes (weekend)")
#abline(h = 1, col = "darkgrey")
#
#r1 <- r/sum(r[is.finite(r)])
#r2 <- rep(1, n)/n
#t3 <- ks.results(s1 = cumsum(r1[is.finite(r1)]), s2 = cumsum(r2), alpha = 0.05)
#
#convert to uniform cdf
#----------------------
#plot(x = alpha[is.finite(r)], y = cumsum(r1[is.finite(r1)]), 
#     type = "s", xlab = expression(alpha), ylab = "cumulative ", col = 4)
#lines(x = alpha[is.finite(r)], y = cumsum(r2), type = "s")
#abline(v = seq(0, 1, by = 0.2), h = seq(0, 1, by = 0.2), col = "lightgrey")
#
#construct Confidance band
#-------------------------
#c.alpha <- sqrt(-0.5*log(0.05/2)) 
#bound <- c.alpha*sqrt(1/100+1/100)
#low <- cumsum(r1[is.finite(r1)])-bound
#up <- cumsum(r1[is.finite(r1)])+bound
#low[low<0]=0
#up[up>1]=1
#lines(x = alpha[is.finite(r)], y = low, type = "s", col = 2)
#lines(x = alpha[is.finite(r)], y = up, type = "s", col = 2)

#difference (in distributions) vs. alpha
#and confidence band
#---------------------------------------
#ds <- cumsum(r1[is.finite(r1)])-cumsum(r2)
#plot(x = alpha[is.finite(r)], y = ds, cex = 0, xlab = expression(alpha),
#     ylab = "distance in distribution", ylim = c(-1, 1))
#segments(x0 = alpha[is.finite(r)], y0 = ds, y1 = 0)
#lines(x = alpha[is.finite(r)], y = rep(bound, 100), type = "s", col = 2)
#lines(x = alpha[is.finite(r)], y = rep(-bound, 100), type = "s", col = 2)
#
#ex. if the probability of crashes given different time (weekend & weekdays)
#    is the same, r, the ratio of % total events within an alpha/size of segments,
#    should be 1 under the same size/alpha.
#------------------------------------------




#segments binnings
#-----------------------------
xlim <- c(0, ceiling(max(z$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
time <- "Weekend"
tag <- c("wk","wkd")
bin2rank.time(data = z, time = time, tag = tag, brks = brks)

#binnings using level sets
#----------------------------------
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

t3 <- ks.results(s1 = wk.lvl$pct.events, s2 = wkd.lvl$pct.events, alpha = 0.05)

#plot: ordered bins vs. cumulative % crashes 
#------------------------------------------------------

#======================================
#--------   markdown format   ---------
#======================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(wk$rank, wkd$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
lines(x = wk$rank, y = cumsum(wk$p), type = "s")
lines(x = wkd$rank, y = cumsum(wkd$p), type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
lines(x = wk.lvl$alpha, y = wk.lvl$pct.events, type = "s")
lines(x = wkd.lvl$alpha, y = wkd.lvl$pct.events, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lwd = 2, lty = 1)



fig.des <- expression(paste("Figure 3.6: ordered bins with bin size = 1 for weekdays and weekend", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)

#bin size of 0.5
#----------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = z, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

#plot: ordered bins vs. cumulative % crashes 
#------------------------------------------------------

#======================================
#--------   markdown format   ---------
#======================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(wk$rank, wkd$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
lines(x = wk$rank, y = cumsum(wk$p), type = "s")
lines(x = wkd$rank, y = cumsum(wkd$p), type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
lines(x = wk.lvl$alpha, y = wk.lvl$pct.events, type = "s")
lines(x = wkd.lvl$alpha, y = wkd.lvl$pct.events, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.7: ordered bins with bin size = 0.5 for weekdays and weekend", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)




#use data generate from obs.rank fn.
#z1 = weekdays; z2 = weekend
#-----------------------------------
wk.mod <- data2pred(ref = z1, compr = z2, "Logmile")
colnames(wk.mod) <- c("ref.rank", "weekend", "weekend.n")

wkd.mod <- data2pred(ref = z2, compr = z1, "Logmile")
colnames(wkd.mod) <- c("ref.rank", "weekdays", "weekdays.n")

par(family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)
plot.new()
plot.window(xlim = c(1, max(z$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
lines(x = wk.mod$ref.rank, y = wk.mod$weekend)
lines(x = wkd.mod$ref.rank, y = wkd.mod$weekdays, col = 4)
box(which = "plot")
legend("bottomright", c("Weekend", "Weekdays"), col = c(1, 4), lwd = 2)

fig.des <- expression(paste("Figure 3.15: Match rates for weekdays and weekend", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)






#weekdays and weekend with bin size 1
#line:306:310, to get data
#------------------------------------------------
wk.mod <- data2pred(ref = wk, compr = wkd, "breaks")
colnames(wk.mod) <- c("ref.rank", "weekend", "weekend.n")

wkd.mod <- data2pred(ref = wkd, compr = wk, "breaks")
colnames(wkd.mod) <- c("ref.rank", "weekdays", "weekdays.n")

par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(wk$rank, wkd$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
lines(x = wk.mod$ref.rank, y = wk.mod$weekend)
lines(x = wkd.mod$ref.rank, y = wkd.mod$weekdays, col = 4)
box(which = "plot")
legend("bottomright", c("Weekend", "Weekdays"), col = c(1, 4), lwd = 2)

fig.des <- expression(paste("Figure 3.19: Match rates with bin size 1 (left) and 0.5 (right) for weekdays and weekend", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#weekdays and weekend with bin size 0.5
#line:354:356, to get data
#and repeat 426:429
#------------------------------------------------









head(z1,10);head(z2,10)
unique(z1$rank);unique(z2$rank)
a = NULL
for (i in 1:length(unique(z1$rank)))
{
  ref = z1[z1$rank<=i,]$Logmile
  a = c(a, sum(z2[z2$Logmile %in% ref,]$p))
}

b = NULL
for (i in 1:length(unique(z2$rank)))
{
  ref = z2[z2$rank<=i,]$Logmile
  b = c(b, sum(z1[z1$Logmile %in% ref,]$p))
}
b

plot(x = 1:length(unique(z1$rank)), y = a, type = "S")
lines(x = 1:length(unique(z2$rank)), y = b, type = "S", col = 4)
abline(v = c(length(unique(z1$rank)), length(unique(z2$rank))), 
       h = 1, col = 3)
