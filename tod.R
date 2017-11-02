#time of day screening
#use function: obs.rank (create)
#level set plot for TOD (alpha = 0.05)
#-------------------------------
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
alpha <- 0.05


#create separate data sets based on the setting "Line 5-9"
#generate data: 0-2, 2-4, 4-6,... 20-22, 22-24
rank.time.dt(data = tod, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- NULL
for (i in 1:12) {out <- rbind(out, get(paste(tag[i], ".lvl", sep = "")))}



hline.dat <- data.frame(shift12 = out$time, hl = out$thres)

point.dat <- data.frame(rbind(s1[which(s1$p>=out$thres[1]), ],
                              s2[which(s2$p>=out$thres[2]), ],
                              s3[which(s3$p>=out$thres[3]), ],
                              s4[which(s4$p>=out$thres[4]), ],
                              s5[which(s5$p>=out$thres[5]), ],
                              s6[which(s6$p>=out$thres[6]), ],
                              s7[which(s7$p>=out$thres[7]), ],
                              s8[which(s8$p>=out$thres[8]), ],
                              s9[which(s9$p>=out$thres[9]), ],
                              s10[which(s10$p>=out$thres[10]), ],
                              s11[which(s11$p>=out$thres[11]), ],
                              s12[which(s12$p>=out$thres[12]), ]))

ggplot(data = tod, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(shift12~.)


#tests for surveillance plot setting for TOD data
#(size of alpha vs. cumulative % crashes: largest -> smallest, in rank order)
#----------------------------------------------------------------------------
#time <- "shift12"
#tod <- obs.rank(2015, 71, time)
#tag <- c(paste("s", 1:12, sep = ""))
#alpha <- 0.05

out1 <- NULL
for (var in tag)
{out1 <- rbind(out1, get(var))}

pair.data(data = out1, time = time, 
          tag = tag, var = "p", alpha = 0.05)

#ks test results for surveillance plot (00-02 ~ 22-24):
# same:
#  06-08 - 08-10
#  08-10 - 10-12, 18-20
#  10-12 - 18-20
#  12-14 - 14-16
#  14-16 - 16-18
# diff: (the rest)
#
#may suggest 4-hour period
#-------------------------


#level set setting for TOD data (% crashes vs. size of alpha)
#------------------------------------------------------------
#tag <- c(paste("s", 1:12, sep = ""))
#step 1: create a list of level set results given "tag"
alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
out2 <- NULL
for (var in tag) {out2 <- rbind(out2, get(paste(var, ".lvl", sep = "")))}

#step 2: rename tag to .lvl data
#compare every combination of .lvl data
tag <- c(paste("s", 1:12, ".lvl", sep = ""))
pair.data(data = data.frame(out2), time = "time", 
          tag = tag, var = "pct.events", alpha = 0.05)

#all different
#-------------------------------------------------


#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. 00-02 - 02-04, 00-02 - 04-06, 00-02 - 04-06, ...)
#-------------------------------------------------------

alpha <- 0.05
tag <- c(paste("s", 1:12, sep = ""))
lvl.set.time(tag = tag, alpha = alpha)
out3 <- NULL
for (var in tag) {out3 <- rbind(out2, get(paste(var, ".lvl", sep = "")))}
pair.match(tag = tag, alpha = alpha)