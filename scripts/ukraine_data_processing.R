# R script for data processing and creating data-informed initial adjacency matrices. 

# Packages: geosphere, scales, cdfquantreg, matrixcalc, DMwR, igraph, sf, spdep, dplyr

# Import raw data and noisy latitude and longitude coordinates.

ukraine_data <- read.csv("ukraine_data_all.csv")
med_noise <- read.csv("noisy_coord.csv")
ukraine_data_med <- subset(ukraine_data,select=c("nr_2012","oblast1","loc_name1","eu_support_12","eu_support_15","discuss_freq1"))
ukraine_data_med$med_long = med_noise$longit_noiseUM2
ukraine_data_med$med_lat = med_noise$latid_noiseUM2

# Impute missing data from political discussion frequency. 

ukraine_data$discuss_freq1[is.na(ukraine_data$discuss_freq1)] <- mean(ukraine_data$discuss_freq1, na.rm = TRUE)

ukraine_data_med$discuss_freq1[is.na(ukraine_data_med$discuss_freq1)] <- mean(ukraine_data_med$discuss_freq1, na.rm = TRUE)

# Impute missing data from EU support (2012) with most common opinion in the oblast (ob.). 

#134 (ob. 11)

ob11 <- ukraine_data_med[which(ukraine_data_med$oblast1=="11"),]

# table(ob11$eu_support_12)
# 
# 0  1  2 
# 10 30 72 

ukraine_data$eu_support_12[134] <- 2
ukraine_data_med$eu_support_12[134] <- 2

#517 (ob. 19)

ob19 <- ukraine_data_med[which(ukraine_data_med$oblast1=="19"),]

# table(ob19$eu_support_12)
# 
# 0  1  2 
# 6 22 18 

ukraine_data$eu_support_12[517] <- 1
ukraine_data_med$eu_support_12[517] <- 1

#723, 724, 725 (ob. 16)

ob16 <- ukraine_data_med[which(ukraine_data_med$oblast1=="16"),]

# table(ob16$eu_support_12)
# 
# 0  1  2 
# 30 38 21 

ukraine_data$eu_support_12[723] <- 1
ukraine_data_med$eu_support_12[723] <- 1
ukraine_data$eu_support_12[724] <- 1
ukraine_data_med$eu_support_12[724] <- 1
ukraine_data$eu_support_12[725] <- 1
ukraine_data_med$eu_support_12[725] <- 1

#1111 (ob. 21)

ob21 <- ukraine_data_med[which(ukraine_data_med$oblast1=="21"),]

# table(ob21$eu_support_12)
# 
# 0  1  2 
# 24 54 32

ukraine_data$eu_support_12[1111] <- 1
ukraine_data_med$eu_support_12[1111] <- 1

#1361 (ob. 13)

ob13 <- ukraine_data_med[which(ukraine_data_med$oblast1=="13"),]

# table(ob13$eu_support_12)
# 
# 0  1  2 
# 27 35 28

ukraine_data$eu_support_12[1361] <- 1
ukraine_data_med$eu_support_12[1361] <- 1

# Remove problematic localities (e.g. couldn't re-interview respondents due to annexation, etc.). 

ukraine_data <- filter(ukraine_data, !(loc_name1 %in% c("70","71","72","73","74","75","76","77","121","124","127","128","104","105","106","107","109")))
ukraine_data_med <- filter(ukraine_data_med, !(loc_name1 %in% c("70","71","72","73","74","75","76","77","121","124","127","128","104","105","106","107","109")))

# Find geographical distances between individuals using the Haversine formula (i.e. great circle distance). 

coords_med <- ukraine_data_med[,c("med_long","med_lat")] #(longitude,latitude) coordinates
dists_med <- distm(coords_med,fun=distHaversine)
dists_med <- dists_med * (1/1000) #convert to km from meters

dists_ordered_med <- t(apply(as.data.frame(dists_med),1,order))
dists_ordered_med <- as.data.frame(dists_ordered_med)
dists_ordered_med <- subset(dists_ordered_med,select=-c(V1))

dists_ordered_med <- dists_ordered_med[1:1522,1:500]

# Re-scale political discussion frequencies to (0,1). 

ukraine_data_med$discuss_freq1 <- scaleTR(ukraine_data_med$discuss_freq1,high=max(ukraine_data_med$discuss_freq1),low=min(ukraine_data_med$discuss_freq1),N=1522,scale=0.5)

# Distribution of talkativeness (i.e. discussion frequency). 

discuss_freq <- ukraine_data$discuss_freq1
#hist(discuss_freq)

# Re-scale discussion frequency to reflect the desired range of initial connections. 

k_from_discuss <- rescale(discuss_freq, to = c(7,22))
k_from_discuss_int <- round(k_from_discuss)

hist(k_from_discuss_int)

discuss_freq <- as.data.frame(discuss_freq)
k_from_discuss <- as.data.frame(k_from_discuss)
k_from_discuss_int <- as.data.frame(k_from_discuss_int)

# Assign a number of nearest and long-range connections. 

k_nearest <- round(1*(k_from_discuss_int))
long_conn <- round(k_from_discuss_int-k_nearest)

# Connect to a subset of your k nearest neighbors (proportional to k) who have not reached their maximum k. 

A_initial <- matrix(0, ncol = 1522, nrow = 1522)
A_initial <- data.frame(A_initial)

for (i in 1:1522)
{
  rdmz <- sample.int(1522)
  cur <- rdmz[i]
  k_ind <- k_nearest$k_from_discuss_int[cur]
  k_neigh <- dists_ordered_med[cur,]
  numconn <- sum(A_initial[cur,])
  if (numconn == k_ind)
  {
    next
  }
  else for (j in 1:500)
  {
    neigh <- as.numeric(k_neigh[j])
    if (sum(A_initial[neigh,]) < k_nearest$k_from_discuss_int[neigh])
    {
      A_initial[cur,neigh] <- 1
      A_initial[neigh,cur] <- 1
      numconn <- numconn + 1
    }
    if (numconn == k_ind)
    {
      break
    }
  }
}

# Add longer-range connections with a 1/d link length distribution.

for (k in 1:1522)
{
  longer <- which(dists_med[k,] > 15)
  conns <- sum(A_initial[k,])
  if (conns == k_from_discuss_int$k_from_discuss_int[k])
  {
    next
  }
  else for (l in 1:1000)
  {
    long_neigh <- longer[l]
    if (A_initial[k,long_neigh] == 1)
    {
      next
    }
    if ((k_from_discuss_int$k_from_discuss_int[long_neigh] - sum(A_initial[long_neigh,])) < 5)
    {
      if (runif(1) < 2*((dists_med[k,long_neigh]))^(-1))
      {
        A_initial[k,long_neigh] <- 1
        A_initial[long_neigh,k] <- 1
        conns <- conns + 1
      }
    }
    if (conns == k_from_discuss_int$k_from_discuss_int[k])
    {
      break
    }
  }
}

A_i = as.matrix(A_initial)
diag(A_i) <- 0
# A_i <- A_i + t(A_i)

totcon <- matrix(0,ncol=1,nrow=1522)
totcon <- data.frame(totcon)

for (i in 1:1522)
{
  totcon[i,] = sum(A_i[i,])
}

# Look at key network metrics. 

print(sprintf("min: %i",min(totcon)))
print(sprintf("max: %i",max(totcon)))
print(sprintf("mean: %f",mean(totcon$totcon)))
print(sprintf("cor: %f",cor(ukraine_data$discuss_freq1,totcon)))

# Look at link length distribution.

g <- graph_from_adjacency_matrix(A_i)

print(sprintf("local transitivity: %f",transitivity(g,"undirected","local")))
print(sprintf("average path length: %f",average.path.length(g,directed=FALSE)))

E <- as_edgelist(g,names=FALSE)
E <- as.data.frame(E)
edgedist <- matrix(0,ncol=1,nrow=nrow(E))
edgedist <- data.frame(edgedist)

for (k in 1:nrow(E))
{
  edgedist$edgedist[k] <- dists_med[E$V1[k],E$V2[k]]
}

hist(edgedist$edgedist)

# Create data to input into Matlab. 

# ukraine_data_m <- subset(ukraine_data_med,select=c("nr_2012","eu_support_12","eu_support_15"))

# Write data to csv. 

#write.csv(ukraine_data_m,"/Users/OJC 1/Dropbox/Political Polarization Workshop Summer 2020/Code for revisions/ukraine_data_m.csv", row.names = FALSE)
#write.csv(A_i,"/Users/OJC 1/Dropbox/Political Polarization Workshop Summer 2020/Code for revisions/adj_init.csv", row.names = FALSE)
