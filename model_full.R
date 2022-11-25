# Clear workspace
rm(list=ls())

# Set working directory to top-level folder containing DMC
# setwd("~/DMC")
# setwd("D:/ATC experiments/Exp5 TP-PM/Analysis/DMC")

# Load libraries and DMC functions
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")


# Load data
print(load("Data/ATC_DOMS-PM_clean.RData"))
head(cleandats)
dat <- cleandats[,c("s","S","DOMS","PM","R","RT")]
# names(dat)[c(1)] <- c("subjects")
levels(dat$R)
levels(dat$S)
head(dat)
str(dat)
# NB: RT has been truncated below 0.20s

# Rename PM factor levels
levels(dat$PM)
dat$PM <- factor(dat$PM, levels = c("Easy", "Hard"), labels = c("E", "H"))

# Rename PM factor levels
levels(dat$DOMS)
dat$DOMS <- factor(dat$DOMS, levels = c("Easy", "Hard"), labels = c("e", "h"))

# Check factor levels
lapply(dat, levels)

# 1600 trials per subject
table(dat$s)


# Create match maps -------------------------------------------------------

expand.grid(list(S = c("cc","nn","pc","pn"),
                 DOMS = c("e","h"),
                 PM = c("E","H"),
                 R = c("C","N","P")))

# Mean v map
map_v <- empty.map(
  
  list(S = c("cc","nn","pc","pn"),
       DOMS = c("e","h"),
       PM = c("E","H"),
       R = c("C","N","P")),
  
  levels = c("cceEC","nneEC","pceEC","pneEC",
             "cchEC","nnhEC","pchEC","pnhEC",
             "cceHC","nneHC","pceHC","pneHC",
             "cchHC","nnhHC","pchHC","pnhHC",
             
             "cceEN","nneEN","pceEN","pneEN",
             "cchEN","nnhEN","pchEN","pnhEN",
             "cceHN","nneHN","pceHN","pneHN",
             "cchHN","nnhHN","pchHN","pnhHN",
             
             "ppeEP",
             "pphEP",
             "ppeHP",
             "pphHP",
             
             "PMFA"))
map_v
length(map_v)
length(levels(map_v))

map_v[1:48] <- c("cceEC","nneEC","pceEC","pneEC",
                 "cchEC","nnhEC","pchEC","pnhEC",
                 "cceHC","nneHC","pceHC","pneHC",
                 "cchHC","nnhHC","pchHC","pnhHC",
                 
                 "cceEN","nneEN","pceEN","pneEN",
                 "cchEN","nnhEN","pchEN","pnhEN",
                 "cceHN","nneHN","pceHN","pneHN",
                 "cchHN","nnhHN","pchHN","pnhHN",
                 
                 "PMFA","PMFA","ppeEP","ppeEP",
                 "PMFA","PMFA","pphEP","pphEP",
                 "PMFA","PMFA","ppeHP","ppeHP",
                 "PMFA","PMFA","pphHP","pphHP")
map_v


# Threshold map
map_B <- empty.map(
  
  list(S = c("cc","nn","pc","pn"),
       DOMS = c("e","h"),
       PM = c("E","H"),
       R = c("C","N","P")),
  
  levels = c("eEC",
             "eHC",
             "hEC",
             "hHC",
             
             "eEN",
             "eHN",
             "hEN",
             "hHN",
             
             "eEP",
             "eHP",
             "hEP",
             "hHP"))
map_B
length(map_B)
length(levels(map_B))

map_B[1:48] <- c("eEC","eEC","eEC","eEC",
                 "hEC","hEC","hEC","hEC",
                 "eHC","eHC","eHC","eHC",
                 "hHC","hHC","hHC","hHC",
                 
                 "eEN","eEN","eEN","eEN",
                 "hEN","hEN","hEN","hEN",
                 "eHN","eHN","eHN","eHN",
                 "hHN","hHN","hHN","hHN",
                 
                 "eEP","eEP","eEP","eEP",
                 "hEP","hEP","hEP","hEP",
                 "eHP","eHP","eHP","eHP",
                 "hHP","hHP","hHP","hHP")
map_B



# Build model -------------------------------------------------------------

model <- model.dmc(
  p.map = list(
    A = "1",
    B = c("MAPB"),
    t0 = "1",
    mean_v = c("MAPV"),
    sd_v = "1",
    st0 = "1",
    N = "PM"), 
  match.map = list(
    M = list(
      cc = "C", 
      nn = "N", 
      pc = "P", 
      pn = "P"),
    MAPB = map_B,
    MAPV = map_v),
  factors = list(
    S = c("cc", "nn", "pc", "pn"),
    DOMS = c("e", "h"),
    PM = c("E", "H")),
  constants = c(N.E = 3, N.H = 3, 
                st0 = 0,
                sd_v = 0.5), 
  responses = c("C","N","P"),
  type = "normN")


# Create parameter vector
p.vector <- c(t0 = 0.3, A = 1.5,
              
              B.eEC = 2,  B.eHC = 2,  B.hEC = 2,  B.hHC = 2,  
              B.eEN = 2,  B.eHN = 2,  B.hEN = 2,  B.hHN = 2, 
              B.eEP = 2,  B.eHP = 2,  B.hEP = 2,  B.hHP = 2,
              
              mean_v.cceEC = 1, mean_v.nneEC = 0, mean_v.pceEC = 0, mean_v.pneEC = 0, 
              mean_v.cchEC = 1, mean_v.nnhEC = 0, mean_v.pchEC = 0, mean_v.pnhEC = 0,
              mean_v.cceHC = 1, mean_v.nneHC = 0, mean_v.pceHC = 0, mean_v.pneHC = 0, 
              mean_v.cchHC = 1, mean_v.nnhHC = 0, mean_v.pchHC = 0, mean_v.pnhHC = 0,
              mean_v.cceEN = 0, mean_v.nneEN = 1, mean_v.pceEN = 0, mean_v.pneEN = 0,
              mean_v.cchEN = 0, mean_v.nnhEN = 1, mean_v.pchEN = 0, mean_v.pnhEN = 0,
              mean_v.cceHN = 0, mean_v.nneHN = 1, mean_v.pceHN = 0, mean_v.pneHN = 0,
              mean_v.cchHN = 0, mean_v.nnhHN = 1, mean_v.pchHN = 0, mean_v.pnhHN = 0,
              mean_v.ppeEP = 1, mean_v.pphEP = 1, mean_v.ppeHP = 1, mean_v.pphHP = 1,
              mean_v.PMFA = 0)
length(p.vector)

# Check parameter vector matches model
check.p.vector(p.vector, model)

# Check model simulates
simulate.dmc(p.vector, model)

# Set priors
p.prior <- prior.p.dmc(
  dists = c("beta", rep("tnorm", length(p.vector)-1)),
  p1 = c(p.vector),                           
  p2 = c(0.2, 0.1, rep(1, 12), rep(1.5, 37)), 
  lower = c(0.1, 0, rep(0, 12), rep(NA, 37)),
  upper = c(1, 10, rep(Inf, 12), rep(Inf, 37))
)
length(p.prior)
length(p.vector)

# Plot priors
# par(mfcol = c(2, 4)); for (i in names(p.prior)) plot.prior(i, p.prior)


# Make data model
dm <- data.model.dmc(dat, model)
save(dm, file = "samples/dmDOMSPM_full.RData")


# Initialize samples object
n.chains <- length(p.prior) * 3

# Generate start points for fixed effects model
samples <- h.samples.dmc(nmc = 100, 
                         p.prior = p.prior, 
                         data = dm, 
                         thin = 10, 
                         n.chains = n.chains)

# Save
save(samples, file = "samples/sDOMSPM_full.RData")

# Load
print(load("samples/sDOMSPM_full.RData"))

# -------------------------------------------------------------------------
# 
# # Generate start points for hierarchical model
# # Hyper-level
# hstart <- make.hstart(samples)
# 
# # Subject-level
# theta <- make.theta1(samples)
# 
# 
# # Hyper-level priors
# # Mu
# mu.prior <- prior.p.dmc(
#   dists = rep("tnorm", 51),
#   p1 = c(t0 = 1, p.vector[-1]),
#   p2 = c(1, 1, rep(1, 12), rep(1.5, 37)), 
#   lower = c(0.1, 0, rep(0, 12), rep(NA, 37)),
#   upper = c(1, 10, rep(Inf, length(p.vector)-2))
# )
# 
# # Sigma
# sigma.prior <- prior.p.dmc(
#   dists = c(rep("gamma", 51)), 
#   p1 = c(1, 1, rep(1, 12), rep(1.5, 37)),                          
#   p2 = c(rep(1, 51))
# )
# 
# # Create hyper prior object
# pp.prior <- list(mu.prior, sigma.prior)
# 
# # Initialize samples object
# n.chains <- length(p.prior) * 3
# 
# # Generate start points for hierarchical model
# hsamples <- h.samples.dmc(nmc = 100, 
#                           p.prior = p.prior, 
#                           data = dm, 
#                           pp.prior = pp.prior, 
#                           thin = 10,
#                           hstart.prior = hstart, 
#                           theta1 = theta, 
#                           n.chains = n.chains
# )
# 
# # Save
# save(hsamples, file = "dmc/samples/hsDOMSPM_full.RData")