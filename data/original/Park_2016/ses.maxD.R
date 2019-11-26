# ses.maxD
# code "modified" by Max Farrell (stolen) from Steve Kembel
require(picante)

maxD <- function (samp, dis, abundance.weighted=FALSE) {

    N <- dim(samp)[1]
    maxD <- numeric(N)
    for (i in 1:N) {
        sppInSample <- names(samp[i, samp[i, ] > 0])
        if (length(sppInSample) > 1) {
            sample.dis <- dis[sppInSample, sppInSample]
            maxD[i] <- max(sample.dis)        
        }
        else {
            maxD[i] <- 0
        }
    }
    maxD
}


ses.maxD <- function (samp, dis, null.model = c("taxa.labels", "richness", 
    "frequency", "sample.pool", "phylogeny.pool", "independentswap", 
    "trialswap"), abundance.weighted = FALSE, runs = 999, iterations = 1000) 
{
    dis <- as.matrix(dis)
    maxD.obs <- maxD(samp, dis, abundance.weighted = abundance.weighted)
    null.model <- match.arg(null.model)
    maxD.rand <- switch(null.model, taxa.labels = t(replicate(runs, 
        maxD(samp, taxaShuffle(dis), abundance.weighted = abundance.weighted))), 
        richness = t(replicate(runs, maxD(randomizeMatrix(samp, 
            null.model = "richness"), dis, abundance.weighted))), 
        frequency = t(replicate(runs, maxD(randomizeMatrix(samp, 
            null.model = "frequency"), dis, abundance.weighted))), 
        sample.pool = t(replicate(runs, maxD(randomizeMatrix(samp, 
            null.model = "richness"), dis, abundance.weighted))), 
        phylogeny.pool = t(replicate(runs, maxD(randomizeMatrix(samp, 
            null.model = "richness"), taxaShuffle(dis), abundance.weighted))), 
        independentswap = t(replicate(runs, maxD(randomizeMatrix(samp, 
            null.model = "independentswap", iterations), dis, 
            abundance.weighted))), trialswap = t(replicate(runs, 
            maxD(randomizeMatrix(samp, null.model = "trialswap", 
                iterations), dis, abundance.weighted))))
    maxD.rand.mean <- apply(X = maxD.rand, MARGIN = 2, FUN = mean, 
        na.rm = TRUE)
    maxD.rand.sd <- apply(X = maxD.rand, MARGIN = 2, FUN = sd, 
        na.rm = TRUE)
    maxD.obs.z <- (maxD.obs - maxD.rand.mean)/maxD.rand.sd
    maxD.obs.rank <- apply(X = rbind(maxD.obs, maxD.rand), MARGIN = 2, 
        FUN = rank)[1, ]
    maxD.obs.rank <- ifelse(is.na(maxD.rand.mean), NA, maxD.obs.rank)
    data.frame(ntaxa = specnumber(samp), maxD.obs, maxD.rand.mean, 
        maxD.rand.sd, maxD.obs.rank, maxD.obs.z, maxD.obs.p = maxD.obs.rank/(runs + 
            1), runs = runs, row.names = row.names(samp))
}

# data(phylocom)
# test <- ses.maxD(phylocom$sample, cophenetic(phylocom$phylo), null.model="phylogeny.pool")

