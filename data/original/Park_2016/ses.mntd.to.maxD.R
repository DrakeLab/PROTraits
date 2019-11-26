# ses.mntd.to.maxD
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


mntd.to.maxD <- function (samp, dis, abundance.weighted=FALSE) {

    mntd.to.maxD <- mntd(samp, dis, abundance.weighted=FALSE)/maxD(samp,dis,abundance.weighted=FALSE)
}


ses.mntd.to.maxD <- function (samp, dis, null.model = c("taxa.labels", "richness", 
    "frequency", "sample.pool", "phylogeny.pool", "independentswap", 
    "trialswap"), abundance.weighted = FALSE, runs = 999, iterations = 1000) 
{
    dis <- as.matrix(dis)
    mntd.to.maxD.obs <- mntd.to.maxD(samp, dis, abundance.weighted = abundance.weighted)
    mntd.obs <- mntd(samp, dis, abundance.weighted = abundance.weighted)
    null.model <- match.arg(null.model)
    mntd.to.maxD.rand <- switch(null.model, taxa.labels = t(replicate(runs, 
        mntd.to.maxD(samp, taxaShuffle(dis), abundance.weighted = abundance.weighted))), 
        richness = t(replicate(runs, mntd.to.maxD(randomizeMatrix(samp, 
            null.model = "richness"), dis, abundance.weighted))), 
        frequency = t(replicate(runs, mntd.to.maxD(randomizeMatrix(samp, 
            null.model = "frequency"), dis, abundance.weighted))), 
        sample.pool = t(replicate(runs, mntd.to.maxD(randomizeMatrix(samp, 
            null.model = "richness"), dis, abundance.weighted))), 
        phylogeny.pool = t(replicate(runs, mntd.to.maxD(randomizeMatrix(samp, 
            null.model = "richness"), taxaShuffle(dis), abundance.weighted))), 
        independentswap = t(replicate(runs, mntd.to.maxD(randomizeMatrix(samp, 
            null.model = "independentswap", iterations), dis, 
            abundance.weighted))), trialswap = t(replicate(runs, 
            mntd.to.maxD(randomizeMatrix(samp, null.model = "trialswap", 
                iterations), dis, abundance.weighted))))
    mntd.to.maxD.rand.mean <- apply(X = mntd.to.maxD.rand, MARGIN = 2, FUN = mean, 
        na.rm = TRUE)
    mntd.to.maxD.rand.sd <- apply(X = mntd.to.maxD.rand, MARGIN = 2, FUN = sd, 
        na.rm = TRUE)
    mntd.to.maxD.obs.z <- (mntd.to.maxD.obs - mntd.to.maxD.rand.mean)/mntd.to.maxD.rand.sd
    mntd.to.maxD.obs.rank <- apply(X = rbind(mntd.to.maxD.obs, mntd.to.maxD.rand), MARGIN = 2, 
        FUN = rank)[1, ]
    mntd.to.maxD.obs.rank <- ifelse(is.na(mntd.to.maxD.rand.mean), NA, mntd.to.maxD.obs.rank)
    data.frame(ntaxa = specnumber(samp), mntd.to.maxD.obs, mntd.to.maxD.rand.mean, 
        mntd.to.maxD.rand.sd, mntd.to.maxD.obs.rank, mntd.to.maxD.obs.z, mntd.to.maxD.obs.p = mntd.to.maxD.obs.rank/(runs + 
            1), runs = runs, row.names = row.names(samp))
}

# data(phylocom)
# test <- ses.mntd.to.maxD(phylocom$sample, cophenetic(phylocom$phylo), null.model="phylogeny.pool")

