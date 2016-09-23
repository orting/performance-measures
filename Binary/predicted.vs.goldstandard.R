#!/usr/bin/Rscript

library( data.table )

main <- function(options) {

    if (length(options) < 1) {
        cat( "Usage: <informed-decisions> [<skew> <bias>]\n" )
        quit(status=1)
    }
    informed.decisions <- as.numeric( options[1] )
    skew <- if (length(options) < 2) 0.5 else as.numeric( options[2] )
    bias <- if (length(options) < 3) 0.5 else as.numeric( options[3] )
    run( informed.decisions, skew, bias )

    quit(status=0)
}
                    


run <- function(informed.decisions, skew, bias) {
    n.runs <- 5
    n.samples <- 200

    results <- data.table(Informedness=rep(0,n.runs),
                          PosAgreement=rep(0,n.runs),
                          NegAgreement=rep(0,n.runs),
                          Accuracy=rep(0,n.runs))
    
    ## Generate a series of references
    for (n in seq(n.runs) ) {
        reference <- rbinom(n=n.samples,size=1,prob=skew)
        predictions <- reference

        ## Make a new decision for (1-informed.decisions) of the reference where 
        ## P(y=1) = bias
        guesses <- 1 - abs(informed.decisions)
        idx <- sample.int(n=n.samples, size=round(n.samples*guesses), replace=FALSE)
        predictions[idx] <- rbinom(n=length(idx),size=1,prob=bias)

        if ( informed.decisions < 0 ) {
            ## We decide opposite of the oracle when sign is negative
            predictions[-idx] <- !predictions[-idx]
        }


        ## Calculate performance metrics
        tbl <- table(r=reference,p=predictions)
        set(results, n, "Informedness", informedness(tbl))
        set(results, n, "Markedness", markedness(tbl))
        set(results, n, "PosAgreement", positive.agreement(tbl))
        set(results, n, "NegAgreement", negative.agreement(tbl))
        set(results, n, "Accuracy",     accuracy(tbl))
        
    }
    print.result.stats(results)}


informedness <- function(tbl) {
    ## In binary case this is
    ## tpr + tnr - 1
    sum( diag(tbl)/rowSums(tbl)) - (nrow(tbl) - 1)
}

positive.agreement <- function(tbl) {
    tbl[2,2]/(tbl[1,2] + tbl[2,1] + tbl[2,2])
}

accuracy <- function(tbl) {
    sum(diag(tbl))/sum(tbl)
}

negative.agreement <- function(tbl) {
    tbl[1,1] / (tbl[1,1] + tbl[2,1] + tbl[1,2])
}

print.result.stats <- function(results) {
    cat("   Measure    |  Mean (std)   | Median (mad)  |  IQR  |  Min  |  Max\n")
    cat("----------------------------------------------------------------------\n")

    for ( col in colnames(results) ) {
        result <- results[,get(col)]
        r.mean <- mean(result)
        r.std <- sd(result)
        
        r.median <- median(result)
        r.mad <- mad(result,constant=1)

        r.IQR <- IQR(result)
        
        r.min <- min(result)
        r.max <- max(result)

        cat(sprintf(" %12s | %0.3f (%0.3f) | %0.3f (%0.3f) | %0.3f | %0.3f | %0.3f\n",
                    col, r.mean, r.std, r.median, r.mad, r.IQR, r.min, r.max))
    }
}

options <- commandArgs(trailingOnly=TRUE);
main(options);
