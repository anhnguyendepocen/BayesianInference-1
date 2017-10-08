calcPosterior <- function(priors, liklihoods, ret=False){
    joint <- priors * liklihoods
    posterior <- joint / sum(joint)
    if(!ret){
        cat("priors\n")
        print(priors)
        cat("liklihoods\n")
        print(liklihoods)
        cat("joint\n")
        print(joint)
        cat("sum joint\n")
        print(sum(joint))
        cat("posterior\n")
        print(posterior)
    }
    else{
        res <- list()
        res$priors <- priors
        res$liklihoods <- liklihoods
        res$joint <- joint
        res$posterior <- posterior
        return(res)
    }
}

latexPosterior <- function(priors, liklihoods){
    # Print a LaTeX Table of the Priors, Liklihoods, Joint, and Posteriors
    res <- calcPosterior(priors=priors, liklihoods=liklihoods, ret=TRUE)
    cat("\\begin{tabular}{lllll}\n")
    for(i in res$priors){
            cat("&  ", "A", "  ")
    }
    cat("\\\\\n")
    cat("\\hline\n")
    cat("Prior\t")
    for(i in res$priors){
        cat("&  ", i, "  ")
        }
    cat("&\\\\\n")
    cat("Liklihood\t")
    for(i in res$liklihoods){
        cat("&  ", i, "  ")
        }
    cat("&\\\\\n")
    cat("Joint\t")
    for(i in res$joint){
        cat("&  ", i, "  ")
        }
    cat(sum(res$joint), "&")
    cat("\\\\\n")
    cat("Liklihood  ")
    for(i in res$posterior){
        cat("&  ", i, "  ")
        }
    cat("&\\\\\n")
    cat("\\hline\n")
    cat("\\end{tabular}\n")
    }

    
latexPosterior(rep(1/3, 3), c(.9^6, .85^6, .8^6))
