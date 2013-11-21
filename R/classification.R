# naive Bayes classifier
# train.data is the frequency matrix: 
# each row for a training point and each column for a feature.
naive.Bayes.train <- function(label, train.data)
{
    cats <- table(label)
    p.cats <- cats / sum(cats)
    X <- split(train.data, label)
    res <- foreach(M = X, .combine = rbind) %dopar%
        colSums(M)
    A <- as.matrix(res) + 1
    B <- colSums(train.data) + ncol(train.data)
    p.feature <- A %*% diag(1 / B)
    return(list(aprior = p.cats, nb = p.feature))
}

naive.Bayes.predict <- function(model, X)
{
    aprior <- model$aprior
    nb <- model$nb
    registerDoParallel()
    ans <- foreach(idx = 1:nrow(X), .combine = c) %dopar%
    {
        res <- foreach(j = 1:nrow(nb), .combine = c) %do%
        {
            y <- nb[j, ] * X[idx, ]
            prod(y[y > 0.0001])
        }
        which.max(res * aprior)
    }
    return(ans)
}

