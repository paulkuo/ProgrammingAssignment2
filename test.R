createTestMatrix <- function(n = 1, m = 1) {
    matrix(c(rep(c(m, rep(0, n)), n - 1), m), n, n)
}

testSolve <- function(m) {
    cacheM <- makeCacheMatrix(m)
    t <- Sys.time()
    cacheSolve(cacheM)
    message(Sys.time() - t)
    
    t <- Sys.time()
    cacheSolve(cacheM)
    message(Sys.time() - t)
}
