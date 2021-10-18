CUGtest_alltriads <- function(dat, mode, cmode, reps) {
  
  # Arguments:
  # dat: network of interest
  # mode: graph (undirected) or digraph (directed)
  # cmode: how to condition random network, see cug.test
  # reps: number of replications to use in simulation
 
  triads_to_count <- c("003",  "012",  "102", "021D", "021U", "021C",
                    "111D", "111U", "030T", "030C", "201",  "120D",
                    "120U", "120C", "210", "300")
  
  cug_list <- list()
  cug_results <- cug.test2(dat, 
                        FUN = triad.census, 
                        mode = mode, 
                        cmode = cmode, 
                        reps = reps)
  
  
  # looping over each triad type
  for (x in 1:length(triads_to_count)){
    
    cug_list[[x]] <- c(obs = as.numeric(cug_results[[x]]$obs.stat),
                    mean_random = mean(cug_results[[x]]$rep.stat), 
                    prop_random_lte_obs = cug_results[[x]]$plteob)
  }
  
  # making table of results
  cug_table <- do.call(rbind, cug_list)
  cug_table <- data.frame(triads = triads_to_count, cug_table)
  
  return(cug_table)
}



cug.test2<-
  function (dat, FUN, mode = c("digraph", "graph"), 
            cmode = c("size", "edges", "dyad.census"), 
            diag = FALSE, reps = 1000, ignore.eval = TRUE, 
            FUN.args = list()) 
  {
    if (ignore.eval) {
      dat <- as.edgelist.sna(dat)
      if (is.list(dat)) 
        return(sapply(dat, cug.test, FUN = FUN, mode = mode, 
                      cmode = cmode, diag = diag, reps = reps, 
                      ignore.eval = ignore.eval, 
                      FUN.args = FUN.args))
      n <- attr(dat, "n")
      if (!diag) {
        dat <- dat[dat[, 1] != dat[, 2], , drop = FALSE]
        attr(dat, "n") <- n
        ndiag <- 0
      }
      else ndiag <- sum(dat[, 1] == dat[, 2])
      mode <- match.arg(mode)
      cmode <- match.arg(cmode)
      if (cmode == "size") {
        m <- NULL
        dc <- NULL
      }
      else if (cmode == "edges") {
        m <- switch(match.arg(mode), graph = (NROW(dat) - 
                                                ndiag)/2 + ndiag, 
                    digraph = NROW(dat))
        dc <- NULL
      }
      else if (cmode == "dyad.census") {
        m <- NULL
        dc <- dyad.census(dat)
      }
      getstat <- function(d) {
        do.call(fun, c(list(d)))
      }
      drawrep <- switch(cmode, size = function(n, ...) {
        rgraph(n, 1, mode = mode, diag = diag, tprob = 0.5, 
               return.as.edgelist = TRUE)
      }, edges = function(n, m, ...) {
        rgnm(n = 1, nv = n, m = m, mode = mode, diag = diag, 
             return.as.edgelist = TRUE)
      }, dyad.census = function(n, dc, ...) {
        rguman(n = 1, nv = n, mut = dc[1], asym = dc[2], 
               null = dc[3], method = "exact", return.as.edgelist = TRUE)
      }, )
    }
    else {
      dat <- as.sociomatrix.sna(dat)
      if (is.list(dat)) 
        return(sapply(dat, cug.test, FUN = FUN, mode = mode, 
                      cmode = cmode, diag = diag, reps = reps, ignore.eval = ignore.eval, 
                      FUN.args = FUN.args))
      else if (length(dim(dat)) > 2) 
        return(apply(dat, 1, cug.test, FUN = FUN, mode = mode, 
                     cmode = cmode, diag = diag, reps = reps, ignore.eval = ignore.eval, 
                     FUN.args = FUN.args))
      n <- NROW(dat)
      m <- NULL
      dc <- NULL
      mode <- match.arg(mode)
      cmode <- match.arg(cmode)
      getstat <- function(d) {
        do.call(fun, c(list(d)))
      }
      drawrep <- switch(cmode, size = function(n, ...) {
        rgraph(n, 1, mode = mode, diag = diag, tprob = 0.5)
      }, edges = switch(mode, digraph = function(n, ...) {
        g <- dat
        g[upper.tri(g, diag = diag) | lower.tri(g)] <- sample(g[upper.tri(g, 
                                                                          diag = diag) | lower.tri(g)])
        g
      }, graph = function(n, ...) {
        g <- dat
        g[upper.tri(g, diag = diag)] <- sample(g[upper.tri(g, 
                                                           diag = diag)])
        g[lower.tri(g)] <- t(g)[lower.tri(g)]
        g
      }), dyad.census = function(n, ...) {
        g <- rewire.ud(dat, 1)[1, , ]
        if (diag) diag(g) <- sample(diag(g))
        g
      }, )
    }
    fun <- match.fun(FUN)
    if ("mode" %in% names(formals(fun))) 
      callmode <- TRUE
    else callmode <- FALSE
    if ("diag" %in% names(formals(fun))) 
      calldiag <- TRUE
    else calldiag <- FALSE
    if (callmode) 
      FUN.args$mode <- mode
    if (calldiag) 
      FUN.args$diag <- diag
    
    repstats_matrix=matrix(ncol=16, nrow=reps)
    
    obs <- getstat(dat)
    
    
    for (i in 1:reps) {
      temp_dat=drawrep(n = n, m = m, dc = dc)
      repstats_matrix[i,] <- getstat(temp_dat)
    }
    
    out=list()
    for (x in 1:16) {
      
      out[[x]] <- list(obs.stat = obs[x], rep.stat = repstats_matrix[,x],
                       mode = mode, 
                       diag = diag, 
                       cmode = cmode, 
                       plteobs = mean(repstats_matrix[,x] <= obs[x]), 
                       pgteobs = mean(repstats_matrix[,x] >= obs[x]), 
                       reps = reps)
    }
    
    return(out)
  }