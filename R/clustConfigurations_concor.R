clustConfigurations_concor <- function(concor_results, observedcorrelation){
    # Arguments: 
    # concor_results: result of concor function (partition results
    # based on concor algorithm)
    # observedcorrelation: observed correlation matrix, with 
    # actors to be clustered on columns
    
    resultlist <- list()
    correlations <- vector()

    for (i in 2:ncol(concor_results)) {

        cluster_result <- list(label = NA, clusters = NA, correlation = NA)
        cluster_result$label <- paste("number of clusters: ", i)
        clusters <- concor_results[,i]
        cluster_result$clusters <- clusters
        cluster_cor_mat <- clusterCorr(observedcorrelation, clusters)
        clustered_observed_cors <- gcor(cluster_cor_mat, observedcorrelation)
        cluster_result$correlation <- (clustered_observed_cors)
        resultlist <- c(resultlist, cluster_result)
        correlations <- c(correlations, clustered_observed_cors)
    }
    plot(x = 2:i, y = correlations, xlab = "depth", 
         ylab = "correlation", type = "b")
    resultlist$correlations <- correlations
    return(resultlist)
}

clusterCorr <- function (observed_cor_matrix, cluster_vector) 
{
    num_vertices <- nrow(observed_cor_matrix)
    cluster_cor_mat <- observed_cor_matrix
    for (i in 1:num_vertices) {
        for (j in 1:num_vertices) {
            cluster_cor_mat[i, j] <-  mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] == 
                cluster_vector[i] & cluster_vector[col(observed_cor_matrix)] == 
                cluster_vector[j])], na.rm = T)
        }
    }
    cluster_cor_mat[is.na(cluster_cor_mat)] <- 1
    return(cluster_cor_mat)
}
