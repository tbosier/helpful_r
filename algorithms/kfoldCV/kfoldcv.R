
kcv <- function(data, k) {
  # This function returns a list of dataframes that can be used for further modeling
  # This function does not do stratified kcv.

  # Error checking
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input data must be a data frame or a matrix.")
  }
  
  if (k <= 0 || k > nrow(data)) {
    stop("k must be a positive integer smaller than or equal to the number of rows in the data.")
  }

  # Shuffle the dataset
  index <- sample(1:nrow(data), nrow(data), replace = FALSE)
  data_shuffled <- data[index, ]
  
  # Initialize a list to hold k data frames
  hold_dfs <- list()
  
  # Calculate the size of each fold
  fold_size <- floor(nrow(data) / k)
  
  # Initialize starting and ending indices for slicing
  start_idx <- 1
  end_idx <- fold_size
  
  # Create k data frames (folds)
  for (i in 1:k) {
    
    # Special case for the last fold to capture remaining rows
    if (i == k) {
      end_idx <- nrow(data)
    }
    
    # Slice the data to create the i-th fold
    hold_dfs[[i]] <- data_shuffled[start_idx:end_idx, ]
    
    # Update the starting and ending indices for the next iteration
    start_idx <- end_idx + 1
    end_idx <- end_idx + fold_size
  }
  
  return(hold_dfs)
}