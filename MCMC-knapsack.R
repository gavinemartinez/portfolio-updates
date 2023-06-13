MCMC_knapsack_algo <- function(values, weights, weight_limit, num_iterations = 10000) {
  M <- length(values) # Number of items
  x <- rep(0, M) # Initialize binary vector for knapsack items
  max_value <- 0 # Maximum value found so far
  max_weight <- 0 # Weight of Optimal knapsack
  # Function to calculate the total value of the knapsack
  calculate_value <- function(x) {
    sum(values * x)
  }
  # Function to calculate the total weight of the knapsack
  calculate_weight <- function(x) {
    sum(weights * x)
  }
  # Run the MCMC algorithm
  for (iter in 1:num_iterations) {
    # Select a random item
    item <- sample(1:M, 1)
    # Check if the item is currently in the knapsack
    if (x[item] == 1) {
      # Remove the item from the knapsack
      x[item] <- 0
    } else {
      # Add the item to the knapsack if weight limit allows
      if (calculate_weight(x) + weights[item] <= weight_limit) {
        10
        x[item] <- 1
      }
    }
    # Check if the current solution is better than the previous best
    curr_value <- calculate_value(x)
    curr_weight <- calculate_weight(x)
    if (curr_value > max_value) {
      max_value <- curr_value
      max_weight <- curr_weight
      optimal_x <- x
    }
  }
  return(list(num_iters = num_iterations, optimal_x = optimal_x, max_value = max_value, max_weight = max))
}

