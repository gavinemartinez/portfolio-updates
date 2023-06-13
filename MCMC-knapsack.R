# Function to calculate the target distribution probability for a given state
calculate_prob <- function(x, beta) {
  exp(beta * sum(v * x))
}
# Function to implement the MCMC algorithm for the knapsack problem with the updated distribution
MCMC_knapsack_updated <- function(values, weights, weight_limit, beta, num_iterations) {
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
  12
  # Function to calculate the acceptance probability
  acceptance <- function(x, y, beta) {
    prob_y <- calculate_prob(y, beta)
    prob_x <- calculate_prob(x, beta)
    prob_y / prob_x
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
    # Randomly decide whether to accept or reject the proposed state
    y <- x
    y[item] <- 1 - y[item] # Flip the state of the selected item
    if (runif(1) > acceptance(x, y, beta)) {
      x <- y # Accept the proposed state
    }
  }
  return(list(num_iters = num_iterations, max_value = max_value, max_weight = max_weight))
}

