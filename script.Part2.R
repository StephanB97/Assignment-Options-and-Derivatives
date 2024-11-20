#####################################################################################################################################
##Exercise 2 
# Part (a): Generate Price Matrix and Delta Matrix
explicit_binomial_with_delta <- function(T, S, K, Ru, Rd) {
  # Initialize price matrix and delta matrix
  price_matrix <- matrix(0, nrow = T + 1, ncol = T + 1)
  delta_matrix <- matrix(0, nrow = T, ncol = T)
  
  # Calculate stock prices and option payoffs at maturity
  for (i in 0:T) {
    price_matrix[i + 1, T + 1] <- S * Ru^i * Rd^(T - i)  # Stock price at maturity
  }
  
  # Calculate the call option payoff at maturity
  price_matrix[, T + 1] <- pmax(price_matrix[, T + 1] - K, 0)  # Call payoff at maturity
  
  # Backward induction to calculate option prices and delta
  for (t in (T - 1):0) {
    for (i in 0:t) {
      # Option price at current time and state
      price_matrix[i + 1, t + 1] <- 0.5 * (price_matrix[i + 1, t + 2] + price_matrix[i + 2, t + 2])
      
      # Calculate delta (the option's sensitivity to the stock price)
      delta_matrix[i + 1, t + 1] <- (price_matrix[i + 2, t + 2] - price_matrix[i + 1, t + 2]) / (S * (Ru - Rd))
    }
  }
  
  return(list(price_matrix = price_matrix, delta_matrix = delta_matrix))
}

# Define parameters
T <- 100    # Number of time steps
S <- 100    # Initial stock price
K <- 100    # Strike price
Ru <- 1.02  # Upward factor
Rd <- 0.99  # Downward factor

# Generate price and delta matrices
result <- explicit_binomial_with_delta(T, S, K, Ru, Rd)

# Extract the price matrix and delta matrix
price_matrix <- result$price_matrix
delta_matrix <- result$delta_matrix
##########################################################################

# Part (b): Monte Carlo Simulation and Hedging Portfolio (Plotting stock price, delta, option price, and portfolio value on one graph)
hedging_portfolio <- function(T, S, K, Ru, Rd, delta_matrix) {
  # Initialize arrays for stock prices, option prices, portfolio values, and deltas
  stock_prices <- numeric(T + 1)
  option_prices <- numeric(T + 1)
  portfolio_values <- numeric(T + 1)
  deltas <- numeric(T + 1)
  
  # Initial stock price and option price
  stock_prices[1] <- S
  option_prices[1] <- price_matrix[1, 1]  # Price at time 0
  deltas[1] <- delta_matrix[1, 1]  # Delta at time 0
  
  # Initialize the portfolio at time 0
  portfolio_values[1] <- deltas[1] * S - (option_prices[1] - deltas[1] * S)
  
  # Simulate the binomial path and calculate the hedging portfolio
  for (t in 1:T) {
    # Update stock price based on upward or downward movement
    if (runif(1) < 0.5) {
      stock_prices[t + 1] <- stock_prices[t] * Ru  # Upward movement
    } else {
      stock_prices[t + 1] <- stock_prices[t] * Rd  # Downward movement
    }
    
    # Calculate delta at time t (use correct indexing)
    if (t < T) {
      deltas[t + 1] <- delta_matrix[t + 1, min(t + 1, T)]  # Ensure indexing is within bounds
    } else {
      deltas[t + 1] <- delta_matrix[t, T]  # At the final step, use last value
    }
    
    # Update option price (we assume option price follows backward induction logic)
    option_prices[t + 1] <- 0.5 * (option_prices[t] + option_prices[t + 1])
    
    # Update portfolio value at time t+1
    portfolio_values[t + 1] <- deltas[t + 1] * stock_prices[t + 1] - (option_prices[t + 1] - deltas[t + 1] * stock_prices[t + 1])
  }
  
  return(list(stock_prices = stock_prices, option_prices = option_prices, portfolio_values = portfolio_values, deltas = deltas))
}

# Run the simulation for hedging portfolio
hedging_result <- hedging_portfolio(T, S, K, Ru, Rd, delta_matrix)

# Set minimum and maximum values for the plot
min_value <- min(c(hedging_result$stock_prices, hedging_result$deltas * max(hedging_result$stock_prices), hedging_result$option_prices, hedging_result$portfolio_values))
max_value <- max(c(hedging_result$stock_prices, hedging_result$deltas * max(hedging_result$stock_prices), hedging_result$option_prices, hedging_result$portfolio_values))

# Create the initial plot for stock prices
plot(1:(T + 1), hedging_result$stock_prices, type = "l", col = "blue", 
     xlab = "Time", ylab = "Value", ylim = c(min_value, max_value), main = "Stock Price, Delta, Option Price, and Portfolio Value")

# Add lines for the remaining variables
lines(1:(T + 1), hedging_result$deltas * max_value, col = "green")  # Rescale delta to match range
lines(1:(T + 1), hedging_result$option_prices, col = "red")
lines(1:(T + 1), hedging_result$portfolio_values, col = "purple")

# Add a legend
legend("topright", legend = c("Stock Price", "Delta", "Option Price", "Portfolio Value"), 
       col = c("blue", "green", "red", "purple"), lty = 1)


#######################################################################################################################################################################################

# Part (c): Adjust Hedging Portfolio Every Other Period (Corrected Portfolio Calculation)
adjusted_hedging_portfolio <- function(T, S, K, Ru, Rd, delta_matrix, rebalancing_period = 2) {
  # Initialize arrays for stock prices, option prices, portfolio values, and deltas
  stock_prices <- numeric(T + 1)
  option_prices <- numeric(T + 1)
  portfolio_values <- numeric(T + 1)
  deltas <- numeric(T + 1)
  
  # Initial stock price and option price
  stock_prices[1] <- S
  option_prices[1] <- price_matrix[1, 1]  # Price at time 0
  deltas[1] <- delta_matrix[1, 1]  # Delta at time 0
  
  # Initialize the portfolio at time 0
  portfolio_values[1] <- deltas[1] * S  # Initial portfolio value: delta * stock price
  
  # Simulate the binomial path and adjust the portfolio every other period
  for (t in 1:T) {
    # Update stock price based on upward or downward movement
    if (runif(1) < 0.5) {
      stock_prices[t + 1] <- stock_prices[t] * Ru  # Upward movement
    } else {
      stock_prices[t + 1] <- stock_prices[t] * Rd  # Downward movement
    }
    
    # Rebalance the portfolio every other period
    if (t %% rebalancing_period == 0) {
      # Ensure delta_matrix indexing is within bounds, preventing out-of-bounds error
      delta_matrix_value <- ifelse(t < T, delta_matrix[t + 1, t + 1], delta_matrix[T, T])
      deltas[t + 1] <- delta_matrix_value
      
      # Recalculate the option price at each time step (Backward induction should have been done earlier)
      option_prices[t + 1] <- 0.5 * (option_prices[t] + option_prices[t + 1])  # Simple approximation for demonstration
      
      # Recalculate the portfolio value: delta * stock price
      portfolio_values[t + 1] <- deltas[t + 1] * stock_prices[t + 1]
    } else {
      # If no rebalancing is done, the portfolio value remains unchanged
      portfolio_values[t + 1] <- portfolio_values[t]  # Maintain previous portfolio value
      option_prices[t + 1] <- option_prices[t]  # Option price remains the same without rebalancing
    }
  }
  
  return(list(stock_prices = stock_prices, option_prices = option_prices, portfolio_values = portfolio_values, deltas = deltas))
}

# Run the simulation for adjusted portfolio
adjusted_portfolio_result <- adjusted_hedging_portfolio(T, S, K, Ru, Rd, delta_matrix)

# Set the plot size
par(mar = c(5, 5, 2, 2))  # Adjust margins to make room for labels and title
plot(1:(T + 1), adjusted_portfolio_result$stock_prices, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Price", main = "Adjusted Hedging Portfolio Every Other Period", ylim = range(c(adjusted_portfolio_result$stock_prices, adjusted_portfolio_result$option_prices, adjusted_portfolio_result$portfolio_values)))
lines(1:(T + 1), adjusted_portfolio_result$option_prices, col = "red", lwd = 2)
lines(1:(T + 1), adjusted_portfolio_result$portfolio_values, col = "green", lwd = 2)
legend("topright", legend = c("Stock Price", "Option Price", "Hedging Portfolio"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

# Reset plotting parameters to default
par(mar = c(5, 4, 4, 2) + 0.1)


#############################################################################################################################################################################################################################################################################################################

# Part (d): Monte Carlo Method for Tracking Error
tracking_error_montecarlo <- function(T, S, K, Ru, Rd, N) {
  # Run Monte Carlo simulations
  tracking_errors <- numeric(T + 1)
  
  for (i in 1:N) {
    # Run a simulation
    portfolio_result <- hedging_portfolio(T, S, K, Ru, Rd, delta_matrix)
    
    # Calculate the difference between the portfolio and option prices at each time step
    error <- portfolio_result$portfolio_values - portfolio_result$option_prices
    
    # Calculate tracking error as the standard deviation of the errors
    tracking_errors <- tracking_errors + error^2
  }
  
  # Return the standard deviation as tracking error
  return(sqrt(tracking_errors / N))
}

# Define the number of Monte Carlo simulations
N <- 10000  # Set the number of simulations to your desired number

# Run the Monte Carlo method for tracking error calculation
tracking_error_result <- tracking_error_montecarlo(T, S, K, Ru, Rd, N)

# Plot the tracking error as a function of time
plot(1:(T + 1), tracking_error_result, type = "l", col = "purple", 
     xlab = "Time", ylab = "Tracking Error", main = "Tracking Error Over Time")
