# 

# Temps par step fixe.
# Mais mettre dans les arguments une durée en millisecondes.

simu_DDM <- function(drift_rate, theta = 20, z = -1, noise = 0.75, sensory_delay = 100, n_steps = 10000, n_iterations = 10) {
  
  # Define the number of steps
  n_steps <- 3000 # Number of step the Euler method is iterate
  n_iterations <- 100 # Number 
  
  # Define some psychological delays
  sensory_delay <- 100 # Delay between the stimulus onset and the beginning of evidences accumulation
  motor_delay <- 100 # Delay between reaching the boundary and made the motor command
  
  # Get the drift rate parameters
  dt <- 0.1 # Time step (second)
  mA <- 1.0 # Firing rate of the neuron A
  mB <- 0.95 # Firing rate of the neuron B # mA - drift_rate
  drift_rate <- (mA - mB) * dt
  
  # Get some other DDM parameters
  z <- -1 # Initial bias
  theta <- 20 # Distance from threshold
  sigma <- 0.75 # Amplitude of the noise
  
  # Prepare reaction times and decision lists
  RT_A <- rep(NA, n_iterations)
  RT_B <- rep(NA, n_iterations)
  decision <- rep(0, n_iterations)
  
  # Initialize plot window
  DDMplot <- plot.new()
  DDMplot <- plot.window(xlim = c(0, n_steps), ylim = c((-theta - 10), (theta + 10)), xaxs = "i", yaxs = "i") # mettre en relatif. Avec max de x ?
  colors <- rgb(runif(n_iterations), runif(n_iterations), runif(n_iterations)) 
  
  # For each decision process
  for (iteration in seq(1, n_iterations)) {
    
    # Initialize some values
    x <- rep(NA, n_steps) # Array of values
    x[1] <- z
    
    # For each time sample
    for (time_step in seq(2, n_steps)) {
      
      # 
      if (time_step <= sensory_delay) {
        x[time_step] = z}
      
      # faire un truc avec while decision[iteration] == 0
      
      if (time_step > sensory_delay & decision[iteration] != 0) {
        x[time_step] = NA}
      
      # If we are over the sensory delay, compute the accumulation of evidences
      if (time_step > sensory_delay & decision[iteration] == 0) {
        noise = sigma * rnorm(1, mean = 0, sd = 1) * sqrt(dt)
        x[time_step] = x[time_step - 1] + drift_rate + noise}
      
      # If the accumulation of evidences reach the thresold A
      if (x[time_step] >= theta & decision[iteration] == 0) {
        RT_A[iteration] = time_step * dt # Time of the decision
        decision[iteration] = 1} # Choose decision A
      
      # If the accumulation of evidences reach the thresold B
      if (x[time_step] <= -theta & decision[iteration] == 0) {
        RT_B[iteration] = time_step * dt # Time of the decision
        decision[iteration] = -1} # Choose decision B
    }
    
    # Plot each accumulation of evidence
    DDMplot <- lines(x, col = colors[iteration])
  }
  
  # Plot theoretical drift rate, initial bias and decision boundaries
  DDMplot <- arrows(x0 = sensory_delay, y0 = z, x1 = (sensory_delay + (n_steps / 4)), y1 = (drift_rate * (sensory_delay + (n_steps / 4)) + z), length = 0.2, angle = 35, code = 2, col = "black", lwd = 2)
  DDMplot <- abline(h = z, col = "black", lty = 2, lwd = 2)
  DDMplot <- abline(h = theta, col = "black", lwd = 2)
  DDMplot <- abline(h = -theta, col = "black", lwd = 2)
  # DDMplot <- text() seuils de dé
  # afficher le bruit gaussien autour de la flèche
  
  # Plot reaction times histograms
  # DDMplot <- barplot(RT_A)
  # DDMplot <- barplot(RT_A)
  
  # Display some informations on the graph
  DDMplot <- axis(1)
  DDMplot <- axis(2)
  DDMplot <- box(lwd = 2)
  DDMplot <- title(xlab = "Time steps", ylab = "Amount of evidences accumulated (a.u.)", main = "Drift Diffusion Model simulation", sub = expression(paste(theta, " = ", as.character(drift_rate))))
}