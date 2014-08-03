# DRIFT DIFFUSION MODEL SIMULATION.
# Maxime Maheu, 2014.

# Arguments :
  # v : drift rate,
  # a : decision boundaries,
  # z : initial bias toward one of the choices,
  # eta : amplitude of the noise,
  # t0 : delay between the stimulus onset and the beginning of evidences accumulation (sensory delay),
  # n_steps : number of time steps,
  # n_iterations : number of decision processes.

DDMsimu <- function(v, a = 30, z = -5, eta = 0.75, t0 = 100, n_steps = 3000, n_iterations = 20) {

  motor_delay <- 100 # Delay between reaching the boundary and made the motor command
  # Temps par step fixe. Mais mettre dans les arguments une durée en millisecondes.
  
  # Get the drift rate parameters
  dt <- 0.1 # Time step (second)
  mA <- 1.0 # Firing rate of the neuron A
  mB <- mA - v # Firing rate of the neuron B
  v <- (mA - mB) * dt
  if (v < 0) {v = 0}
  
  # Prepare reaction times and decision lists
  RT_A <- rep(NA, n_iterations)
  RT_B <- rep(NA, n_iterations)
  decision <- rep(0, n_iterations)
  
  # Initialize plot window
  graphics.off()
  layout(matrix(c(2, 1, 3), ncol = 1, byrow = TRUE), widths = 1, heights = c(1, 4, 1))
  par(oma = c(3, 3, 3, 3))
  
  # Get some random colors
  colors <- rgb(runif(n_iterations), runif(n_iterations), runif(n_iterations)) 
  
  # Draw the first plot
  par(mar = c(0, 2, 0, 2), cex = 1.5)
  plot.new()
  plot.window(xlim = c(0, n_steps), ylim = c(-a, a), xaxs = "i", yaxs = "i")
  
  # For each decision process
  for (iteration in seq(1, n_iterations)) {
    
    # Initialize some values
    x <- rep(NA, n_steps) # Array of values
    x[1] <- z
    
    # For each time sample
    for (time_step in seq(2, n_steps)) {
      
      # Apply non-decision time
      if (time_step <= t0) {
        x[time_step] = z}
      
      if (time_step > t0 & decision[iteration] != 0) {
        x[time_step] = NA}
      
      # If we are over the sensory delay, compute the accumulation of evidences
      if (time_step > t0 & decision[iteration] == 0) {
        noise = eta * rnorm(1, mean = 0, sd = 1) * sqrt(dt)
        x[time_step] = x[time_step - 1] + v + noise}
      
      # If the accumulation of evidences reach the thresold A
      if (x[time_step] >= a & decision[iteration] == 0) {
        RT_A[iteration] = time_step * dt # Time of the decision
        decision[iteration] = 1} # Choose decision A
      
      # If the accumulation of evidences reach the thresold B
      if (x[time_step] <= -a & decision[iteration] == 0) {
        RT_B[iteration] = time_step * dt # Time of the decision
        decision[iteration] = -1} # Choose decision B
    }
    
    # Plot each accumulation of evidence
    lines(x, col = colors[iteration])
  }
  
  # Plot theoretical drift rate and initial bias
  arrows(x0 = t0, y0 = z, x1 = (t0 + (n_steps / 4)), y1 = (v * (t0 + (n_steps / 4)) + z), length = 0.2, angle = 35, code = 2, col = "black", lwd = 2)
  abline(h = z, col = "black", lty = 2, lwd = 2)
  
  # Display some informations on the graph
  axis(2, at = c(-a, z, a), labels = c("B", "z", "A"), las = 2)
  box(lwd = 2)

  # Draw the second plot
  par(mar = c(0, 2, 0, 2), xaxs = "i", yaxs = "i") #mar = c(5.1, 4.1, 4.1, 2.1)
  if (length(na.omit(RT_A)) >= 1) {RT_A_hist <- hist((RT_A / dt), breaks = (n_iterations/2), main = "", xlim = c(0, n_steps), axes = FALSE, col = "blue")}
  if (length(na.omit(RT_A)) >= 2) {RT_A_hist <- lines(density(na.omit(RT_A / dt), adjust = 2), lwd = 2)}

  
  # Draw the third plot
  par(mar = c(0, 2, 0, 2), xaxs = "i", yaxs = "i") #mar = c(0, 0, 0, 0)
  if (length(na.omit(RT_B)) >= 1) {RT_B_hist <- hist((RT_B / dt), breaks = (n_iterations/2), main = "", yaxt = "n", xlim = c(0, n_steps), ylim = c(max(as.numeric(RT_A_hist[2])), 0), col = "red")}
  if (length(na.omit(RT_B)) >= 2) {RT_B_hist <- lines(density(na.omit(RT_B / dt), adjust = 2), lwd = 2)}
  
  # Label the outer margin area so we can see where it is  
  mtext("Drift Diffusion Model simulation", side = 3, line = 1, cex = 1.5, col = "black", outer = TRUE)
}