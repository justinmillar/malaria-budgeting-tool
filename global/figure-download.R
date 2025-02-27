
generate_plots <- function(plan, year, currency) {
  # Example: Filter your data based on the inputs (here we use dummy data)
  # In your real application, replace the dummy data with your actual data filtering and plotting code.

  library(ggplot2)

  # Dummy plot 1
  data1 <- data.frame(x = 1:10, y = rnorm(10))
  plot1 <- ggplot(data1, aes(x, y)) +
    geom_line() +
    ggtitle(paste("Plan:", paste(plan, collapse = ", "),
                  "| Year:", year,
                  "| Currency:", currency))

  # Dummy plot 2
  data2 <- data.frame(x = 1:10, y = rnorm(10, mean = 5))
  plot2 <- ggplot(data2, aes(x, y)) +
    geom_point() +
    ggtitle("Additional Plot Example")

  # Return a named list of plots
  list(plot1 = plot1, plot2 = plot2)
}
