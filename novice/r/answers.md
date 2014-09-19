---
layout: lesson
root: ../..
---



## Answers to R lessons

### Creating Functions


<pre class='in'><code>fence <- function(original, wrapper) {
  answer <- c(wrapper, original, wrapper)
  return(answer)
}</code></pre>


<pre class='in'><code>outer <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}</code></pre>


<pre class='in'><code>analyze <- function(fname) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = fname, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, min)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, max)
  plot(min_day_inflammation)
}</code></pre>


<pre class='in'><code>rescale <- function(v) {
  # Rescales a vector, v, to lie in the range 0 to 1.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L)
  return(result)
}
answer <- rescale(dat[, 4])
min(answer)
max(answer)
plot(answer)
plot(dat[, 4], answer)  # This hasn't been introduced yet, but it may be
                        # useful to show when explaining the answer.</code></pre>


<pre class='in'><code>rescale <- function(v, lower = 0, upper = 1) {
  # Rescales a vector, v, to lie in the range lower to upper.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L) * (upper - lower) + lower
  return(result)
}
answer <- rescale(dat[, 4], lower = 2, upper = 5)
min(answer)
max(answer)
answer <- rescale(dat[, 4], lower = -5, upper = -2)
min(answer)
max(answer)</code></pre>

### Analyzying Multiple Data Sets


<pre class='in'><code>print_N <- function(N) {
  nseq <- seq(N)
  for (num in nseq) {
    print(num)
  }
}</code></pre>


<pre class='in'><code>expo <- function(base, power) {
  result <- 1
  for (i in seq(power)) {
    result <- result * base
  }
  return(result)
}</code></pre>


<pre class='in'><code>total <- function(vec) {
  #calculates the sum of the values in a vector
  vec_sum <- 0
  for (num in vec) {
    vec_sum <- vec_sum + num
  }
  return(vec_sum)
}</code></pre>


<pre class='in'><code>analyze_all <- function(pattern) {
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(pattern = pattern)
  for (f in filenames) {
    analyze(f)
  }
}

# analyze_all("csv")</code></pre>

## Making Choices


<pre class='in'><code>plot_dist <- function(x, threshold) {
  if (length(x) > threshold) {
    boxplot(x)
  } else {
    stripchart(x)
  }
}</code></pre>


<pre class='in'><code>analyze <- function(filename, output = NULL) {
  # Plots the average, min, and max inflammation over time.
  # Input:
  #    filename: character string of a csv file
  #    output: character string of pdf file for saving
  if (!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, type = "l")
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation, type = "l")
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation, type = "l")
  if (!is.null(output)) {
    dev.off()
  }
}</code></pre>

### Command-line programs



<pre class='in'><code># arith.R
main <- function() {
  # Performs addition or subtraction from the command line.
  #
  # Takes three arguments:
  # The first and third are the numbers.
  # The second is either + for addition or - for subtraction.
  #
  # Ex. usage:
  #   Rscript arith.R 1 + 2
  #   Rscript arith.R 3 - 4
  #
  args <- commandArgs(trailingOnly = TRUE)
  num1 <- as.numeric(args[1])
  operation <- args[2]
  num2 <- as.numeric(args[3])
  if (operation == "+") {
    answer <- num1 + num2
    cat(answer)
  } else if (operation == "-") {
    answer <- num1 - num2
    cat(answer)
  } else {
    stop("Invalid input. Use + for addition or - for subtraction.")
  }
}

main()</code></pre>


<pre class='in'><code># find-pattern.R
main <- function() {
  # Finds all files in the current directory that contain a given pattern.
  #
  # Takes one argument: the pattern to be searched.
  #
  # Ex. usage:
  #   Rscript find-pattern.R csv
  #
  args <- commandArgs(trailingOnly = TRUE)
  pattern <- args[1]
  files <- list.files(pattern = pattern)
  cat(files, sep = "\n")
}

main()</code></pre>



<pre class='in'><code># check.R
main <- function() {
  # Checks that all csv files have the same number of rows and columns.
  #
  # Takes multiple arguments: the names of the files to be checked.
  #
  # Ex. usage:
  #   Rscript check.R inflammation-*
  #
  args <- commandArgs(trailingOnly = TRUE)
  first_file <- read.csv(args[1], header = FALSE)
  first_dim <- dim(first_file)
#   num_rows <- dim(args[1])[1]  # nrow(args[1])
#   num_cols <- dim(args[1])[2]  # ncol(args[1])
  for (filename in args[-1]) {
    new_file <- read.csv(filename, header = FALSE)
    new_dim <- dim(new_file)
    if (new_dim[1] != first_dim[1] | new_dim[2] != first_dim[2]) {
      cat("Not all the data files have the same dimensions.")
    }
  }
}

main()</code></pre>


<pre class='in'><code># readings-usage.R
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  action <- args[1]
  filenames <- args[-1]
  if (!(action %in% c("--min", "--mean", "--max"))) {
    usage()
  } else if (length(filenames) == 0) {
    process(file("stdin"), action)
  } else {  
    for (f in filenames) {
      process(f, action)
    }
  }
}

process <- function(filename, action) {
  dat <- read.csv(file = filename, header = FALSE)
  
  if (action == "--min") {
    values <- apply(dat, 1, min)
  } else if (action == "--mean") {
    values <- apply(dat, 1, mean)
  } else if (action == "--max") {
    values <- apply(dat, 1, max)
  }
  cat(values, sep = "\n")
}

usage <- function() {
  cat("usage: Rscript readings-usage.R [--min, --mean, --max] filenames", sep = "\n")
}

main()</code></pre>



<pre class='in'><code># line-count.R
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    for (filename in args) {
      input <- readLines(filename)
      num_lines <- length(input)
      cat(filename)
      cat(" ")
      cat(num_lines, sep = "\n")
    }
  } else {
    input <- readLines(file("stdin"))
    num_lines <- length(input)
    cat(num_lines, sep = "\n")
  }
}

main()</code></pre>
