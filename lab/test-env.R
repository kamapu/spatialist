## Testing behaviour in and outside of a function
df <- data.frame(a = 1:5, b = letters[1:5])

# Calling subset() directly in the global environment
global_result <- subset(df, a > 2)
print(global_result)

test_subset <- function(x, subset) {
  # Calling subset() inside the function
  function_result <- subset(x, subset)
  print(function_result)
}

test_subset(df, a > 2)

# Solution
test_subset <- function(x, subset_expr) {
  function_result <- subset(x, subset_expr, drop = TRUE)
  print(function_result)
}
test_subset(df, a > 2)

# Test in raster
library(terra)

r <- raster <- rast(file.path("inst", "binrast.tif"))

ra <- as.array(r)

as.array2 <- function(x) {
    x <- as.array(x)
    return(x)
    }

rb <- as.array2(r)

