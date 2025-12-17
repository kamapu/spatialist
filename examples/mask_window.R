# Random window with weights
set.seed(42)
window <- matrix(sample(1:3, size = 121, replace = TRUE), ncol = 11)
window

mask_window(option = "right", window = window)
