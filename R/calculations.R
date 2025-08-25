# calculations.R

estimate_sample_size <- function(d, power) {
  pwr.t.test(d = d, sig.level = 0.05, power = power, type = "two.sample")
}
