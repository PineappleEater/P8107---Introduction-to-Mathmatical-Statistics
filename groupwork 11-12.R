# Find c1 such that P(X < c1) <= 0.1 and as close as possible
# P(X < c1) = P(X <= c1 - 1)
lambda_0 <- 6
lambda_72h <- lambda_0 * 3 # 72-hour period
possible_c1 <- 0:30
alpha_values_test1 <- ppois(possible_c1 - 1, lambda_72h)

# Find the largest c1 where alpha <= 0.1
alpha_target <- 0.1
valid_c1 <- possible_c1[alpha_values_test1 <= alpha_target]
c1 <- max(valid_c1)
alpha_test1 <- ppois(c1 - 1, lambda_72h)

cat(sprintf("c1 = %d\n", c1))
cat(sprintf("Actual alpha for test (i): α = P(X < %d) = P(X <= %d) = %.4f\n\n", c1, c1 - 1, alpha_test1))


# Find c2 such that P(Y < c2) <= 0.1
# Y = minimum of three independent 24-hour observations
# Y < c2  iff  all three observations < c2
# P(Y < c2) = 1 - P(Y >= c2) = 1 - [P(X) >= c2)]^3
# P(Y < c2) = 1 - [1 - P(X < c2)]^3 = 1 - [1 - P(X <= c2 - 1)]^3

possible_c2 <- 0:30
alpha_values_test2 <- 1 - (1 - ppois(possible_c2 - 1, lambda_0))^3


valid_c2 <- possible_c2[alpha_values_test2 <= alpha_target]
c2 <- max(valid_c2)
alpha_test2 <- 1 - (1 - ppois(c2 - 1, lambda_0))^3

cat(sprintf("c2 = %d\n", c2))
cat(sprintf("Actual alpha for test (ii): α = P(Y < %d) = %.4f\n\n", c2, alpha_test2))


# c. if true rate is 5.5, calculate power for both tests
power_test1 <- ppois(c1 - 1, lambda = 5.5 * 3) # Power for test (i)
power_test2 <- 1 - (1 - ppois(c2 - 1, lambda = 5.5))^3 # Power for test (ii)
cat("the power for test (i) is ", power_test1, "\n")
cat("the power for test (ii) is ", power_test2, "\n")
