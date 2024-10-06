Homework 2
================
Mari Sanders

# Problem 1

## a)

This can be solved using a binomial distribution because there is a
fixed number of trials and there are only two possible outcomes, either
the individual had at least 1 dental checkup or not.

To solve this problem, we then know that X ~Binomial(56,0.73). The
formula of a binomial distribution is
$P(X= x) = {n \choose x}p^x(1-p)^{n-x} = \frac{n!}{x!(n-x)!}p^x(1-p)^{n-x}$

In this case, we have to do
$P(X = 40)= {56 \choose 40}0.73^{40}(1-(0.73))^{56-40} = \frac{56!}{40!(56-40)!}(0.73)^{40}(1-0.73)^{56-40} = 0.1133$

## b)

$P(X \geq 40) = 1-P(X \leq 39)$

``` r
at_most_39 <- pbinom(q = 39, size = 56, prob = 0.73)

at_least_40 <- 1 - at_most_39
```

$P(X \geq 40) = 0.6678734$

## c)

It would not be good to approximate this with a Poisson distribution
because n = 56, which is not greater than 100. Also the probability of
success is 0.73, and it should be less than 0.01 for Poisson to be a
good approximation.

However, a normal approximation would be valid for this situation
because $np = (0.73)(56)=40.88\geq 10$ and
$n(1-p) = 56(1-0.73) = 15.12 \geq 10$. Therefore, we can approximate
this as a normal distribution.

We need to find the expected value and standard deviation of the normal
distribution

$E(X) = \mu = np = (0.73)(56) = 40.88$

$Var(x) = \sigma = \sqrt{np(1-p)} = \sqrt{(0.73)(56)(1-0.73)} = 12.9185337$

``` r
mu <- (0.73)*(56) 
sd <- sqrt(0.73)*(56)*(1-0.73)
pnorm(40, mean = mu, sd = sd)
```

    ## [1] 0.4728454

``` r
1 - pnorm(40, mean = mu, sd = sd)
```

    ## [1] 0.5271546

## d)

$E(X) = \mu = np = (0.73)(56) = 40.88$

I expect about 41 individuals to have at least one dental checkup.

## e)

$Var(x) = \sigma = \sqrt{np(1-p)} = \sqrt{(0.73)(56)(1-0.73)} = 12.9185337$

The standard deviation of the number of individuals who will have at
least one dental checkup is about 14.

# Problem 2

## a)

Poisson Distribution: $P(X = x) = \frac{\lambda^xe^{-\lambda}}{x!}$

Then $P(X < 3) = P(X = 0) + P(X = 1) + P(X = 2)$

$P(X = 0) = \frac{6^0e^{-6}}{0!} = .0025$

$P(X = 1) = \frac{6^1e^{-6}}{1!} = 0.0145$

$P(X = 2) = \frac{6^2e^{-6}}{2!} = 0.045$

$P(X < 3) = 0.0025 + 0.0145 + 0.045 = 0.062$

Want to find $P(X < 3)$

``` r
fewer_than_3_tornadoes <- ppois(2, lambda = 6)
```

The probability of seeing less than 3 tornadoes is 0.0619688

## b)

Want to find $P(X = 3)$

``` r
exactly_3_tornadoes <- dpois(3, lambda = 6)
```

The probability of having exactly 3 tornadoes is 0.0892351

## c)

Want to find $P(X \geq 3)$

``` r
three_or_less_tornades <- ppois(3, lambda = 6)
more_than_3_tornadoes <- 1 - fewer_than_3_tornadoes
```

The probability of having more than three tornadoes is 0.9380312

# Problem 3

## a)

$H_0: \mu = 137$ $H_A: \mu > 137$

``` r
pop_mean <- 128
pop_sd <- 10.2
probability_greater_137 <-  1 - pnorm(137, mean = pop_mean, sd = pop_sd)
```

The probability that a randomly selected American male between 20 and 29
has a systolic blood pressure above 137.0 is 0.188793

## b)

``` r
n <- 50
z_score <- (125 - 128)/(10.2/sqrt(50))
prob_less_125 <- pnorm(z_score)
```

The probability that the sample mean for blood pressure of 50 males
between 20 and 29 years old will be less than 125 is 0.0187753.

## c)

``` r
z_90 <- qnorm(0.90)
n <- 40

percentile_90 <- 
  pop_mean + z_90 * (pop_sd/sqrt(n))
```

The 90th percentile of the sampling distribution of the sample mean X
for a sample size of 40 is 130.0668372

# Problem 4

## a)

``` r
sample_mean <- 80 
sample_sd <- 10
n <- 40 

SE <- sample_sd / sqrt(n)

critical_val <- qt(1-(0.05/2),df = 39)

margin_of_error <- critical_val*SE

lower_bound <- sample_mean - margin_of_error
lower_bound
```

    ## [1] 76.80184

``` r
upper_bound <- sample_mean + margin_of_error
upper_bound
```

    ## [1] 83.19816

## b)

We are 95% confident that the mean pulse of young women suffering from
fibromyalgia falls between (76.8018448, 83.1981552)

## c)

$H_0: \mu = 70$ $H_A: \mu \neq 70$

``` r
critical_val_2 <- qt(1-(0.01/2), df = 39)

t_statistic <- (sample_mean - 70)/ sample_sd

p_value <- 2 * (1 - pt(abs(t_statistic), df = 39))
abs(t_statistic) > critical_val
```

    ## [1] FALSE

Fail to reject the null hypothesis that $\mu=70$. This means that the
mean pulse of young women suffering from fibromyalgia is equal to 70.
