
library(TrialSimulator)

#' define three arms
pbo <- arm(name = 'placebo')
low <- arm(name = 'low dose')
high <- arm(name = 'high dose')

#' define endpoints in placebo
eps <- endpoint(name = c('pfs', 'os'), type = c('tte', 'tte'),
                generator = CorrelatedPfsAndOs3, 
                h01 = .06, h02 = .30, h12 = .30)

pbo$add_endpoints(eps)

#' define endpoints in low dose arm
eps <- endpoint(name = c('pfs', 'os'), type = c('tte', 'tte'),
                generator = CorrelatedPfsAndOs3, 
                h01 = .06, h02 = .30, h12 = .30)

low$add_endpoints(eps)

#' define endpoints in high dose arm
eps <- endpoint(name = c('pfs', 'os'), type = c('tte', 'tte'),
                generator = CorrelatedPfsAndOs3, 
                h01 = .06, h02 = .30, h12 = .30)

high$add_endpoints(eps)

accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 40,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rweibull, shape = 2.139, scale = 38.343,
  silent = TRUE
)

trial$add_arms(sample_ratio = c(1, 1, 1), low, high, pbo)

#' action1 is defined in the Action editor — do not redefine it here.
#' Set "Function name in main code" to action1 before running.

dose_selection <- milestone(name = 'dose selection', action = action1,
                            when = eventNumber(endpoint = 'pfs', n = 200)
                            )

interim <- milestone(name = 'interim', action = action2,
                     when = eventNumber(endpoint = 'pfs', n = 300)
                     )

final <- milestone(name = 'final',
                   when = enrollment(n = 1000, arms = c('placebo', 'low dose', 'high dose')) &
                     eventNumber(endpoint = 'os', n = 300) & (
                       calendarTime(time = 28) |
                         eventNumber(endpoint = 'pfs', n = 520)
                       )
                   )

listener <- listener(silent = TRUE)
listener$add_milestones(
  dose_selection,
  interim
)

controller <- controller(trial, listener)
controller$run(silent = TRUE)
