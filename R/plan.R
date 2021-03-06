plan <- drake_plan(
  ## Exposure data
  exposure_annual = readAnnualExposure(),
  exposure_daily = readDailyExposure(),
  pm25_var_aim1 = getAnnualVariance(exposure_annual, model = "apache"),
  pm25_var_aim2 = getDailyVariance(exposure_daily),

  ## Incidence
  n_population = 5000000,
  n_cases = 115000,
  hospitalizations = 32000,
  deaths = 13000,

  ## Aim 1: Long-term exposure
  hospi_power_aim1 = calcPowerDefault(
    n = n_population, events = hospitalizations, sigma2 = pm25_var_aim1,
    hr.min = 1, hr.max = 1.06
  ),
  hospi_plot_aim1 = plotPowerDefault(hospi_power_aim1),
  dead_power_aim1 = calcPowerDefault(
    n = n_population, events = deaths, sigma2 = pm25_var_aim1,
    hr.min = 1, hr.max = 1.06
  ),
  dead_plot_aim1 = plotPowerDefault(dead_power_aim1),

  ## Aim 2: Short-term exposure
  hospi_power_aim2 = calcPowerDefault(
    n = n_cases, events = hospitalizations, sigma2 = pm25_var_aim2,
    hr.min = 1, hr.max = 1.06
  ),
  hospi_plot_aim2 = plotPowerDefault(hospi_power_aim2),
  dead_power_aim2 = calcPowerDefault(
    n = n_cases, events = deaths, sigma2 = pm25_var_aim2,
    hr.min = 1, hr.max = 1.06
  ),
  dead_plot_aim2 = plotPowerDefault(dead_power_aim2),
  power_report = rmarkdown::render(
                              input = knitr_in("reports/power.Rmd"),
                              output_file = "power.html")
)
