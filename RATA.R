# load necessary packages
packages <- c("tidyverse", "data.table", "tidyr", "knitr", "tinytex", "DescTools", "kableExtra", "ggpubr")
missing.packages <- setdiff(packages,installed.packages()[,"Package"])
if(length(missing.packages)) install.packages(missing.packages)
for(pkg in packages) {
  do.call("library",list(pkg))
}

# The round function in R rounds to the nearest even number if the decimal value is .5, so we may not get correct values. 
# The following round function will be used instead.
# inputs:
# - x = value to be rounded
# - n = how many places to round
round2 <- function(x, n) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)

# create function to read in files with appropriate error and warning messages
readfile <- function(file, ...) {
  tryCatch(read_csv(file, ...),
           error = function(e) message("File not found. Make sure file resides in directory with Rmd file.", file),
           warning = function(w) message("File not found. Make sure file resides in directory with Rmd file.", file)
  )
}  

# read in files
so2rata <- readfile("rawdata/SO2RATA.csv")
noxrata <- readfile("rawdata/NOXRATA.csv")
noxrrata <- readfile("rawdata/NOXRRATA.csv")
co2rata <- readfile("rawdata/CO2RATA.csv")
o2rata <- readfile("rawdata/O2RATA.csv")
h2omrata <- readfile("rawdata/H2OMRATA.csv")
h2orata <- readfile("rawdata/H2ORATA.csv")

## SO2 and NOx concentration
# combine SO2 and NOX concentration RATA data
so2nox <- rbind(so2rata, noxrata)

# filter RATAs
so2nox_filtered <- so2nox %>% 
  filter(Mean.RATA.Reference <= 250)

## NOx Rate
# filter RATAs
noxrrata_filtered <- noxrrata %>% 
  filter(Mean.RATA.Reference <= 0.200)

## CO2 and O2 (%)
# combine CO2 and O2 files and filter RATAs
co2o2 <- rbind(co2rata, o2rata)

## Moisture RATA (%H2O)
# combine H2O and H2OM files and filter RATAs
h2omh2o <- rbind(h2omrata, h2orata)

# custom theme
theme_custom <- function (base_size = 10, base_family = "sans") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) %+replace%           # basic theme to start
    theme(
      line = element_line(colour = "black", 
                          size = 0.5, 
                          linetype = 1, 
                          lineend = "butt"), 
      rect = element_rect(fill = "white",                                              # background
                          colour = "black", 
                          size = 0.5, 
                          linetype = 1),
      text = element_text(family = base_family,                                        # text
                          face = "plain",
                          colour = "black", 
                          size = base_size,
                          lineheight = 0.9,  
                          hjust = 0.5,
                          vjust = 0.5, 
                          angle = 0, 
                          margin = margin(), 
                          debug = FALSE), 
      
      axis.line = element_blank(),                                                     # chart border
      axis.text = element_text(size = rel(0.8), colour = "grey20"),                    # axis text
      axis.text.x = element_text(margin = margin(t = 0.8*half_line/2), vjust = 1),     # axis x label text
      axis.text.y = element_text(margin = margin(r = 0.8*half_line/2), hjust = 1),     # axis y label text
      axis.ticks = element_line(colour = "grey20"),                                    # axis tick marks
      axis.ticks.length = unit(half_line/2, "pt"),                                     # axis tick mark length
      axis.title.x = element_text(size = rel(1),  face = "bold",                       # styles the x axis title
                                  margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = rel(1),  face = "bold", angle = 90,           # styles the y axis title
                                  margin = margin(t = 0, r = 15, b = 0, l = 0)),
      
      legend.background = element_rect(fill = "grey92", colour = NA ),                 # legend background and border
      legend.spacing.x = unit(0.2, 'cm'),                                              # category spacing
      legend.key = element_rect(fill = "grey95", colour = "grey85"),                   # category background and border
      legend.key.size = unit(1.2, "lines"),                                            # category item size
      legend.key.height = NULL,                                                        # category item height
      legend.key.width = NULL,                                                         # category item width
      legend.text = element_text(size = rel(0.8)),                                     # legend category item text size
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0), 
      legend.title.align = .5,                                                         # alignment of legend title (0-1 is left-right)
      legend.position = "bottom",                                                      # sets the location of the legend relative to the grid
      legend.direction = NULL,
      legend.justification = "center",                                                 # aligns the legend
      legend.box = NULL, 
      
      panel.background = element_rect(fill = "grey92", colour = NA),                   # sets the chart panel background
      panel.border = element_blank(),                                                  # sets the chart panel border
      panel.grid.major = element_line(colour = "white"),                               # sets the chart grid line
      panel.grid.minor = element_line(colour = "white", size = 0.25), 
      #panel.margin = unit(half_line, "pt"), 
      panel.margin.y = NULL, 
      panel.margin.x = NULL,
      panel.ontop = FALSE, 
      
      strip.background = element_rect(fill = "grey85", colour = NA),
      strip.text = element_text(colour = "grey10", size = rel(0.8)),
      strip.text.x = element_text(margin = margin(t = half_line, b = half_line)), 
      strip.text.y = element_text(angle = -90, 
                                  margin = margin(l = half_line, r = half_line)),
      strip.switch.pad.grid = unit(0.1, "cm"),
      strip.switch.pad.wrap = unit(0.1, "cm"), 
      
      plot.background = element_rect(colour = "white"),                               # sets the figure background color
      plot.title = element_text(size = rel(1.2), face = "bold",                       # styles the chart title
                                hjust = 0.5, vjust = 2.5,
                                margin = margin(b = half_line * 1.2)),
      plot.margin = margin(half_line, half_line, half_line, half_line),
      
      complete = TRUE)
}

# function to determine historical test results
# case_when is evaluated in order of statements
# required function inputs:  
# - df = filtered dataframe  
# - ra_4qtr - relative accuracy for annual evaluation
# - ra_2qtr - relative accuracy for semi-annual evaluation
# - diff_4qtr - mean difference between CEM and reference value for annual evaluation
# - diff_2qtr - mean difference between CEM and reference value for semi-annual evaluation
# output: a new dataframe with a column for testresults

testresults <- function(df, ra_4qtr, ra_2qtr, diff_4qtr, diff_2qtr) {
  df %>%
    mutate(testresult = case_when(
      Relative.Accuracy <= ra_4qtr ~ "PASS_4QTRS",
      Relative.Accuracy > ra_4qtr & between(Mean.Diff, -diff_4qtr, diff_4qtr) ~ "PASSAPS_4QTRS",
      Relative.Accuracy > ra_4qtr & Relative.Accuracy <= ra_2qtr ~ "PASS_2QTRS",
      Relative.Accuracy > ra_2qtr & between(Mean.Diff, -diff_2qtr, diff_2qtr) ~ "PASSAPS_2QTRS",
      TRUE ~ "FAILED"
    )
    )  
  
}


# function to get statistics for RATA mean difference
# required function inputs:  
# - x = vector of values 
# output: statistical values for supplied vector
getstats <- function(x) { 
  stats_table <- data.table(
    mean = mean(x),
    se = MeanSE(x),
    median = median(x),
    mode = paste(Mode(x), collapse=", "),
    sd = sd(x),
    variance = var(x),
    kurtosis = Kurt(x),
    skewness = Skew(x),
    range = Range(x),
    min = min(x),
    max = max(x),
    sum = sum(x),
    count = length(x),
    sd1_low = mean(x) - sd(x),
    sd1_high = mean(x) + sd(x),
    sd1_pct = length(which(x >= (mean(x) - sd(x)) & 
                             x <= (mean(x) + sd(x)))) / length(x) * 100,
    sd2_low = mean(x) - (2*sd(x)),
    sd2_high = mean(x) + (2*sd(x)),
    sd2_pct = length(which(x >= (mean(x) - (2*sd(x))) & 
                             x <= (mean(x) + (2*sd(x))))) / length(x) * 100,
    sd3_low = mean(x) - (3*sd(x)),
    sd3_high = mean(x) + (3*sd(x)),
    sd3_pct = length(which(x >= (mean(x) - (3*sd(x))) & 
                             x <= (mean(x) + (3*sd(x))))) / length(x) * 100
  )
  t(stats_table)
  
}


# function to add a column in each dataframe of passed observations to indicate the RATAs 
# standard deviation. This will be used to color the histograms.
# required function inputs:  
# - df = dataframe
# - dfstats = dataframe of statistical values for df (from getstats function)
# output: dataframe modified to include standard deviations column
addsd <- function(df, dfstats) {
  df %>% mutate(sd = factor(case_when(
    Mean.Diff >= dfstats$sd1_low & Mean.Diff <= dfstats$sd1_high ~ "SD1",
    Mean.Diff >= dfstats$sd2_low & Mean.Diff <= dfstats$sd2_high ~ "SD2",
    Mean.Diff >= dfstats$sd3_low & Mean.Diff <= dfstats$sd3_high ~ "SD3",
    TRUE ~ "OTHER"
  ), levels = c("SD1","SD2","SD3","OTHER"))) %>% 
    arrange(sd) -> df
}

## Statistical Evaluation ## 
## SO2 and NOx (ppm)
# Mean Difference for RATAs that passed
# keep only those observations that passed
#so2nox_passed <- so2nox_results %>% filter(testresult !="FAILED") 

# statistical evaluation of all that passed
# function call: getstats(x)
#so2nox_stats <- getstats(so2nox_passed$Mean.Diff) 

so2nox_results <- testresults(so2nox_filtered, 7.5, 10, 12, 15)
so2nox_passed <- so2nox_results %>% filter(testresult !="FAILED")
getstats(so2nox_passed$Mean.Diff) 

