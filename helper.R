# load necessary packages
#packages <- c("tidyverse", "data.table", "tidyr", "knitr", "tinytex", "DescTools", "kableExtra", "ggpubr","rsconnect")
#missing.packages <- setdiff(packages,installed.packages()[,"Package"])
#if(length(missing.packages)) install.packages(missing.packages)
#for(pkg in packages) {
#  do.call("library",list(pkg))
#}

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
    mean = round2(mean(x),4),
    se = round2(MeanSE(x),4),
    median = round2(median(x),4),
    mode = paste(Mode(x), collapse=", "),
    sd = round2(sd(x),4),
    variance = round2(var(x),4),
    kurtosis = round2(Kurt(x),4),
    skewness = round2(Skew(x),4),
    range = round2(Range(x),4),
    min = round2(min(x),4),
    max = round2(max(x),4),
    count = round2(length(x),4),
    sd1_low = round2(mean(x) - sd(x),4),
    sd1_high = round2(mean(x) + sd(x),4),
    sd1_pct = round2(length(which(x >= (mean(x) - sd(x)) & 
                                    x <= (mean(x) + sd(x)))) / length(x) * 100, 4),
    sd2_low = round2(mean(x) - (2*sd(x)),4),
    sd2_high = round2(mean(x) + (2*sd(x)),4),
    sd2_pct = round2(length(which(x >= (mean(x) - (2*sd(x))) & 
                                    x <= (mean(x) + (2*sd(x))))) / length(x) * 100,4),
    sd3_low = round2(mean(x) - (3*sd(x)),4),
    sd3_high = round2(mean(x) + (3*sd(x)),4),
    sd3_pct = round2(length(which(x >= (mean(x) - (3*sd(x))) & 
                                    x <= (mean(x) + (3*sd(x))))) / length(x) * 100,4)
  ) 
  
  stats_table <- stats_table %>% mutate(sd1_range = str_c("(",sd1_low,", ",sd1_high,")"),
                                        sd2_range = str_c("(",sd2_low,", ",sd2_high,")"),
                                        sd3_range = str_c("(",sd3_low,", ",sd3_high,")")) %>%
    select(mean, "standard error" = se, median, mode, "standard deviation" = sd, variance, 
           kurtosis, skewness, range, "minimum" = min, "maximum" = max,  
           "number of RATAs" = count, "1 standard deviation" = sd1_range, 
           "% RATAs in 1 standard deviation" = sd1_pct, "2 standard deviations" = sd2_range, 
           "% RATAs in 2 standard deviations" = sd2_pct, "3 standard deviations" = sd3_range, 
           "% of RATAs in 3 standard deviations" = sd3_pct)
  
  
  stats_table <- data.table(Variable = names(stats_table), transpose(stats_table)) %>% 
    setnames("V1","Value") 
  
  return(stats_table)
  #%>% 
  #mutate(Value = round2(as.numeric(Value),2))
  #apply(df$Value, 2, round(digits = 2))
  #data.table(transpose(stats_table)) #%>% setnames(c(""))
  
}

#getstats(so2nox_passed$Mean.Diff)


# function to add a column in each dataframe of passed observations to indicate the RATAs 
# standard deviation. This will be used to color the histograms.
# required function inputs:  
# - df = dataframe
# - dfstats = dataframe of statistical values for df (from getstats function)
# output: dataframe modified to include standard deviations column
addsd <- function(df) {
  df %>% mutate(sd = factor(case_when(
    Mean.Diff >= round2(mean(Mean.Diff) - sd(Mean.Diff),4) & Mean.Diff <= round2(mean(Mean.Diff) + sd(Mean.Diff),4) ~ "SD1",
    Mean.Diff >= round2(mean(Mean.Diff) - (2*sd(Mean.Diff)),4) & Mean.Diff <= round2(mean(Mean.Diff) + (2*sd(Mean.Diff)),4) ~ "SD2",
    Mean.Diff >= round2(mean(Mean.Diff) - (3*sd(Mean.Diff)),4) & Mean.Diff <= round2(mean(Mean.Diff) + (3*sd(Mean.Diff)),4) ~ "SD3",
    TRUE ~ "OTHER"
  ), levels = c("SD1","SD2","SD3","OTHER"))) %>% 
    arrange(sd) -> df
}

# create function to plot distribution with boxplot and histogram
# required function inputs:  
# - df = filtered dataframe 
# - var = variable used in plot
# - bins = # of bins for histogram
# - title - title for the plot
# output: a box plot and histogram plot of supplied variable
plotdist <- function(df,var,bins,title) {
  var <- enquo(var)   # convert unquoted var to a quoted one
  p1 <- ggplot(df, aes(x = "", y = !! var)) +    # !! required to unquote var
    stat_boxplot(geom = "errorbar", width = 0.4) + 
    geom_boxplot(outlier.colour = "#F15A60") +
    coord_flip() + 
    stat_summary(fun.y = mean, colour = "#9E67AB", geom = "point", shape = 18, size = 3) + 
    labs(x = "", y = "") + 
    theme_set(theme_custom()) + 
    ggtitle(title)
  
  
  p2 <- ggplot(df, aes(x=Mean.Diff, fill = sd)) + 
    geom_histogram(bins=bins, color="#3b3b3b", size = 0.5) + 
    geom_vline(aes(xintercept=mean(Mean.Diff)), color="blue", linetype="solid", size=1)  +
    xlab("Mean Difference") + 
    ylab("Frequency") + 
    theme_set(theme_custom()) +
    scale_fill_manual(values=c("#7AC36A","#5A9BD4","#FAA75B","#F15A60"), 
                      limits=c("SD1","SD2","SD3","OTHER")) +
    labs(fill="Standard Deviations")
  
  
  ggarrange(p1,p2,heights = c(.75, 2), align = "v", ncol = 1, nrow = 2)
}

# Results tables
# current results
display_cur_results <- function(df) {
  display_tbl <- data.table(
    numratas = df %>% nrow(),
    numpass4 = df %>% filter(testresult=="PASS_4QTRS") %>% nrow(),
    numpassaps4 = df %>% filter(testresult=="PASSAPS_4QTRS") %>% nrow(),
    numpass2 = df %>% filter(testresult=="PASS_2QTRS") %>% nrow(),
    numpassaps2 = df %>% filter(testresult=="PASSAPS_2QTRS") %>% nrow(),
    numfailed = df %>% filter(testresult=="FAILED") %>% nrow()
  )
    display_tbl %>% setnames(old = c('numratas','numpass4','numpassaps4','numpass2','numpassaps2','numfailed'), 
                             new = c('# RATAs','# Pass 4QTRS', '# Pass APS 4QTRS','# Pass 2QTRS','# Pass APS 2QTRS', '# Failed')
                            )
  
  return(display_tbl)
}

# proposed results
display_prop_results <- function(df) {
  display_tbl <- data.table(
    numratas = df %>% nrow(),
    numproppass4 = df %>% filter(propresults=="PASS_4QTRS") %>% nrow(),
    numproppassaps4 = df %>% filter(propresults=="PASSAPS_4QTRS") %>% nrow(),
    numproppass2 = df %>% filter(propresults=="PASS_2QTRS") %>% nrow(),
    numproppassaps2 = df %>% filter(propresults=="PASSAPS_2QTRS") %>% nrow(),
    numpass = df %>% filter(!propresults=="FAILED") %>% nrow(),
    percpass = round2((df %>% filter(!propresults=="FAILED") %>% nrow())/(df %>% nrow())*100,2),
    numfailed = df %>% filter(propresults=="FAILED") %>% nrow(),
    numpasstofail = (df %>% filter(testresult !="FAILED") %>% filter(propresults=="FAILED") %>% nrow()),
    num4to2 = (df %>% filter(RATA.Frequency=="2QTRS" | RATA.Frequency=="4QTRS") %>% 
                 filter(testresult=="PASS_4QTRS" | testresult=="PASSAPS_4QTRS") %>%
                 filter(propresults=="PASS_2QTRS" | propresults=="PASSAPS_2QTRS") %>% nrow()),
    perc4to2 = round2(((df %>% filter(RATA.Frequency=="2QTRS" | RATA.Frequency=="4QTRS") %>%
                          filter(testresult=="PASS_4QTRS" | testresult=="PASSAPS_4QTRS") %>%
                          filter(propresults=="PASS_2QTRS" | propresults=="PASSAPS_2QTRS") %>% 
                          nrow()))/(df %>% nrow())*100,2)
  )
  display_tbl %>% setnames(old = c('numratas','numproppass4','numproppassaps4','numproppass2','numproppassaps2','numpass','percpass',
                                   'numfailed','numpasstofail','num4to2','perc4to2'), 
                           new = c('# RATAs','# Pass 4QTRS', '# Pass APS 4QTRS','# Pass 2QTRS','# Pass APS 2QTRS', 
                                   '# Pass','% Pass','# Failed','# Change from Pass to Fail',
                                   '# Change from 4QTR to 2QTR','% Change from 4QTR to 2QTR'))
  
  return(display_tbl)
    
}


