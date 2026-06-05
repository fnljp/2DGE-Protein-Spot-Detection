# load renv
renv::load('path') # Point to the project folder

# import packages
library('ggplot2', 'ggsignif')

# function that generates plots
AILS_plot <- function(data, y_label, significance) {
  ggplot(data=data, aes(x=method, y=value, ymin=value-sd, ymax=value+sd)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=15, color='black'), 
             axis.text.y = element_text(size=15, color='black'),
             axis.title.x = element_text(size=20),
             axis.title.y = element_text(size=20)) +
    geom_bar(stat='identity', fill=c('darkgray', 'lightgray'), width=0.3) +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    labs(x='Method', y=y_label) +
    geom_errorbar(width=0.05, colour="black", alpha=0.9, linewidth=0.5) +
    # ggsignif::geom_signif(stat="signif",position="identity", comparisons=list(c("Model","CellProfiler")), map_signif_level=TRUE, annotations=significance, textsize=10, vjust=0.6) +
    geom_text(aes(y=0, label = paste("n = ", 2)), vjust=1.2, size=4.5)
}

# sensitivity
sensitivity_data <- data.frame(value=c(39, 34.45), method=c('Model', 'CellProfiler'), sd=c(17.71, 11.28))
sensitivity_plot <- AILS_plot(data=sensitivity_data, y_label='Sensitivity (%)')

# f-measure
fmeasure_data <- data.frame(value=c(43.23, 47.13), method=c('Model', 'CellProfiler'), sd=c(14, 13.62))
fmeasure_plot <- AILS_plot(data=fmeasure_data, y_label='F-Measure (%)')

# precision
precision_data <- data.frame(value=c(49.92, 75), method=c('Model', 'CellProfiler'), sd=c(6.958, 15.15))
precision_plot <- AILS_plot(data=precision_data, y_label='Precision (%)')