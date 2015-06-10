library("ggplot2")
library("reshape2")
library("grid")
library("gridExtra")

get_mat <- function(mat_res, ss, methods, scale, dontplot) {
  out <- mat_res[mat_res$sample_size %in% ss, c("sample_size", methods)]
  out <- melt(out, id.vars = "sample_size", variable.name = "method")
  out$method <- as.character(out$method)
  out$value <- out$value / scale
  
  out$nopoint <- is.na(out$value)
  for(m in unique(out$method[is.na(out$value)])) {
    tmp_lm <- lm(value ~ sample_size + I(sample_size^2), data = out[out$method == m, ])
    out$value[out$method == m & is.na(out$value)] <- predict(tmp_lm, out[out$method == m & is.na(out$value), ])
  }
  
  out$nopoint[out$sample_size %in% dontplot] <- TRUE
  
  out
}

load("results.RData")

cpu_res <- cpu_res[cpu_res$covs == 2, ]
mem_res <- mem_res[mem_res$covs == 2, ]
cpu_res$covs <- mem_res$covs <- NULL

# Small graph
methods_small <- c("aoULAV", "aoD2AG", "moore", "greedy", "nbpm")
ss_small <- c(1e1, 25e2, 50e2, 75e2, 1e4, 125e2, 150e2, 175e2, 2e4,
              225e2, 250e2, 275e2, 300e2, 325e2, 350e2, 375e2, 400e2, 5e4)
cpu_small <- get_mat(cpu_res, ss_small, methods_small, 60, 1e1)
mem_small <- get_mat(mem_res, ss_small, methods_small, 1024 * 1024, 1e1)

# Adjust so aoULAV and aoD2AG not overlap
adjust_factor <- 0.0075
cpu_small$value[cpu_small$method == "aoULAV"] <- cpu_small$value[cpu_small$method == "aoULAV"] - 61 * adjust_factor
cpu_small$value[cpu_small$method == "aoD2AG"] <- cpu_small$value[cpu_small$method == "aoD2AG"] + 61 * adjust_factor
mem_small$value[mem_small$method == "aoULAV"] <- mem_small$value[mem_small$method == "aoULAV"] - 49 * adjust_factor
mem_small$value[mem_small$method == "aoD2AG"] <- mem_small$value[mem_small$method == "aoD2AG"] + 49 * adjust_factor

# Large graph
methods_large <- c("aoULAV", "aoD2AG")
ss_large <- c(1e1, 1e6, 1e7, 2e7, 3e7, 4e7,
              5e7, 6e7, 7e7, 8e7, 9e7, 1e8)
cpu_large <- get_mat(cpu_res, ss_large, methods_large, 60, c(1e1, 1e6))
mem_large <- get_mat(mem_res, ss_large, methods_large, 1024 * 1024, c(1e1, 1e6))

#ss_log <- c(1e2, 18e1, 32e1, 56e1, 1e3, 18e2, 32e2, 56e2, 
#            1e4, 18e3, 32e3, 56e3, 1e5, 18e4, 32e4, 56e4, 
#            1e6, 18e5, 32e5, 56e5, 1e7, 18e6, 32e6, 56e6, 1e8)
#cpu_log <- get_mat(cpu_res, ss_log, methods_small, 60, NULL)
#mem_log <- get_mat(mem_res, ss_log, methods_small, 1024 * 1024, NULL)

palette <- c(aoULAV = "#E41A1C",
             aoD2AG = "#377EB8",
             moore = "#4DAF4A",
             greedy = "#34495E",
             nbpm = "#FF7F00")
legend_title <- "Algorithms"
legend_labels <- c(aoULAV = "Approximation algorithm",
                   aoD2AG = "Improvements 1-3",
                   moore = "Greedy (fixed)",
                   greedy = "Greedy (threshold)",
                   nbpm = "Non-bipartite matching")
legend_order <- c("aoULAV", "aoD2AG", "moore", "greedy", "nbpm")

make_plot <- function(data, small, cpu, annotation, smooth_span) {
  out_plot <- ggplot(aes(x = sample_size, y = value, colour = method, shape = method, fill = method), data = data) +
    geom_line(stat = "smooth", method = "loess", span = smooth_span, size = 0.8, alpha = 0.4) + 
    geom_point(data = data[!data$nopoint, ], size = 2) +
    scale_colour_manual(values = palette,
                        labels = legend_labels, breaks = legend_order, name = legend_title) + 
    scale_fill_manual(values = palette,
                      labels = legend_labels, breaks = legend_order, name = legend_title) + 
    scale_shape_manual(values = c(aoULAV = 22, aoD2AG = 24, moore = 21, greedy = 23, nbpm = 25),
                       labels = legend_labels, breaks = legend_order, name = legend_title) + 
    theme_bw() + theme(panel.grid.major.x = element_line(size = 0.25, color = "#EEEEEE"),
                       panel.grid.major.y = element_line(size = 0.25, color = "#EEEEEE"),
                       panel.grid.minor.x = element_blank(),
                       panel.grid.minor.y = element_blank(),
                       axis.title.x = element_text(vjust = 0),
                       legend.background = element_rect(size = 0.5, colour = "#CCCCCC"),
                       legend.key = element_blank(),
                       legend.position = "none")
  if (small) {
    out_plot <- out_plot +
      scale_x_continuous(name="Data points (in thousands)",
                         breaks = c(0, 1e4L, 2e4L, 3e4L, 4e4L),
                         labels = c("0", "10k", "20k", "30k", "40k"))
    xlim <- c(0, 41e3L)
    ann_x <- 3000
  } else {
    out_plot <- out_plot +
      scale_x_continuous(name="Data points (in millions)",
                         breaks = c(0, 2e7L, 4e7L, 6e7L, 8e7L, 1e8L),
                         labels = c("0", "20M", "40M", "60M", "80M", "100M")) +
      theme(axis.title.y = element_blank())
    xlim <- c(0, 102e6L)
    ann_x <- 85e5
  }
  if (cpu) {
    out_plot <- out_plot +
      scale_y_continuous(name="Minutes",
                         breaks = c(0, 10, 20, 30, 40, 50, 60))
    ylim <- c(-1.25, 60)
    ann_y <- 55
  } else {
    out_plot <- out_plot +
      scale_y_continuous(name="Gigabytes",
                         breaks = c(0, 10, 20, 30, 40, 48))
    ylim <- c(-1, 48)
    ann_y <- 44
  }
  out_plot + coord_cartesian(xlim = xlim, ylim = ylim) +
    annotate("text", x = ann_x, y = ann_y, label = annotation, size = 6)
}

small_cpu <- make_plot(cpu_small, TRUE, TRUE, "A", 0.45)
large_cpu <- make_plot(cpu_large, FALSE, TRUE, "B", 0.7)
small_mem <- make_plot(mem_small, TRUE, FALSE, "C", 0.45) 
large_mem <- make_plot(mem_large, FALSE, FALSE, "D", 0.7) 

legend_grob <- ggplotGrob(small_cpu + theme(legend.justification=c(1,1), legend.position = c(1,1)))$grobs
legend_grob <- legend_grob[[which(sapply(legend_grob, function(x) x$name) == "guide-box")]]
large_cpu <- ggplotGrob(large_cpu + theme(legend.justification=c(1,1), legend.position = c(1,1)))
large_cpu$grobs[[which(sapply(large_cpu$grobs, function(x) x$name) == "guide-box")]] <- legend_grob

all_plots <- arrangeGrob(arrangeGrob(small_cpu, large_cpu, nrow = 1,
                                     main = textGrob("Run time in minutes", vjust = 0.6, gp = gpar(fontface = "bold", cex = 1.25))),
                         rectGrob(gp=gpar(col = NA)),
                         arrangeGrob(small_mem, large_mem, nrow = 1,
                                     main = textGrob("Memory usage in gigabytes", vjust = 0.6, gp = gpar(fontface = "bold", cex = 1.25))),
                         nrow = 3, heights = unit.c(unit(0.48, "npc"), unit(0.04, "npc"), unit(0.48, "npc")))

#grid.draw(all_plots)

ggsave("latex/complexity.pdf", all_plots, scale = 2,
       width = 11.4, height = 12, units = "cm")
