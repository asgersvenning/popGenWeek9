library(tidyverse)
library(ggforce)
library(magrittr)
library(extrafont)
library(ggpubr)

formatScientific <- function(n, latex = F) sapply(n, function(x) {
  
  if (is.na(x)) return(NA)
  if (x == 0) return("0")
  
  negativeFlag <- sign(x) == -1
  
  x <- abs(x)
  log10Base <- log10(x)
  
  if (log10Base < 3 & log10Base > -2) {
    
    out <- if (log10Base < 0) as.character(signif(x, 2)) else as.character(round(x, 2))
    
  } 
  else {
    
    log10Base <- floor(log10Base)
    x <- x * 10^-log10Base
    
    if (round(x, 2) == 1) {
      out <- paste0("10^{", log10Base, "}")
    } 
    else {
      out <- paste0(round(x, 2), " \\times 10^{", log10Base, "}")
    }
  }
  
  out <- stringr::str_remove(out, "^1 ")
  
  if (negativeFlag) out <- paste0("-", out)
  
  if (!latex) return(
    latex2exp::TeX(paste0("$", out, "$"))
  )
  else return(paste0("$", out, "$"))
}, USE.NAMES = F, simplify = T)

theme_set(
  theme_pubr(base_family = "CMU Serif",
             legend = "right") +
    theme(title = element_text(face = "bold",
                               size = 14),
          plot.title = element_text(face = "plain",
                                    hjust = .5,
                                    size = 18))
)


ArchaicSegments <- read_delim("ArchaicSegments.txt", delim = "\t") %>% 
  select(!2)


ArchaicSegments %>% 
  group_by(name) %>% 
  summarize(
    length = sum(length, na.rm = T)
  ) %>% 
  ggplot(aes(name, length)) +
  geom_col(color = "gray85", fill = "gray85") +
  scale_y_continuous(labels = formatScientific) +
  coord_cartesian(expand = F) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Individual", y = "Length")


ArchaicSegments %>% 
  group_by(pop) %>% 
  summarize(
    length = sum(length, na.rm = T)
  ) %>% 
  ggplot(aes(pop, length)) +
  geom_col(color = "gray85", fill = "gray85") +
  scale_y_log10(labels = formatScientific) +
  coord_cartesian(expand = F,
                  ylim = c(10^7, 10^10)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Population", y = "Length")
