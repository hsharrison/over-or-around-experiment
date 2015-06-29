library(magrittr)
library(plyr)
library(lme4)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts()

e <- read.table('e3.csv', header=TRUE, sep=',')
e$h_max <- e$lowest_height_not_afforded
e$lowest_height_not_afforded <- NULL
e$action <- ordered(e$outbound, c('over', 'around'))
e$order <- as.factor(e$sorted_order)
e$height <- e$height + 1
e$h_max <- e$h_max + 1
e$relative_height <- e$height - e$h_max
e$scaled_height <- e$height / e$h_max
obstacle_distance <- 5
e$tan_angle <- (e$width / 2) / obstacle_distance
e$angle <- tan(e$tan_angle)
# e <- e[e$participant != 2,]

h_crit_glm <- function(d) {
  glm(action ~ height, family=binomial(link='logit'), data = d)
}
p.glm <- function(mdl, p = .5) {
  a <- mdl %>% coef %>% extract(1)
  b <- mdl %>% coef %>% extract(2)
  result <- (log(p /(1-p)) - a) / b
  names(result) <- mdl %>% terms %>% labels
  return(result)
}
sem <- function(x) {
  sd(x) / sqrt(length(x))
}

ddply.fun <- function(d, p = .5) {
  result <- d[1, apply(d, 2, . %>% unique %>% length %>% equals(1))]
  
  if ('action' %in% names(result)) {
    result$switched <- FALSE
    
    if (result[1, 'action'] == 'over') {
      # Always went over. Set switching point to maximum height.
      result$h_crit <- result$h_max    
    } else {
      # Always went around. Set switching point to in-between the lowest height and what the next lowest would be if the sequence continued.
      result$h_crit <- min(d$height) - d$height %>% diff %>% mean %>% abs %>% divide_by(2)
    }
    
  } else {
    result$switched <- TRUE
    result$h_crit <- d %>% h_crit_glm %>% p.glm
  }
  
  result$sorted_order <- NULL
  return(result)
}

data <- ddply(e, .(participant, block), ddply.fun)
data$scaled_height.50 <- data$h_crit / data$h_max
data$rel_height.50 <- data$h_crit - data$h_max

fig.dims <- c(5, 5)
font.family <- 'Ubuntu'
font.size <- 20

widths <- e %>% extract2('width') %>% unique
width_x <- scale_x_continuous(breaks=widths, name='Obstacle width (ft.)')
crit_scaled_height_y <- scale_y_continuous(name=expression(frac(h[crit.], h[max])), limits=c(0.5, 1))
upright_ylabel <- theme(axis.title.y=element_text(angle=0))
theme <- theme_few(base_size=font.size, base_family=font.family)
order_color <- scale_fill_few(palette='light')
p_color <- scale_color_manual(values=c(few_pal('dark')(7), few_pal('medium')(5)), guide='none')
width_offset <- 4

ggplot(data, aes(x=width, y=scaled_height.50, fill=order, group=interaction(width, order))) +
   stat_boxplot(position='dodge', width=8, size=1) +
   stat_summary(aes(group=interaction(width, participant), color=as.factor(participant),
                    x=width + width_offset/2 - width_offset*(order=='ascending')),
                fun.y=mean,
                geom='line',
                size=1.5) +
   width_x + theme + upright_ylabel + order_color + crit_scaled_height_y + p_color
ggsave('hysteresis.png', height = 9, width = 8)

# Modeling.
means <- lm(h_crit ~ 1, data)
h_max <- lm(h_crit ~ h_max, data)
width <- lm(h_crit ~ h_max + width, data)
order <- lm(h_crit ~ h_max + width + order, data)
order_width_int <- lm(h_crit ~ h_max + width*order, data)
anova(means, h_max, width, order, order_width_int)
summary(order)
anova(lm(h_crit ~ h_max*order*width, data))
