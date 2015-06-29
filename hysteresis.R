library(magrittr)
library(plyr)
library(lme4)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts()

e <- read.table('e3.csv', header=TRUE, sep=',')
e$action <- ordered(e$outbound, c('over', 'around'))
e$order <- as.factor(e$sorted_order)
e$height <- e$height + 1
e$lowest_height_not_afforded <- e$lowest_height_not_afforded + 1
e$relative_height <- e$height - e$lowest_height_not_afforded
e$scaled_height <- e$height / e$lowest_height_not_afforded
obstacle_distance <- 5
e$tan_angle <- (e$width / 2) / obstacle_distance
e$angle <- tan(e$tan_angle)
# e <- e[e$participant != 2,]

simple_glm <- function(d, predictor) {
  glm(as.formula(paste('action', predictor, sep='~')), family=binomial(link='logit'), data=d)
}
p.glm <- function(mdl, p=.5) {
  a <- mdl %>% coef %>% extract(1)
  b <- mdl %>% coef %>% extract(2)
  result <- (log(p /(1-p)) - a) / b
  names(result) <- mdl %>% terms %>% labels
  return(result)
}
sem <- function(x) {
  sd(x) / sqrt(length(x))
}

ddply.fun <- function(d, predictor, p=.5) {
  result <- d[1, apply(d, 2, . %>% unique %>% length %>% equals(1))]
  new_col_name <- paste(predictor, p %>% as.character %>% strsplit('.', fixed=TRUE) %>% extract2(1) %>% extract(2), sep='.')
  
  if ('action' %in% names(result)) {
    result$switched <- FALSE
    
    if (result[1, 'action'] == 'over') {
      # Always went over. Set switching point to maximum height.
      result[[new_col_name]] <- d[d$relative_height == 0, predictor]
    } else {
      # Always went around. Set switching point to in-between the lowest height and what the next lowest would be if the sequence continued.
      height <- min(d$height) - d$height %>% diff %>% mean %>% abs %>% divide_by(2)
      if (predictor == 'scaled_height') {
        result[[new_col_name]] <- height / result$lowest_height_not_afforded
      } else if (predictor == 'relative_height') {
        result[[new_col_name]] <- height - result$lowest_height_not_afforded
      }
    }
    
  } else {
    result$switched <- TRUE
    result[[new_col_name]] <- d %>% simple_glm(predictor) %>% p.glm(p=p)
  }
  
  result$sorted_order <- NULL
  return(result)
}

rel_height.50 <- ddply(e, .(participant, block), . %>% ddply.fun('relative_height'))
scaled_height.50 <- ddply(e, .(participant, block), . %>% ddply.fun('scaled_height'))

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

ggplot(scaled_height.50, aes(x=width, y=scaled_height.5, fill=order, group=interaction(width, order))) +
   stat_boxplot(position='dodge', width=8, size=1) +
   stat_summary(aes(group=interaction(width, participant), color=as.factor(participant),
                    x=width + width_offset/2 - width_offset*(order=='ascending')),
                fun.y=mean,
                geom='line',
                size=1.5) +
   width_x + theme + upright_ylabel + order_color + crit_scaled_height_y + p_color
