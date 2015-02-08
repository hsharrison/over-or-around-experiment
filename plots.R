library(ggplot2)
library(ggthemes)
library(magrittr)
library(plyr)
library(binom)

p <- read.table('props.csv', header=TRUE, sep=',')
names(p) <- c('rel_height', 'width', 'p_around')

e <- na.omit(read.table('e1.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
e$rel_height <- e$relative_height

e.totals <- ddply(e, .(rel_height, width), function(x) data.frame(total=nrow(x), around=sum(x$action == 'around')))
e.totals$prop <- e.totals$around / e.totals$total

e.confints <- with(e.totals, binom.confint(around, total, .95, method='exact'))
e.totals$ci.low <- e.confints$lower
e.totals$ci.high <- e.confints$upper


widths <- unique(e.totals$width)
rel_heights <- unique(e.totals$rel_height)

plot_theme <- function(g) {g +
  theme_pander()
}
width_color_scale <- function(g) {g +
  scale_color_gradient(low='light green', high='#242424', name='width (ft.)', breaks=widths)
}
p_over_color <- function(g) {g + 
  scale_fill_gradient2(low='#B21826', high='#2166AC', mid='black', midpoint=.5, name='P(over)')
}
rel_height_color <- function(g) {g +
  scale_color_gradient(high='#242424', low='#2497b7', name='rel. height (in.)', breaks=rel_heights) +
  scale_fill_gradient(high='#242424', low='#2497b7', name='rel. height (in.)', breaks=rel_heights)
}
rel_height_x <- function(g) {g +
  scale_x_continuous(breaks=rel_heights, name='obstacle height (in. relative to max)')
}
scaled_height_x <- function(g) {g +
  xlab('obstacle height (% of max)')
}
width_x <- function(g) {g +
  scale_x_continuous(breaks=widths, name='width (ft.)')
}
p_y <- function(g) {g + 
  ylab('P(around)')
}
p_over_y <- function(g) {g +
  ylab('P(over)')
}
binomial_y <- function(g) {g +
  scale_y_continuous(breaks=c(0, 1), labels=c('over', 'around'), name='action')
}
rel_height_y <- function(g) {g +
  scale_y_continuous(breaks=rel_heights, name='obstacle height (in. relative to max)')
}

g <- ggplot(e.totals, aes(x=rel_height, y=prop, ymin=ci.low, ymax=ci.high, color=width, group=width)) +
  geom_line(size=2) + geom_point(size=3) + geom_errorbar(width=2, size=1, position=position_dodge(width=1.75))
g %>%
  rel_height_x %>% p_y %>% width_color_scale %>%
  plot_theme
ggsave('proportions.pdf')


g <- ggplot(e, aes(y=as.numeric(action)-1, x=rel_height, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=1.15)) +
  stat_smooth(method='glm', family=binomial(link='logit'), se=FALSE, size=2, fullrange=TRUE)
g %>%
  rel_height_x %>% binomial_y %>% width_color_scale %>%
  plot_theme
ggsave('every_trial.pdf')


 g <- ggplot(e, aes(y=as.numeric(action)-1, x=scaled_height, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=0.2)) +
  stat_smooth(method='glm', family=binomial(link='logit'), se=FALSE, size=2, fullrange=TRUE)
g %>%
  scaled_height_x %>% binomial_y %>% width_color_scale %>%
  plot_theme
ggsave('every_trial_scaled.pdf')

g <- ggplot(e.totals, aes(x=width, y=1-prop, ymax=1-ci.low, ymin=1-ci.high, color=rel_height, group=rel_height, fill=rel_height)) +
  geom_line(size=2) + geom_point(size=3) + geom_errorbar(width=0) + geom_ribbon(alpha=0.3)
g %>%
  width_x %>% p_over_y %>% rel_height_color %>%
  plot_theme
ggsave('by-width.pdf')

g <- ggplot(e.totals, aes(x=width, y=rel_height, fill=1-prop)) + geom_tile()
g %>% 
  width_x %>% rel_height_y %>% p_over_color %>%
  plot_theme
ggsave('tile.pdf')