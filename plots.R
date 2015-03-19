library(ggplot2)
library(ggthemes)
library(magrittr)
library(plyr)
library(binom)
library(directlabels)
library(extrafont)
loadfonts()

obstacle_distance <- 5
angle_coefficient <- 0.17

font.family <- 'Ubuntu'

p <- read.table('e2-props.csv', header=TRUE, sep=',')
names(p) <- c('rel_height', 'width', 'p_around')

e <- na.omit(read.table('e2.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
e$rel_height <- e$relative_height
e$tan_angle <- (e$width / 2) / obstacle_distance
e$angle <- atan(e$angle)
e$mutual_scale <- with(e, scaled_height - angle_coefficient * tan_angle)

n_return_actions <- with(subset(e, e$phase == 'return'), tapply(action, list(participant), . %>% unique %>% length))
drop_return_participants <- names(n_return_actions[n_return_actions == 1])
e <- e[!(e$phase == 'return' & e$participant %in% drop_return_participants),]

n_trials <- e$trial %>% unique %>% length
e <- e[!with(e, participant == 15 & phase == 'outbound' & trial <= n_trials/2), ]

e.totals <- ddply(e, .(rel_height, width), function(x) data.frame(total=nrow(x), around=sum(x$action == 'around')))
e.totals$prop <- e.totals$around / e.totals$total

e.confints <- with(e.totals, binom.confint(around, total, .95, method='exact'))
e.totals$ci.low <- e.confints$lower
e.totals$ci.high <- e.confints$upper


widths <- unique(e.totals$width)
rel_heights <- unique(e.totals$rel_height)

plot_theme <- function(g) {g +
  theme_pander() +
  theme(text=element_text(size=13, family=font.family))
}
plot_theme_bw <- function(g) {g +
  theme_bw() +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(size = 0.7, color="black"),
    text=element_text(size=20, family=font.family),
    legend.position='none'
  )
}
width_color_scale <- function(g) {g +
  scale_color_gradient(low='light green', high='#242424', name='obstacle\nwidth (ft.)', breaks=widths, guide='legend')
}
width_color_scale_bw <- function(g) {g +
  scale_color_gradient(low='gray', high='black', name='obstacle width (ft.)', breaks=widths)
}
p_over_color <- function(g) {g + 
  scale_fill_gradient2(low='#B21826', high='#2166AC', mid='black', midpoint=.5, name='P(over)')
}
rel_height_color <- function(g) {g +
  scale_color_gradient(high='#242424', low='#2497b7', name='rel. height (in.)', breaks=rel_heights) +
  scale_fill_gradient(high='#242424', low='#2497b7', name='rel. height (in.)', breaks=rel_heights)
}
rel_height_color_bw <- function(g) {g +
  scale_color_gradient(high='dark gray', low='black', name='rel. height (in.)', breaks=rel_heights)
}
rel_height_x <- function(g) {g +
  scale_x_continuous(breaks=rel_heights, name='obstacle height (in. relative to max)')
}
height_x <- function(g) {g +
  scale_x_continuous(name='obstacle height (in.)')
}
scaled_height_x <- function(g) {g +
  xlab('obstacle height (% of max)')
}
mutual_scale_x <- function(g) {g +
   scale_x_continuous(name=expression(k[h] * frac(h, h[max]) - k[theta] * tan(theta)))
}
width_x <- function(g) {g +
  scale_x_continuous(breaks=widths, name='obstacle width (ft.)')
}
p_y <- function(g) {g + 
  scale_y_continuous(name='P(around)', lim=c(0, 1), breaks=c(0, .5, 1))
}
p_over_y <- function(g) {g +
  scale_y_continuous(name='P(over)', lim=c(0, 1), breaks=c(0, .5, 1))
}
binomial_y <- function(g) {g +
  scale_y_continuous(breaks=c(0, 1), labels=c('over', 'around'), name='action')
}
rel_height_y <- function(g) {g +
  scale_y_continuous(breaks=rel_heights, name='obstacle height (in. relative to max)')
}

add_labels <- function(g, xs, ys, labels, size=6) {g +
  annotate('text', x=xs, y=ys, label=labels, family=font.family, size=size)
}

g <- ggplot(e, aes(x=rel_height, y=as.numeric(action) - 1, group=interaction(participant, width), color=width)) +
  stat_summary(fun.y=mean, geom='line', position=position_jitter())
g %>% rel_height_x %>% binomial_y %>% width_color_scale %>%
  plot_theme

g <- ggplot(e, aes(x=mutual_scale, y=as.numeric(action) - 1, group=width, color=width)) +
  stat_summary(fun.y=mean, geom='line')
g %>% mutual_scale_x %>% binomial_y %>% width_color_scale %>%
  plot_theme


g <- ggplot(e.totals, aes(x=rel_height, y=prop, ymin=ci.low, ymax=ci.high, color=width, group=width)) +
  geom_line(size=2) + geom_point(size=3) + geom_errorbar(width=2, size=1, position=position_dodge(width=1.75))
g %>%
  rel_height_x %>% p_y %>% width_color_scale %>%
  plot_theme
ggsave('proportions.pdf')

g <- ggplot(e.totals, aes(x=rel_height, y=prop, ymin=ci.low, ymax=ci.high, color=width, group=width)) +
  geom_line(size=2) + geom_point(size=3.5)
g <- g %>%
  rel_height_x %>% p_y %>% width_color_scale_bw %>%
  direct.label(., method=list(gapply.fun(d[-(1:3),]), first.qp, calc.boxes, hjust=0.5, function(d, ...) {
    d$w <- d$w + 0.25
    d$h <- d$h + 0.25
    d[4, 'y'] <- d[4, 'y'] + 0.22
    d$x <- d$x - 0.675
    d$y <- c(2.8, 3.85, 4.63, 5.38, 6.2, 7.15) + 0.15
    return(d)
  }, draw.rects,
  fontfamily=font.family, cex=1.25, colour='black')) %>%
  plot_theme_bw
g
ggsave('proportions_bw.png')

g + geom_errorbar(position=position_dodge(width=0.75))
ggsave('proportions_bw_error.png')

g <- ggplot(e, aes(y=as.numeric(action)-1, x=rel_height, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=0.8)) +
  stat_smooth(method='glm', family=binomial(link='logit'), se=FALSE, size=2, fullrange=TRUE)
g %>%
  rel_height_x %>% binomial_y %>% width_color_scale %>%
  plot_theme
ggsave('every_trial.pdf')


 g <- ggplot(e, aes(y=as.numeric(action)-1, x=scaled_height, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=0.01)) +
  stat_smooth(method='glm', family=binomial(link='logit'), se=FALSE, size=2, fullrange=TRUE)
g %>%
  scaled_height_x %>% binomial_y %>% width_color_scale %>%
  plot_theme
ggsave('every_trial_scaled.pdf')

g <- ggplot(e, aes(y=as.numeric(action) - 1, x=mutual_scale, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=0.01)) +
  stat_smooth(method='glm', family=binomial(link='logit'), se=FALSE, size=2, fullrange=TRUE)
g %>%
  binomial_y %>% width_color_scale %>% mutual_scale_x %>%
  plot_theme

g <- ggplot(e.totals, aes(x=width, y=1-prop, ymax=1-ci.low, ymin=1-ci.high, color=rel_height, group=rel_height, fill=rel_height)) +
  geom_line(size=2) + geom_point(size=3) + geom_errorbar(width=0) + geom_ribbon(alpha=0.3)
g %>%
  width_x %>% p_over_y %>% rel_height_color %>%
  plot_theme
ggsave('by-width.pdf')

g <- ggplot(e.totals, aes(x=width, y=1-prop, ymax=1-ci.low, ymin=1-ci.high, group=rel_height, color=rel_height, label=rel_height)) +
  geom_line(size=2) + geom_point(size=4) #+ geom_errorbar(position=position_dodge(width=0.5))
g <- g %>%
  width_x %>% p_over_y %>% rel_height_color_bw %>%
  #direct.label(., list(gapply.fun(d[-(1:3),]), first.bumpup, calc.boxes, enlarge.box, draw.rects, family=font.family, cex=0.75)) %>%
  add_labels(.,
    xs=rep(12.6, each=7),
    ys=1 - e.totals[e.totals$width == widths[length(widths)],'prop'],
    labels=rel_heights
  ) %>%
  plot_theme_bw
g
ggsave('by-width_bw.png', width=5, height=5)

g + geom_errorbar(position=position_dodge(width=0.3))
ggsave('by-width_bw_error.png', width=5, height=5)

g <- ggplot(e.totals, aes(x=width, y=rel_height, fill=1-prop)) + geom_tile()
g %>% 
  width_x %>% rel_height_y %>% p_over_color %>%
  plot_theme
ggsave('tile.pdf')


raw_fits <- by(p, list(width=p$width), function(df) {df %$% approx(p_around, scaled_height, xout=.5)})
p50 <- data.frame(
  rel_height=as.numeric(lapply(raw_fits, function(x) x$y)),
  width=widths
)
g <- ggplot(p50, aes(x=width, y=rel_height)) + geom_line(size=2) + geom_point(size=4) + scale_y_continuous(breaks=rel_heights, name='{rel. height (in.) | P(around) = 0.5}') + stat_smooth(method='lm')
g %>% width_x %>% plot_theme
ggsave('50-perception-heights.pdf')
