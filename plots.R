library(ggplot2)
library(ggthemes)

p <- read.table('props.csv', header=TRUE, sep=',')
names(p) <- c('rel_height', 'width', 'p_around')

ggplot(p,aes(x=rel_height, y=p_around, color=width, group=width)) +
  geom_line(size=1) +
  geom_point(size=3) +
  theme_pander() +
  scale_color_gradient(low='light green', high='black', name='width (ft.)') +
  xlab('relative hieght (in. below max)') +
  ylab('P(around)')
ggsave('propotions.pdf')

e <- na.omit(read.table('e1.csv', header=TRUE, sep=','))
e <- reshape(e, direction='long',
             varying=c('outbound', 'return'),
             v.names='action',
             timevar='phase',
             times=c('outbound', 'return')
)

e[e$action == 'unsuccessful', 'action'] <- factor('over')
e$action <- ordered(e$action, c('over', 'around'))
e$rel_height <- e$height - e$lowest_height_not_afforded


ggplot(e, aes(y=as.numeric(action)-1, x=rel_height, color=width, group=width)) +
  geom_point(position=position_jitter(h=0.07, w=1.15)) +
  stat_smooth(method='glm', family='binomial', se=FALSE, size=2) +
  scale_x_continuous(breaks=c(-14, -10, -6, -2, 2)) +
  scale_y_continuous(breaks=c(0, 1), labels=c('over', 'around')) +
  scale_color_gradient(low='light green', high='black', name='width (ft.)') +
  theme_pander() +
  xlab('relative hieght (in. below max)') +
  ylab('action')
ggsave('every_trial.pdf')
