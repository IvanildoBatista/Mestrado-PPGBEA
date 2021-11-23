install.packages('ggplot2')
install.packages('ggpubr')
library(ggplot2)
library(ggpubr)


x = c(77, 112, 137, 113, 117, 110, 94, 125, 116, 133, 102, 115, 111, 105, 93, 87, 88, 91, 102,
      100, 91, 76, 104, 66, 107)
y = c(84, 96, 116, 144, 123, 139, 128, 113, 155, 146, 101, 128, 118, 115, 113, 79, 104, 85, 88,
      120, 104, 60, 129, 51, 86)
df <- data.frame(x,y)

plot(df$x,df$y)


graph1 <- ggplot(data=df, aes(x=x, y=y)) +geom_smooth(method="lm") +
  ggtitle('Gráfico de regressão') + 
  geom_point() + stat_regline_equation(output.type = 'latex') + xlab('Número de fumantes') + 
  ylab('Mortalidade') + theme_gray() + theme(plot.title = element_text(hjust=0.5))

ggsave(graph1, height = 5, width = 15, device = 'Gráfico de regressão')
                                             