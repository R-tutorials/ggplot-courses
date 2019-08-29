#####################################################
##    Code for "An Introduction to `{ggplot2}`"    ##
##         by Cédric Scherer, IZW Berlin           ##
##      Stats Group  |  28th of August 2019        ##
#####################################################

## packages needed:
## install.packages("tidyverse")
## install.packages("grid")
## install.packages("viridis")
## install.packages("rcartocolor")
## install.packages("ggthemes")
## install.packages("hrbrthemes", repos = "https://cinc.rud.is")
## devtools::install_github("Ryo-N7/tvthemes")

## Setup
library(tidyverse)
## library(ggplot2)
theme_set(theme_bw(base_size = 18))

chic <- readr::read_csv("https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
chic$season <- factor(chic$season, levels = c("Spring", "Summer", "Autumn", "Winter"))
chic$year <- factor(chic$year, levels = as.character(1997:2000))
tibble::glimpse(chic)

ggplot(data = chic,
       mapping = 
         aes(
           x = date,
           y = temp
         )
       )

ggplot(chic, aes(date, temp))

ggplot(chic) +
  aes(date, temp)

ggplot(chic, aes(date, temp)) +
  geom_point()

ggplot(chic, aes(date, temp)) +
  geom_line()

ggplot(chic, aes(date, temp)) +
  geom_area()

ggplot(chic, aes(date, temp)) +
  geom_boxplot()

ggplot(chic, aes(year, temp)) +
  geom_boxplot()

ggplot(chic, aes(date, temp)) +
  geom_point() +
  geom_line() +
  geom_rug(sides = "r")

ggplot(chic, aes(year, temp)) +
  geom_boxplot() +
  stat_summary(
    fun.y = mean,
    geom = "point"
  )

ggplot(tibble(x = c(-8, 8)), aes(x)) +
  stat_function(fun = dnorm) +
  stat_function(
    fun = dcauchy,
    geom = "point",
    n = 75
  )

ggplot(chic, aes(temp)) +
  stat_ecdf(geom = "step")

ggplot(chic, aes(date, temp)) +
  geom_point() +
  stat_smooth()

ggplot(chic, aes(date, temp)) +
  geom_point() +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x, k = 100),
    se = F
  )

ggplot(chic, aes(date, temp)) +
  geom_point(
    size = 4,
    alpha = 0.2
  )

ggplot(chic, aes(date, temp)) +
  geom_point(
    aes(
      color = season,
      shape = year
    ),
    size = 4,
    alpha = 0.2
  )

ggplot(
  chic,
  aes(date,
    temp,
    color = season,
    shape = year)
  ) +
  geom_point(
    size = 4,
    alpha = 0.2
  )

ggplot(chic, aes(temp)) +
  stat_ecdf(
    aes(
      group = year),
      geom = "step"
  )

ggplot(chic, aes(temp)) +
  stat_ecdf(
    aes(color = year),
    geom = "step"
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_x_date(
    name = NULL
  ) +
  scale_y_continuous(
    name = "Temperature (°F)"
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_x_date(
    name = NULL,
    limits = c(#<<
      as.Date("1997-01-01"),
      as.Date("1999-12-31")
    )
  ) +
  scale_y_log10(
    name = "Temperature (°F)",
    breaks = c(1, 10, 100),
    labels = scales::scientific
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_y_continuous(
    name = "How I feel...",
    breaks = c(0, 30, 60, 90),
    labels = c(
      '"Brrr, too cold!"',
      '"Still freezing."',
      '"Okay."',
      '"Uff, too hot!"'
    )
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_x_date(
    name = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Temperature (°F)",
    expand = c(0, 0)
  )

ggplot(chic, aes(date, temp)) +
geom_point(aes(color = season)) +
  scale_color_manual(
    values = c(
      "firebrick",
      "dodgerblue",
      "darkorchid",
      "goldenrod"
    )
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_color_manual(
    values = c(
      "firebrick",
      "dodgerblue",
      "darkorchid",
      "goldenrod"
    ),
    name = "Pretty\ncolored\nsesons:"
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_color_manual(
    values = c(
      "firebrick",
      "dodgerblue",
      "darkorchid",
      "goldenrod"
    ),
    guide = FALSE
  )


ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_color_manual(
    values = c(
      "firebrick",
      "dodgerblue",
      "darkorchid",
      "goldenrod"
    ),
    guide = FALSE
  ) +
  scale_shape_discrete(
    solid = FALSE,
    name = "Year"
  )

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_color_brewer(
    palette = "Set2",
    guide = FALSE
  )

## Figure code ######
g <- ggplot(chic, aes(x = date, y = temp, color = temp)) + 
       geom_point() + 
       labs(x = "Year", y = "\nTemperature (°F)") +
       guides(color = guide_colorbar(direction = "horizontal",
                                     barheight = unit(6, units = "mm"), 
                                     barwidth = unit(100, units = "mm"),
                                     draw.ulim = FALSE, title.position = 'bottom',
                                     title.hjust = 0.5, label.hjust = 0.5))

p1 <- g + scale_color_continuous(NULL) + ggtitle("scale_color_continuous()")
p2 <- g + viridis::scale_color_viridis(NULL, option = "inferno") + ggtitle("scale_color_viridis()")
p3 <- g + rcartocolor::scale_color_carto_c(NULL, palette = "Temps") + ggtitle("scale_color_carto_c()")

library(patchwork)
(p1 + p2 + p3) * 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, family = "Roboto Mono", size = 16))
#####################

ggplot(chic, aes(date, temp)) +
  geom_line() +
  coord_cartesian(
    ylim = c(50, 70)
  )

ggplot(chic, aes(year, temp)) +
  geom_boxplot()  +
  coord_cartesian(
    ylim = c(50, 70)
  )

ggplot(chic, aes(year, temp)) +
  geom_boxplot()  +
  scale_y_continuous(
    limits = c(50, 70)
  )

ggplot(chic, aes(date, temp)) +
  geom_point() +
  scale_x_date(
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 80))
  
ggplot(chic, aes(date, temp)) +
  geom_point() +
  scale_x_date(
    expand = c(0, 0)
  ) +
  coord_cartesian(
    ylim = c(0, 80),
    clip = "off"
  )

ggplot(chic, aes(season, temp)) +
  geom_col() +
  coord_flip()

ggplot(chic, aes(season, temp)) +
  stat_summary(fun.y = mean,
              geom = "bar")

ggplot(chic, aes(season, temp)) +
  stat_summary(fun.y = mean,
              geom = "bar") +
  coord_polar()

ggplot(chic, aes(o3, temp)) +
  geom_point() +
  coord_trans(x = "log2")

(g_world <-
  ggplot(map_data("world"),
    aes(long, lat,
        group = group)) +
  geom_polygon(
    fill = "tan",
    color = "grey20"
  )
)

g_world +
  coord_map(xlim = c(-180,180))

g_world +
  coord_map("ortho")

ggplot(chic, aes(temp, o3)) +
  geom_point(aes(color = year)) +
  facet_wrap(~ season)

ggplot(chic, aes(temp, o3)) +
  geom_point(aes(color = year)) +
  facet_wrap(~ season, scales = "free")

ggplot(chic, aes(temp, o3)) +
  geom_point(aes(color = year)) +
  facet_grid(season ~ year)

ggplot(chic, aes(date, temp)) +
  geom_point() +
  theme(
    axis.text = element_text(
      size = 15,
      face = "bold",
      color = "red"
    ),
    panel.grid.major.x = element_line(
      linetype = "dotted",
      color = "black"
    ),
    plot.background = element_rect(
      fill = "dodgerblue",
      color = "goldenrod"
    )
  )

ggplot(chic, aes(date, temp)) +
  geom_point() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggplot(chic, aes(date, temp)) +
  geom_point() +
  theme_gray()

ggplot(chic, aes(date, temp)) +
  geom_point() +
  theme_minimal()

ggplot(chic, aes(date, temp)) +
  geom_point() +
  theme_bw(base_size = 40)

## Figure code ######
t1 <- ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  ggthemes::theme_gdocs() +
  ggthemes::scale_color_gdocs() +
  ggtitle("{ggthemes} theme_gdocs()") +
  theme(plot.title = element_text(family = "Roboto Mono", size = 30,
                                  face = "bold", hjust = 0.5),
        plot.background = element_rect(color = "white"))
  
ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_colour_grey() +
  ggthemes::theme_tufte() +
  ggtitle("{ggthemes} theme_tufte()") +
  theme(plot.title = element_text(family = "Roboto Mono", size = 30,
                                  face = "bold", hjust = 0.5),
        plot.background = element_rect(color = "white"))

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  hrbrthemes::theme_ft_rc(grid = "Y") +
  hrbrthemes::scale_color_ft() +
  ggtitle("{hrbrthemes} theme_ft_rc()") +
  theme(plot.title = element_text(family = "Roboto Mono", size = 30,
                                  face = "bold", hjust = 0.5),
        plot.background = element_rect(color = "white"))
  
ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  tvthemes::theme_simpsons() +
  tvthemes::scale_color_simpsons() +
  ggtitle("\n{tvthemes} theme_simpsons()") +
  theme(plot.title = element_text(family = "Roboto Mono", size = 30,
                                  face = "bold", hjust = 0.5),
        plot.background = element_rect(color = "white"))

library(gridExtra)
(t <- arrangeGrob(t1, t2, t3, t4, ncol = 2))
#####################

(g <- ggplot(chic, aes(date, temp)) +
  geom_point())

old <- theme_set(theme_grey())
g

theme_update(
  panel.background = element_rect(
    fill = "tan",
    color = "black"
  )
)
g

theme_set(old)
g


## Additional Slides

ggplot(chic, aes(date, temp)) +
  geom_point() +
  ggtitle("Temperatures in Chicago")

(g <- ggplot(chic, aes(date, temp)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Temperature (°F)",
    title = "Temperatures in Chicago",
    subtitle = "Seasonal pattern of daily temperatures\nfrom 1997 to 2001",
    caption = "Data: NMMAPS",
    tag = "(a)"
  )
)

g +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 1,
      margin = margin(15, 0, 15, 0)#<<
    ),
    plot.caption = element_text(
      face = "italic"
    ),
    plot.tag.position = c(0.15, 0.75)
  )

ggplot(chic, aes(date, temp)) +
  geom_point() +
  annotate(
    "text",
    x = as.Date("1998-07-01"),
    y = 10,
    label = "The Summer\nof 1998",
    size = 3,
    fontface = "bold",
    color = "firebrick"
  )

ggplot(chic, aes(date, temp)) +
  geom_point() +
  annotate(
    "text",
    x = as.Date("1998-07-01"),
    y = 10,
    label = "The Summer\nof 1998",
    size = 3,
    fontface = "bold",
    color = "firebrick"
  ) +
  facet_wrap(~year, scales = "free_x")

text <- grid::grobTree(
  grid::textGrob(
    "Summertime!",
    x = 0.5, 
    y = 0.1, 
    hjust = 0.5,
    gp = grid::gpar(
      fontsize = 12,
      fontface = "bold",
      col = "firebrick"
    )
  )
)

ggplot(chic, aes(date, temp)) +
  geom_point() +
  annotation_custom(text) +
  facet_wrap(~ year, scales = "free_x")

library(ggtext)

ggplot(chic, aes(date, temp)) +
  geom_point(aes(color = season)) +
  scale_color_manual(
    name = NULL,
    values = c("#0072B2", "#009E73", "#FF8423", "#B22222"),
    labels = c(
      "<b style='color:#0072B2'>Spring:<br></b><i>Apr to Jun<br></i>",
      "<b style='color:#009E73'>Summer:</b><br><i>Jul to Sep<br></i>",
      "<b style='color:#FF8423'>Autumn:</b><br><i>Oct to Dec<br></i>",
      "<b style='color:#B22222'>Winter:</b><br><i>Jan to Mar<br></i>"
    )
  ) +
  theme(legend.text = element_markdown())

img <- png::readPNG("./img/logo.png")
logo <- grid::rasterGrob(img, interpolate = T)

ggplot(chic, aes(date, temp)) +
  annotation_custom(
    logo,
    xmin = as.Date("1997-06-01"),
    xmax = as.Date("2000-06-01"),
    ymin = 0,
    ymax = 80
  ) +
  geom_point(alpha = 0.2)

ggplot(iris, aes(Species, Sepal.Width)) +
  geom_boxplot() +
  scale_x_discrete(
    name = NULL,
    labels = c(
      setosa = "*I. setosa*<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/Iris_setosa.JPG/180px-Iris_setosa.JPG' width='100' />",
      virginica = "*I. virginica*<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Iris_virginica_-_NRCS.jpg/320px-Iris_virginica_-_NRCS.jpg' width='100' />",
      versicolor = "*I. versicolor*<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/20140427Iris_versicolor1.jpg/320px-20140427Iris_versicolor1.jpg' width='100' />"
    )
  ) +
  theme(axis.text.x = element_markdown())

ggplot(chic, aes(date, temp)) +
  annotate(
    "rect",
    xmin = as.Date("1997-01-01"),
    xmax = as.Date("1997-12-31"),
    ymin = -Inf, ymax = Inf,
    color = NA, alpha = 0.2
  ) +
  annotate(
    "rect",
    xmin = as.Date("1997-01-01"),
    xmax = as.Date("1997-12-31"),
    ymin = -Inf, ymax = Inf,
    color = NA, alpha = 0.2
  ) +
  geom_point()

rect <- tibble(
  xmin = as.Date(c("1997-01-01",
                   "1999-01-01")),
  xmax = as.Date(c("1997-12-31",
                   "1999-12-31")),
  ymin = rep(-Inf, 2),
  ymax = rep(Inf, 2)
)

ggplot(chic) +
  geom_rect(
    data = rect,
    aes(
      xmin = xmin, 
      xmax = xmax,
      ymin = ymin, 
      ymax = ymax
    ),
    color = NA,
    alpha = 0.2
  ) +
  geom_point(aes(date, temp))

lines <- tibble(
  xintercept = seq(
    as.Date("1997-01-01"),
    as.Date("2001-01-01"),
    length.out = 5
  )
)
 
ggplot(chic, aes(date, temp)) +
  geom_vline(
    data = lines,
    aes(xintercept = xintercept),
    color = "grey30"
  ) +
  geom_point()
