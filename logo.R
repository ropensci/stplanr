# Aim: make logo

install.packages("transformr")
library(stplanr)
library(tmap)

plot(flowlines_sf["All"])

f = flowlines_sf[flowlines_sf$Area.of.residence != flowlines_sf$Area.of.workplace,]
tm_shape(f) +
  tm_lines(col = "All", pal = "viridis", lwd = "All", scale = 9) +
  tm_layout(legend.show = FALSE, frame = FALSE)

tmap_save(.Last.value, "logo-test.pdf")


# Animation
circles <- poly_circles()
star_hole <- poly_star_hole()

morph <- tween_polygon(circles, star_hole,
                       ease = 'linear',
                       id = id,
                       nframes = 12,
                       match = FALSE)

ggplot(morph) +
  geom_polygon(aes(x = x, y = y, group = id), fill = NA, colour = 'black') +
  facet_wrap(~.frame, labeller = label_both, ncol = 3) +
  theme_void()

library(gganimate)
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

gganimate::anim_save(animation = animation, "animation.gif")
