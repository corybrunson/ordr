# This function is used in conjunction with a position scale to create a
# secondary axis, positioned opposite of the primary axis. All secondary axes
# must be based on a one-to-one transformation of the primary axes.

p <- ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()
p
p + scale_y_continuous(sec.axis = sec_axis(~.+10))

# *ordr* procedure:
# 1. detect conference of inertia `p = c(p[i], p[j])` (plotting dimensions i, j)
# 2. obtain new scales for dimension `j` as `~ . * p[j]`
# 3. set new scale names to old scale name
# 4. create secondary axes using `ggplot2::scale_*_continuous()`
