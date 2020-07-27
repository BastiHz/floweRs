# testing some stuff

library(manipulate)

x <- seq(0, 1, length.out = 1000)

manipulate(
    {
        y <- dbeta(x, alpha, beta)
        y <- c(y[-1], -rev(y)[-1])
        xx <- c(x[-1], rev(x)[-1])
        plot(NA, xlim = range(x), ylim = range(y))
        polygon(xx, y, col = "red", lwd = 2)
    },
    alpha = slider(1.1, 5, initial = 3, step = 0.1),
    beta = slider(1.1, 5, initial = 3, step = 0.1)
)


# -------------------------------------------------------------------------

# vom BastiHzR Paket
linear_map <- function(x,
                       in_start = min(x, na.rm = TRUE),
                       in_end = max(x, na.rm = TRUE),
                       out_start = 0,
                       out_end = 1) {
    stopifnot(
        in_start != in_end,
        out_start != out_end
    )
    (x - in_start) / (in_end - in_start) * (out_end - out_start) + out_start
}

rotate_xy <- function(m, angle) {
    if (angle == 0) return(m)
    rotation_matrix <- matrix(
        c(cos(angle), sin(angle), -sin(angle), cos(angle)),
        nrow = 2
    )
    m %*% rotation_matrix
}



# Test Rotation:
foo <- cbind(x = c(0, 1, 3), y = c(0, 1, 1))
plot(foo, type = "l", asp = 1, ylim = c(0, 3))
angle = -pi / 4  # 45° counterclockwise
lines(rotate_xy(foo, angle))


# -------------------------------------------------------------------------


x <- seq(0, 1, length.out = 1000)
alpha <- 1.7
beta <- 3.5
y <- dbeta(x, alpha, beta)
y <- c(y[-1], -rev(y)[-1])
x <- c(x[-1], rev(x)[-1])
max_width <- 0.25
y <- linear_map(y, out_start = -max_width, out_end = max_width)
petal <- cbind(x, y)
head(petal)

par(pty = "s", mar = rep(0, 4))
plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, ann = FALSE)
polygon(petal, col = "#ff9900a0", lwd = 1.5)


# -------------------------------------------------------------------------


x <- seq(0, 1, length.out = 1000)
ab <- runif(2, 1.1, 4)
y <- dbeta(x, ab[1], ab[2])
y <- c(y[-1], -rev(y)[-1])
x <- c(x[-1], rev(x)[-1])
max_width <- runif(1, 0.1, 0.3)
y <- linear_map(y, out_start = -max_width, out_end = max_width)
petal <- cbind(x, y)
par(pty = "s", mar = rep(0, 4))
plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, ann = FALSE)
n_petals <- sample(3:9, 1)
offset_angle <- runif(1, 0, 2 * pi)
petal <- rotate_xy(petal, offset_angle)
angles <- seq(0, 2 * pi, length.out = n_petals + 1)[-(n_petals + 1)]  # remove the last angle because it is == 0
color <- runif(3)
color <- rgb(color[1], color[2], color[3])
# Shapes:
for (angle in angles) {
    polygon(rotate_xy(petal, angle), col = color, border = NA)
}
# Outlines:
for (angle in angles) {
    polygon(rotate_xy(petal, angle), lwd = 2)
}
print(ab)
print(max_width)
print(color)

# TODO: Andere Wahrscheinlichkeitsverteilungen für die Auswahl der Parameter nutzen.
# TODO: Probiere es mit anderen Arten von Kurven. Vielleicht Bezier?


# Blütenboden
# Kelchblätter
# Kronblätter
# Staubblätter
# Stempel
