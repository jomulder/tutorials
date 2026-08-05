# Ensure required package is available
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Create a grid of points in (-1,1)^3
n <- 100
vals <- seq(-1, 1, length.out = n)
grid <- expand.grid(committee = vals, infl.rep = vals, pref.sim = vals)

# Classify each point into one of the constrained subspaces
grid$H1 <- with(grid, committee > infl.rep & infl.rep > pref.sim & pref.sim > 0)
tol <- 0.05
grid$H2 <- with(grid, abs(committee - infl.rep) < tol & 
                  abs(infl.rep - pref.sim) < tol & 
                  pref.sim > 0)
tol <- 0.02
grid$H3 <- with(grid, abs(committee) < tol & 
                  abs(infl.rep) < tol & 
                  abs(pref.sim) < tol)

# Extract points for each hypothesis
h1 <- grid[grid$H1, ]
h2 <- grid[grid$H2, ]
h3 <- grid[grid$H3, ]

# Build plotly figure
fig <- plot_ly() %>%
  add_trace(
    data = h1,
    x = ~infl.rep, z = ~committee, y = ~pref.sim,
    type = "scatter3d", mode = "markers",
    marker = list(size = 5, color = "#40E0D0", opacity = 1),
    name = "H1: committee > infl.rep > pref.sim > 0"
  ) %>%
  add_trace(
    data = h2,
    x = ~infl.rep, z = ~committee, y = ~pref.sim,
    type = "scatter3d", mode = "markers",
    marker = list(size = 2, color = "#C8622A", opacity = 0.3),
    name = "H2: committee = infl.rep = pref.sim > 0"
  ) %>%
  add_trace(
    data = h3,
    x = ~infl.rep, z = ~committee, y = ~pref.sim,
    type = "scatter3d", mode = "markers",
    marker = list(size = 4, color = "black", opacity = 1),
    name = "H3: committee = infl.rep = pref.sim = 0"
  ) %>%
  
  layout(
    title = "Constrained parameter subspaces",
    scene = list(
      zaxis = list(title = "committee", range = c(-1, 1)),
      xaxis = list(title = "infl.rep", range = c(-1, 1)),
      yaxis = list(title = "pref.sim", range = c(-1, 1)),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.5)
      )
    ),
    legend = list(x = 0, y = 1)
  )

fig
