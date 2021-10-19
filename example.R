extent <- c(0, 1, 0, 1)
dimension <- c(2, 3)
projection <- ""
opt <- options(grid.spec = list(extent = extent, dimension = dimension, projection = projection))

grid_spec(dimension = finer(10))
grid_spec(dimension = coarser(2))
grid_spec(dimension = c(20, 20))
grid_spec(extent = bigger())
grid_spec(extent = bigger(4))

grid_spec(extent = wider(4))
grid_spec(extent = taller(4))
