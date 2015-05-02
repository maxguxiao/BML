plotGrid = 
function(x)
{
  image(x, col = c('white', 'red', 'blue'), axe = FALSE)
}


createGrid = 
function(rowNum = 10, colNum = 6, rho = 0.3)
{
  grid = integer(rowNum * colNum)
  blueNum = redNum = rowNum * colNum * rho / 2
  set.seed(1000)
  pos = sample(x = rowNum * colNum, size = blueNum + redNum, replace = FALSE)
  blueCars = pos[1:blueNum]
  redCars = pos[-(1:blueNum)]
  grid[redCars] = 1
  grid[blueCars] = 2
  myGrid = matrix(grid, nrow = rowNum, ncol = colNum, byrow = TRUE)
  plotGrid(myGrid)
  box()
  return(myGrid)
}



