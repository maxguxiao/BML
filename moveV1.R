moveCars =
function()
{
  
}

rowNum = 10; colNum = 6; rho = 0.3;
n = rowNum * colNum
test = createGrid()
currentRedPos = test$rc
currentBluePos = test$bc

dev.new()
for(i in 1: 100)
{
movetry = (currentRedPos + colNum) %% n
whereToMove = movetry %in% currentBluePos
move = (currentRedPos[!whereToMove] + colNum) %% n 
currentRedPos[!whereToMove] = move

grid = integer(rowNum * colNum)
grid[currentRedPos] = 1
grid[currentBluePos] = 2
myGrid = matrix(grid, nrow = rowNum, ncol = colNum, byrow = TRUE)
plotGrid(myGrid)
box()

bmovetry = (currentBluePos + 1 - 
              (ceiling((currentBluePos + 1) / colNum) - 
                 ceiling(currentBluePos / colNum)) * colNum) %% n


bwhereToMove = bmovetry %in% currentRedPos
move = (currentBluePos[!bwhereToMove] + 1 -
          (ceiling((currentBluePos[!bwhereToMove] + 1) / colNum) - 
             ceiling(currentBluePos[!bwhereToMove] / colNum)) * colNum) %% n


currentBluePos[!bwhereToMove] = move
grid = integer(rowNum * colNum)
grid[currentRedPos] = 1
grid[currentBluePos] = 2
bmyGrid = matrix(grid, nrow = rowNum, ncol = colNum, byrow = TRUE)
plotGrid(bmyGrid)
box()
}

dev.off()


