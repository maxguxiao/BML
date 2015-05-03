moveCars =
function(rowNum = 10, colNum = 6, rho = 0.3, steps = 1000)
{
  n = rowNum * colNum
  set.seed(100)
  test = createGrid(rowNum = rowNum,colNum = colNum, rho = rho )
  currentRedPos = test$rc
  currentBluePos = test$bc
  
  for(i in 1: steps)
  {
    #move red cars
    redRun = redCarMove(currentRedPos, currentBluePos, colNum, rowNum, n)
    currentRedPos = redRun$currentRedPos
    currentBluePos = redRun$currentBluePos
    #move blue cars
    blueRun = blueCarMove(currentBluePos, currentRedPos, colNum, rowNum, n)
    currentRedPos = blueRun$currentRedPos
    currentBluePos = blueRun$currentBluePos
  }  
}


redCarMove = function(currentRedPos, currentBluePos, colNum, rowNum, n)
{
  movetry = (currentRedPos + colNum) 
  movetry[movetry > 60] = movetry[movetry > 60] - 60
  whereToMove = movetry %in% currentBluePos
  names(whereToMove) = currentRedPos
  checkPoints = as.numeric(names(whereToMove[whereToMove == TRUE]))
  if(length(checkPoints))
  {
      whereToMove <- rowSums(sapply(checkPoints, checkRed, whereToMove, colNum)) != 0
  }
  move = (currentRedPos[!whereToMove] + colNum)
  move[move > 60] = move[move > 60] - 60  
  currentRedPos[!whereToMove] = move
  grid = integer(n)
  grid[currentRedPos] = 1
  grid[currentBluePos] = 2
  myGrid = matrix(grid, nrow = rowNum, ncol = colNum, byrow = TRUE)
  plotGrid(myGrid)
  return(list(currentRedPos = currentRedPos, currentBluePos = currentBluePos))
}


blueCarMove = function(currentBluePos, currentRedPos, colNum, rowNum, n)
{
  bmovetry = (currentBluePos + 1 - 
                (ceiling((currentBluePos + 1) / colNum) - 
                   ceiling(currentBluePos / colNum)) * colNum) 
  
  
  bwhereToMove = bmovetry %in% currentRedPos
  names(bwhereToMove) = currentBluePos
  checkPoints = as.numeric(names(bwhereToMove[bwhereToMove == TRUE]))
  if(length(checkPoints))
  {
    whereToMove <- rowSums(sapply(checkPoints, checkBlue, bwhereToMove,colNum)) != 0
  }
  
  move = (currentBluePos[!bwhereToMove] + 1 -
            (ceiling((currentBluePos[!bwhereToMove] + 1) / colNum) - 
               ceiling(currentBluePos[!bwhereToMove] / colNum)) * colNum)
  
  
  currentBluePos[!bwhereToMove] = move
  grid = integer(rowNum * colNum)
  grid[currentRedPos] = 1
  grid[currentBluePos] = 2
  bmyGrid = matrix(grid, nrow = rowNum, ncol = colNum, byrow = TRUE)
  plotGrid(bmyGrid)
  return(list(currentRedPos = currentRedPos, currentBluePos = currentBluePos))
}



checkRed <- function(checkPos, whereToMove, colNum)
{
    if( (checkPos - 6 ) %in% as.numeric(names(whereToMove)))
    {
        
        whereToMove[as.character(checkPos-6)] =TRUE
        check(checkPos - 6, whereToMove,colNum)
    }
    else return(whereToMove)
}

checkBlue <- function(checkPos, whereToMove, colNum)
{
  if( (checkPos - 1 ) %in% as.numeric(names(whereToMove)) & 
        (floor(checkPos - 1) == floor(checkPos - 1)))
  {
    
    whereToMove[as.character(checkPos-1)] = TRUE
    check(checkPos - 1, whereToMove,colNum)
  }
  else return(whereToMove)
}



