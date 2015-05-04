moveCars =
function(rowNum = 6, colNum = 4, rho = 0.3, steps = 3)
{
  n = rowNum * colNum
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
  movetry[movetry > n] = movetry[movetry > n] - n
  whereToMove = movetry %in% currentBluePos
  names(whereToMove) = currentRedPos
  checkPoints = as.numeric(names(whereToMove[whereToMove == TRUE]))
  if(length(checkPoints))
  {
      whereToMove <- rowSums(sapply(checkPoints, checkRed, whereToMove, colNum, n)) != 0
  }
  move = (currentRedPos[!whereToMove] + colNum)
  move[move > n] = move[move > n] - n  
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
  #bmovetry[bmovetry > n] = bmovetry[bmovetry > n] - n
  
  bwhereToMove = bmovetry %in% currentRedPos
  names(bwhereToMove) = currentBluePos
  checkPoints = as.numeric(names(bwhereToMove[bwhereToMove == TRUE]))
  if(length(checkPoints))
  {
    bwhereToMove <- rowSums(sapply(checkPoints, checkBlue, bwhereToMove,colNum)) != 0
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



checkRed <- function(checkPos, whereToMove, colNum, n)
{
    checkNum = checkPos - colNum
    checkNum[checkNum < 0] = checkNum[checkNum < 0] + n
    if(checkNum %in% as.numeric(names(whereToMove)))
    {
        
        whereToMove[as.character(checkNum)] =TRUE
        checkRed(checkNum, whereToMove, colNum, n)
    }
    else return(whereToMove)
}

checkBlue <- function(checkPos, whereToMove, colNum)
{
  checkNum = checkPos - 1
  if(ceiling(checkNum/colNum) < ceiling(checkPos/colNum))
  {
    checkNum = checkNum + colNum
  }
  if( (checkNum) %in% as.numeric(names(whereToMove)))
  {
    
    whereToMove[as.character(checkNum)] = TRUE
    checkBlue(checkNum, whereToMove,colNum)
  }
  else return(whereToMove)
}



