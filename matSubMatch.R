matSubMatch = function(subMat, fullMat){
  
  rowMatch = match(rownames(subMat), rownames(fullMat))
  colMatch = match(colnames(subMat), colnames(fullMat))
  
  if( (any(is.na(rowMatch))) || (any(is.na(colMatch))) )
  {
    print("invalid, sub matrix is larger than full matrix")
    return(NULL)
  }
  
  fullMat[rowMatch, colMatch] = subMat
  return(fullMat)
}