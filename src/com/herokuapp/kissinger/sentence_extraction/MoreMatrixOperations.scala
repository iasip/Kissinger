package com.herokuapp.kissinger.sentence_extraction

import Jama.Matrix

/** Additional Matrix Operations **/
object MoreMatrixOperations {
  
  def rowNormalize(M: Matrix): Matrix = {
    val rowDimension = M.getRowDimension
    val columnDimension = M.getColumnDimension
    var rowNormalizedMatrix = new Matrix(rowDimension, columnDimension)
    
    var i = 0
    var j = 0
    while (i < rowDimension) {
      var sum = 0.0
      while (j < columnDimension) {
        sum = sum + M.get(i, j)
        j = j + 1
      }
      if (sum == 0.0) sum = 1
      
      j = 0
      while (j < columnDimension) {
        rowNormalizedMatrix.set(i, j, M.get(i, j) / sum)
        j = j + 1
      }
      i = i + 1
    }
    
    return rowNormalizedMatrix
  }
  
  def columnNormalize(M: Matrix): Matrix = {
    val rowDimension = M.getRowDimension
    val columnDimension = M.getColumnDimension
    var columnNormalizedMatrix = new Matrix(rowDimension, columnDimension)
    
    var i = 0
    var j = 0
    while (j < columnDimension) {
      var sum = 0.0
      while (i < rowDimension) {
        sum = sum + M.get(i, j)
        i = i + 1
      }
      if (sum == 0.0) sum = 1
      
      i = 0
      while (i < rowDimension) {
        columnNormalizedMatrix.set(i, j, M.get(i, j) / sum)
        i = i + 1
      }
      j = j + 1
    }
    
    return columnNormalizedMatrix
  }
}