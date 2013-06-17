package com.herokuapp.kissinger.sentence_extraction

import Jama.Matrix

/** Chinese sentences and English sentences are simultaneously ranked
  * in a unified graph-based algorithm (Wan, 2011).
  * 
  */
class ConstrainedCoRank() {
  val m = Processor.enSents.length
  val n = Processor.cnSents.length
  var en_en = new Matrix(m, m)
  var cn_cn = new Matrix(n, n)
  var distance_en_cn = new Matrix(m, n)
  var similarity_en_cn = new Matrix(m, n)
  populateAllMatrices()
  var distance_cn_en = distance_en_cn.transpose()
  var similarity_cn_en = similarity_en_cn.transpose()
  rowNormalizeAllMatrices()
  
/** Fill matrix entries with the appropriate similarity measures. 
  *  
  */
  def populateAllMatrices() = {
    var i = 0
    var j = 0
    while (i < m) {
      while (j < m) {
        if (!(i==j)) {
          val v1 = Processor.getTermVector(Processor.enSents(i), "en")
          val v2 = Processor.getTermVector(Processor.enSents(j), "en")
          en_en.set(i, j, VectorSpaceModel.cosineSimilarity(v1, v2))
        }
        j = j + 1
      }
      i = i + 1
    }
    
    i = 0
    j = 0
    while (i < n) {
      while (j < n) {
        if (!(i==j)) {
          val v1 = Processor.getTermVector(Processor.cnSents(i), "cn")
          val v2 = Processor.getTermVector(Processor.cnSents(j), "cn")
          cn_cn.set(i, j, VectorSpaceModel.cosineSimilarity(v1, v2))
        }
        j = j + 1
      }
      i = i + 1
    }
    
    i = 0
    j = 0
    while (i < m) {
      while (j < n) {
        if (!(i==j)) {
          val v1 = Processor.getTermVector(Processor.enSents(i), "en")
          val v2 = Processor.getTermVector(Processor.cnEnSents(j), "cnen")
          val d1 = VectorSpaceModel.normalizedEuclideanDistance(v1, v2)
          val v3 = Processor.getTermVector(Processor.enCnSents(i), "encn")
          val v4 = Processor.getTermVector(Processor.cnSents(j), "cn")
          val d2 = VectorSpaceModel.normalizedEuclideanDistance(v3, v4)
          val geometricMean = math.sqrt(d1*d2)
          distance_en_cn.set(i, j, geometricMean)
        }
        j = j + 1
      }
      i = i + 1
    }
    
    i = 0
    j = 0
    while (i < m) {
      while (j < n) {
        if (!(i==j)) {
          val v1 = Processor.getTermVector(Processor.enSents(i), "en")
          val v2 = Processor.getTermVector(Processor.cnEnSents(j), "cnen")
          val d1 = VectorSpaceModel.cosineSimilarity(v1, v2)
          val v3 = Processor.getTermVector(Processor.enCnSents(i), "encn")
          val v4 = Processor.getTermVector(Processor.cnSents(j), "cn")
          val d2 = VectorSpaceModel.cosineSimilarity(v3, v4)
          val geometricMean = math.sqrt(d1*d2)
          similarity_en_cn.set(i, j, geometricMean)
        }
        j = j + 1
      }
      i = i + 1
    }
  }
  
  /**
    * 
    */
  def rowNormalizeAllMatrices() = {
    en_en = MoreMatrixOperations.rowNormalize(en_en)
    cn_cn = MoreMatrixOperations.rowNormalize(cn_cn)
    distance_en_cn = MoreMatrixOperations.rowNormalize(distance_en_cn)
    distance_cn_en = MoreMatrixOperations.rowNormalize(distance_cn_en)
    similarity_en_cn = MoreMatrixOperations.rowNormalize(similarity_en_cn)
    similarity_cn_en = MoreMatrixOperations.rowNormalize(similarity_cn_en)
  }
  
  /** The difference score indicates how much a sentence contains differential information.
    * The difference score of a sentence in one language relies not only on the other sentences
    * in the same language, but also on the sentences in the other language (Wan, 2011).
    * 
    * @param iterations specifies the number of iterations the DiffScore equations are run.
    * @param alpha specifies the relative contribution of sentences in the same language to the final difference score.
    * @param beta specifies the relative contribution of sentences in different languages to the final difference score.
    */
               
  def differenceScore(iterations: Integer, alpha: Double, beta: Double): (Matrix, Matrix) = {
    var enDiffScores = new Matrix(m, 1)
    var cnDiffScores = new Matrix(n, 1)
    var i = 0
    while (i < m) {
      enDiffScores.set(i, 0, 1.0)
      i = i + 1
    }
    i = 0
    while (i < n) {
      cnDiffScores.set(i, 0, 1.0)
      i = i + 1
    }
    
    for (_ <- 0 until iterations) {
      enDiffScores = MoreMatrixOperations.columnNormalize(en_en.transpose().times(alpha).times(enDiffScores)
        .plus(distance_cn_en.transpose().times(beta).times(cnDiffScores)).copy())
      cnDiffScores = MoreMatrixOperations.columnNormalize(cn_cn.transpose().times(alpha).times(cnDiffScores)
        .plus(distance_en_cn.transpose().times(beta).times(enDiffScores)).copy())
    }
    return (enDiffScores, cnDiffScores) 
  }
  
  /** The common score indicates how much a sentence contains important and common information
    * in the two document sets (Wan, 2011).
    * 
    * @param iterations specifies the number of iterations the CommScore equations are run.
    * @param alpha specifies the relative contribution of sentences in the same language to the final common score.
    * @param beta specifies the relative contribution of sentences in different languages to the final common score.
    */
  def commonScore(iterations: Integer, alpha: Double, beta: Double): (Matrix, Matrix) = {
    var enCommScores = new Matrix(m, 1)
    var cnCommScores = new Matrix(n, 1)
    var i = 0
    while (i < m) {
      enCommScores.set(i, 0, 1.0)
      i = i + 1
    }
    i = 0
    while (i < n) {
      cnCommScores.set(i, 0, 1.0)
      i = i + 1
    }
    for (_ <- 0 until iterations) {
      enCommScores = MoreMatrixOperations.columnNormalize(en_en.transpose().times(alpha).times(enCommScores)
        .plus(similarity_cn_en.transpose().times(beta).times(cnCommScores)))
      cnCommScores = MoreMatrixOperations.columnNormalize(cn_cn.transpose().times(alpha).times(cnCommScores)
        .plus(similarity_en_cn.transpose().times(beta).times(enCommScores)))
    }
    return (enCommScores, cnCommScores) 
  }
}