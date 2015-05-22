package commlab.weejang

import scala.io.Source

/**
 * 指标评定
 */

class Evaluation(val predictionSetFile: String,
  val referenceSetFile: String) {

  /**
   * 交集大小
   */
  lazy val sizeOfCrossSet: Long = {
  
    println("lazy........")
    
    def createImmutableHashMap(setFile: String): Map[String, Set[String]] = {
      import scala.collection.immutable.HashSet

      val retMap = new scala.collection.mutable.HashMap[String, Set[String]]()
      val bufferedSource = Source.fromFile(setFile)
      for (line <- bufferedSource.getLines()) {
        val cols = line.split(",")
        retMap(cols(0)) = retMap.getOrElse(cols(0), Set.empty) + cols(1)
      }
      bufferedSource.close()
      Map.empty ++ retMap
    }

    val referenceImMap = createImmutableHashMap(referenceSetFile)

    val predictBufferedSource = Source.fromFile(predictionSetFile)
    var count = 0

    for (line <- predictBufferedSource.getLines()) {
      line.split(",") match {
        case Array(a, b, _*) => {
          count += { if (referenceImMap.getOrElse(a, Set.empty).contains(b)) 1 else 0 }
        }
        case _ => { println("dirty data") }
      }
    }
    count
  }

  /**
   * 求取精确度
   */
  lazy val precision: Double = {
    sizeOfCrossSet.toDouble / Evaluation.sizeOfSet(predictionSetFile)
  }

  /**
   * 求取召回率
   */
  lazy val recall: Double = {
    sizeOfCrossSet.toDouble / Evaluation.sizeOfSet(referenceSetFile)
  }

  /**
   * 求取 F1
   */
  lazy val F1Value: Double = {
    2 * precision * recall / (precision + recall)
  }

}

object Evaluation {
  /**
   * 集合大小
   */
  def sizeOfSet(setFile: String): Long = {
    Source.fromFile(setFile).getLines().length
  }

  def apply(predictionItemFile: String, referenceItemFile: String) = new Evaluation(predictionItemFile, referenceItemFile)

}