package commlab.weejang

import scala.io.Source
import scala.PartialFunction.OrElse

object Utils {

  /**
   * 序列化对象，持久到硬盘上
   */
  def serializeObj(obj: AnyRef, uri: String): Unit = {
    import java.io._
    val out = new ObjectOutputStream(new FileOutputStream(uri))
    out.writeObject(obj)
    out.close()
  }

  /**
   * 反序列化对象
   */
  def deserializeObj[T](uri: String): T = {
    import java.io._
    val in = new ObjectInputStream(new FileInputStream(uri))
    in.readObject().asInstanceOf[T]
  }

  /**
   * 越往前，影响越小
   * 分割时间　１，３，６，１０，１５，２１，２８　为分割日期,起始2014-12-18 00
   * 输入格式：　2014-11-26　20
   * 返回下标　１－＞　０　; 3 -> 1 ....
   */
  def indexedDate(currentDate: String): Option[Int] = {
    val i = splitDate(currentDate).get
    if (i >= 28) Some(6)
    else if (i >= 21) Some(5)
    else if (i >= 15) Some(4)
    else if (i >= 10) Some(3)
    else if (i >= 6) Some(2)
    else if (i >= 3) Some(1)
    else Some(0)

  }

  def splitDate(currentDate: String): Option[Int] = {
    var retInt = 0
    currentDate.split(" ") match {
      case Array(ymd, h) => {
        ymd.split("-") match {
          case Array(y, m, d) => {
            Some((12 - m.toInt) * 30 + 18 - d.toInt )
          }
          case _ => None
        }
      }
      case _ => None
    }
  }
  
  /**
   * ４维向量相加
   */
  
  def tuple4Add(a:(Int,Int,Int,Int),b:(Int,Int,Int,Int)): (Int,Int,Int,Int) = {
      val new_a  = { if(a == null) (0,0,0,0) else a }
      (new_a._1 + b._1 ,new_a._2 + b._2 ,new_a._3 + b._3 , new_a._4 + b._4)
    }
   
  /**
   * 向一个可能为null的集合中添加元素，返回新集合
   */
  
  def optionSetAdd(optionSet:Set[String],str: String) : Set[String] = {
    val newSet:Set[String] =  if(optionSet == null) Set.empty else optionSet 
    newSet + str
  }
  
  /**
   * 删除一个可能为null的集合中的已经存在的元素，并返回新集合
   */
   def optionSetRemoveSameElem(optionSet:Set[String],str: String) : Set[String] = {
    val newSet:Set[String] =  if(optionSet == null) Set.empty else optionSet 
    if(newSet.contains(str)) {
      newSet - str
    }
    else {
      newSet
    }
    
  }
  
}  