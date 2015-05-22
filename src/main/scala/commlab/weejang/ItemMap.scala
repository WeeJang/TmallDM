package commlab.weejang

import scala.collection.immutable.HashMap

/**
 * 商品关系
 */

class ItemMap(itemFile: String) {

  private[this] val serializedPItemId2CategoryMapObjUri = "/home/jangwee/Tmall/serializedData/pItemId2GateoryMap.obj"
  private[this] val serializedPItemId2GeohashMapObjUri = "/home/jangwee/Tmall/serializedData/pItemId2GeohashMap.obj"

  /**
   * isSerialized
   * 判断是否持久化映射关系
   */
  var isSerialized: Boolean = {
    new java.io.File(serializedPItemId2CategoryMapObjUri).exists &&
      new java.io.File(serializedPItemId2GeohashMapObjUri).exists
  }

  /**
   * pItemId2Category
   *    item_id -> item_category映射表
   *    Map[item_id,item_category]
   */
  lazy val pItemId2CategoryMap: Map[String, String] = {
    isSerialized match {
      case true => { Map.empty ++ Utils.deserializeObj(serializedPItemId2CategoryMapObjUri) }
      case false => { init._1 }
    }
  }

  /**
   * pItemId2Geohash
   *    item_id -> item_geohash映射表
   *    Map[item_id,item_geohash_set]
   */
  lazy val pItemId2GeohashMap: Map[String, Set[String]] = {
    isSerialized match {
      case true => { Map.empty ++ Utils.deserializeObj(serializedPItemId2GeohashMapObjUri) }
      case false => { init._2 }
    }
  }

  /**
   * 初始化，建立映射关系
   */
  def init(): (Map[String, String], Map[String, Set[String]]) = {

    val innerId2CategoryMap = new scala.collection.mutable.HashMap[String, String]()
    val innerId2GeohashMap = new scala.collection.mutable.HashMap[String, Set[String]]()

    val bufferedSource = scala.io.Source.fromFile(itemFile)

    for (line <- bufferedSource.getLines()) {
      line.split(",") match {
        case Array("item_id", "item_geohash", "item_category") => {}
        case Array(a, b, c) => {
          innerId2CategoryMap(a) = c
          b match {
            case "" => {}
            case geo: String => { innerId2GeohashMap(a) = (innerId2GeohashMap.getOrElse(a, Set.empty) + geo) }
            case _ => { println("dirty data") }
          }
        }
        case _ => { println("dirty data") }
      }
    }
    bufferedSource.close()
    
    val retTuple2 = (Map.empty ++ innerId2CategoryMap, Map.empty ++ innerId2GeohashMap)

    //序列化对象
    Utils.serializeObj(innerId2CategoryMap, serializedPItemId2CategoryMapObjUri)
    Utils.serializeObj(innerId2GeohashMap, serializedPItemId2GeohashMapObjUri)
    isSerialized = true
   
    retTuple2
  }

}