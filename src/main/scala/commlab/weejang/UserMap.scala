package commlab.weejang

import scala.util.Sorting
import scala.collection.mutable.{HashMap => mHashMap}


/**
 * 用户关系
 */

class UserMap(userFile: String) {

  private[this] val serializedUserId2ItemMapObjUri = "/home/jangwee/Tmall/serializedData/UserId2ItemMap.obj"
  private[this] val serializedIItemId2CategoryMapObjUri = "/home/jangwee/Tmall/serializedData/IItemId2CategoryMap.obj"
  private[this] val serializedCategory2ItemsMapObjUri = "/home/jangwee/Tmall/serializedData/Category2Items.obj"
  
  var isSerialized: Boolean = {
    new java.io.File(serializedUserId2ItemMapObjUri).exists &&
      new java.io.File(serializedIItemId2CategoryMapObjUri).exists &&
        new java.io.File(serializedCategory2ItemsMapObjUri).exists 
  }

  /**
   * 用户到商品的映射，包含行为信息
   * user -> [ item -> (timeStamp,behavior,geohash) ]
   */
  lazy val userId2ItemMap: Map[String, Map[String, Array[(String, String,String)]]] = {
    isSerialized match {
      case true => { Utils.deserializeObj(serializedUserId2ItemMapObjUri) }
      case false => { init._1 }
    }
  }

  /**
   * 商品全集到商品种类的映射
   * item -> category
   */
  lazy val iItemId2CategoryMap: Map[String, String] = {
    isSerialized match {
      case true => { Utils.deserializeObj(serializedIItemId2CategoryMapObjUri) }
      case false => { init._2 }
    }
  }

  /**
   * 商品种类包含的商品
   * category -> Set(item)
   */
  lazy val category2ItemsMap: Map[String, Set[String]] = {
    isSerialized match {
      case true => { Utils.deserializeObj(serializedCategory2ItemsMapObjUri) }
      case false => { init._3 }
    }
  }

  /**
   * 初始化，
   */
  def init(): (Map[String, Map[String, Array[(String,String,String)]]], Map[String, String], Map[String, Set[String]]) = {
    import scala.collection.mutable.{HashMap => mHashMap}
    val innerUserId2ItemMap = new mHashMap[String, Map[String, Array[(String, String, String)]]]()
    val innerIItemId2CategoryMap = new mHashMap[String, String]()
    val innerCategory2ItemsMap = new mHashMap[String, Set[String]]()

    val bufferedSource = scala.io.Source.fromFile(userFile)
    for (line <- bufferedSource.getLines()) {
      line.split(",") match {
        case Array("user_id", "item_id", "behavior_type", "user_geohash", "item_category", "time") => {}
        case Array(user_id, item_id, behavior_type, user_geohash, item_category, time) => {
          innerIItemId2CategoryMap(item_id) = item_category
          innerCategory2ItemsMap(item_category) = ( innerCategory2ItemsMap.getOrElse(item_category, Set.empty) + item_id )
          
          val itemInfoMap = innerUserId2ItemMap.getOrElse(user_id,Map.empty)
          val itemInfoBehaviorArray = (itemInfoMap.getOrElse(item_id, Array.empty) ++ Array((time,behavior_type,user_geohash)))
          //按时间排序
          Sorting.quickSort(itemInfoBehaviorArray)
          val newItemInfoMap = itemInfoMap.updated(item_id,itemInfoBehaviorArray)
          innerUserId2ItemMap(user_id) = newItemInfoMap
        }
        case _ => { println("dirty data") }
      }
    }
    bufferedSource.close()
    
    val retTuple3 =  (Map.empty ++ innerUserId2ItemMap , Map.empty ++ innerIItemId2CategoryMap , Map.empty ++ innerCategory2ItemsMap )
    
    Utils.serializeObj(retTuple3._1, serializedUserId2ItemMapObjUri)
    Utils.serializeObj(retTuple3._2, serializedIItemId2CategoryMapObjUri)
    Utils.serializeObj(retTuple3._3, serializedCategory2ItemsMapObjUri)
    
    isSerialized = true
    
    retTuple3
  }
}