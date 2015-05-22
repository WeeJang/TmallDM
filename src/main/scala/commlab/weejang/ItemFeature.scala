package commlab.weejang

/**
 * 品牌特征
 */

class ItemFeature(userFile: String) {

  /**
   * 基本统计信息,按１，３，６，１０，１５，２１，２８　为分割日期，进行统计
   * Item -> [(浏览，收藏，加购物车，购买)[time]]
   */

  // 周期个数
  private val periodNum: Int = 7

  lazy val basicItemFeature = {

    //Item -> [(浏览，收藏，加购物车，购买)[time]]

    val retMap = new scala.collection.mutable.HashMap[String, Array[(Int, Int, Int, Int)]]
    // 浏览用户集合
    val scanedUserSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 收藏用户集合
    val savedUserSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 购物车用户集合
    val addedUserSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 购买用户集合
    val buyedUserSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]

    // 浏览用户＋时间集合
    val scanedUserTimeSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 收藏用户+时间集合
    val savedUserTimeSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 购物车用户＋时间集合
    val addedUserTimeSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]
    // 购买用户＋时间集合
    val buyedUserTimeSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]

    //记录跳出用户集合（周期内只对品牌进行过１次点击操作的用户数）
    val clickOnceUserSet = new scala.collection.mutable.HashMap[String, Array[Set[String]]]

    val bufferedSource = scala.io.Source.fromFile(userFile)

    for (line <- bufferedSource.getLines()) {
      line.split(",") match {
        case Array("user_id", "item_id", "behavior_type", "user_geohash", "item_category", "time") => {}
        case Array(user_id, item_id, behavior_type, user_geohash, item_category, time) => {

          val itemStatisticArrays = retMap.getOrElse(item_id, new Array[(Int, Int, Int, Int)](periodNum))

          val itemScanedUserSetArrays = scanedUserSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemSavedUserSetArrays = savedUserSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemAddedUserSetArrays = addedUserSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemBuyedUserSetArrays = buyedUserSet.getOrElse(item_id, new Array[Set[String]](periodNum))

          val itemScanedUserTimeSetArrays = scanedUserTimeSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemSavedUserTimeSetArrays = savedUserTimeSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemAddedUserTimeSetArrays = addedUserTimeSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          val itemBuyedUserTimeSetArrays = buyedUserTimeSet.getOrElse(item_id, new Array[Set[String]](periodNum))

          val clickOnceUserSetArrays = clickOnceUserSet.getOrElse(item_id, new Array[Set[String]](periodNum))
          
          
          
          
          val index = Utils.indexedDate(time).get

          behavior_type match {
            case "0" => {
              itemStatisticArrays(index) = Utils.tuple4Add(itemStatisticArrays(index), (1, 0, 0, 0))

              //判断是否之前出现过
              if (itemScanedUserSetArrays(index) == null) {
                clickOnceUserSetArrays(index) = Set(user_id)
              } else if (!itemScanedUserSetArrays(index).contains(user_id)) {
                clickOnceUserSetArrays.update(index, Utils.optionSetAdd(clickOnceUserSetArrays(index), user_id))
              } else {
                clickOnceUserSetArrays.update(index, Utils.optionSetRemoveSameElem(clickOnceUserSetArrays(index), user_id))
              }
              clickOnceUserSet(item_id) = clickOnceUserSetArrays

              itemScanedUserSetArrays.update(index, Utils.optionSetAdd(itemScanedUserSetArrays(index), user_id))
              scanedUserSet(item_id) = itemScanedUserSetArrays

              itemScanedUserTimeSetArrays.update(index, Utils.optionSetAdd(itemScanedUserTimeSetArrays(index), user_id + time))
              scanedUserTimeSet(item_id) = itemScanedUserTimeSetArrays

            }
            case "1" => {
              itemStatisticArrays(index) = Utils.tuple4Add(itemStatisticArrays(index), (0, 1, 0, 0))
              itemSavedUserSetArrays.update(index, Utils.optionSetAdd(itemSavedUserSetArrays(index), user_id))
              savedUserSet(item_id) = itemSavedUserSetArrays
              itemSavedUserTimeSetArrays.update(index, Utils.optionSetAdd(itemSavedUserTimeSetArrays(index), user_id + time))
              savedUserTimeSet(item_id) = itemSavedUserTimeSetArrays
            }
            case "2" => {
              itemStatisticArrays(index) = Utils.tuple4Add(itemStatisticArrays(index), (0, 0, 1, 0))
              itemAddedUserSetArrays.update(index, Utils.optionSetAdd(itemAddedUserSetArrays(index), user_id))
              addedUserSet(item_id) = itemAddedUserSetArrays
              itemAddedUserTimeSetArrays.update(index, Utils.optionSetAdd(itemAddedUserTimeSetArrays(index), user_id + time))
              addedUserTimeSet(item_id) = itemAddedUserTimeSetArrays
            }
            case "3" => {
              itemStatisticArrays(index) = Utils.tuple4Add(itemStatisticArrays(index), (0, 0, 0, 1))
              itemBuyedUserSetArrays.update(index, Utils.optionSetAdd(itemBuyedUserSetArrays(index), user_id))
              buyedUserSet(item_id) = itemBuyedUserSetArrays
              itemBuyedUserTimeSetArrays.update(index, Utils.optionSetAdd(itemBuyedUserTimeSetArrays(index), user_id + time))
              buyedUserTimeSet(item_id) = itemBuyedUserTimeSetArrays

            }
            case _ => { println("bad behavior_type") }
          }
          retMap(item_id) = itemStatisticArrays
        }
        case _ => { println("dirty data") }
      }
    }
    bufferedSource.close

    //整理特征
    // 1天前　－－　３天前　－－６天前　－－　１０天前　－－　１５天前　－－　２１　天前　－－　２８　天前
    //   |
    // [点击数，收藏数，购物车数，购买数，　点击数，收藏数，购物车数，购买数，点击数，收藏数，购物车数，购买数　，　　．．． ->基本特征（１２）
    // 　＿＿＿＿　瞬时　＿＿＿＿＿＿＿＿｜＿＿＿＿＿＿针对UserID去重＿＿＿｜＿＿针对UserID+time_____去重｜
    // ...　收藏数／点击数，购物车数／点击数，购买数／点击数，　．．．                                                        ９
    //　　　＿＿＿＿　瞬时　＿＿＿＿＿＿＿＿｜＿＿＿＿＿＿针对UserID去重＿＿＿｜＿＿针对UserID+time_____去重｜                    
    // ... 点击反客率（多次点击用户数／总购物数），老点击客户率（3天前点击，其他周期再次点击）                                   　８＊
    //
  }

}