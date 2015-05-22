package commlab.weejang

object Main {

  def main(args: Array[String]) {
        val userFile = "/home/jangwee/Tmall/tianchi_mobile_recommend_train_user.csv"
        new ItemFeature(userFile).basicItemFeature
  }

  def done() {
    val itemFile = "/home/jangwee/Tmall/tianchi_mobile_recommend_train_item.csv"
    val item = new ItemMap(itemFile)
    println(item.pItemId2CategoryMap.size)
    println(item.pItemId2GeohashMap.size)
    println(item.pItemId2CategoryMap("96406570").toString())
    println(item.pItemId2GeohashMap.getOrElse("96406570", Set.empty).size)

    val userFile = "/home/jangwee/Tmall/tianchi_mobile_recommend_train_user.csv"
    println(userFile)
    val user = new UserMap(userFile)
    println(user.category2ItemsMap.size)
    println(user.iItemId2CategoryMap.size)
    println(user.userId2ItemMap.size)
  }
}