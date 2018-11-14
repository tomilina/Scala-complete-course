package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] =
      (left, right) match {
        case (Nil, _) => right
        case (_, Nil) => left
        case (l :: lt, r :: rt) =>
          if (l < r) l +: merge(lt, right)
          else r +: merge(left, rt)
      }

    val n = data.length / 2
    if (n == 0) data
    else {
      val (left, right) = data splitAt n
      merge(mergeSort(left), mergeSort(right))
    }
  }

  print(mergeSort(Seq(3, 4, 2, 5, 1)));

}
