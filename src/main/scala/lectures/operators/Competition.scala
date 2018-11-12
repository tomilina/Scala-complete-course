package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо
  * исправить ошибки компиляции
  * напечатать:
  * * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * * "Продули", если победили приезжие
  * * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  * В целях упрощения можно поменять тип исходных данных
  */

object Competition extends App {

  val locals = Map("Artem" -> 6, "Sergey" -> 5, "Anton" -> 2, "Vladimir" -> "2", "Alexander" -> 4D)
  val foreigners = Map[String, Int]("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

  def toInt(x: Any): Int = x match {
    case i: Int => i
    case s: String => s.toInt
    case d: Double => d.toInt
    case a: Any => a.toString.toInt
    case _ => throw new Exception("Not a number!")
  }

  val results = for {
    (l_name, l_score) <- locals
    (f_name, f_score) <- foreigners
  } yield  l_name + " vs " + f_name -> (toInt(l_score) - toInt(f_score))

  var finalResult = 0
  for ((title, r: Int) <- results) {
    if (r > 0) finalResult += 1
    else finalResult -= 1
  }

  if (finalResult > 0) println("Наша взяла")
  else if (finalResult < 0) println("Продули")
  else println("Победила дружба")

}
