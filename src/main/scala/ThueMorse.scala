package thue.morse

case class TEntry(v: List[Boolean])

case class Accumulator(v : List[TEntry] = List(TEntry(List(false)))) {
  def withNext = Accumulator(nextValue::v)
  def nextValue = TEntry(result.map(!_))

  def result: List[Boolean] = v.reverse.map(_.v).flatten
  def resultAsString = result.map { case true => 1 case _ => 0 }.mkString("")
}

object ThueMorse {
  def compute(n: Int): Accumulator = compute(Accumulator(), n)
  def compute(acc: Accumulator, n: Int): Accumulator = {
    if(n > 0) compute(acc.withNext, n-1)
    else acc
  }


  def invStr(s: String) = s.map { c => c match {
      case '0' => '1'
      case _   => '0'
    }
  }
  def strCompute(n: Int): String = strCompute("0", n)
  def strCompute(acc: String, n: Int): String = {
    if(n > 0) strCompute(acc+invStr(acc), n-1)
    else acc
  }

  def iterCompute(n: Int) = {
    var nn = n
    var r: List[Boolean] = List(false)
    while(nn > 0) {
      r = r:::r.map(!_)
      nn -= 1
    }
    r
  }
}

object ThueMorseCompute {
  def main(args: Array[String]): Unit = {
    val nth: Int = args.toList.headOption.map(_.toInt).getOrElse(10)
    val kind: String = args.toList.tail.headOption.getOrElse("")

    kind match {
      case "iter" => time { ThueMorse.iterCompute(nth) }
      case "str" => time { ThueMorse.strCompute(nth) } //.resultAsString
      case "rec"  => time { ThueMorse.compute(nth) }//.resultAsString
      case _  => {
        println("recursive caseclass")
        time { ThueMorse.compute(nth) }

        println("iterative")
        time { ThueMorse.iterCompute(nth) }

        println("recursive str")
        time { ThueMorse.strCompute(nth) }
      }
    }
  }
  def time[A](a: => A) = {
    val now = System.nanoTime
    val res = a
    val seconds = (System.nanoTime - now) / 1000 / 1000.0 / 1000.0
    println(s"$seconds seconds")

    res
  }

}
