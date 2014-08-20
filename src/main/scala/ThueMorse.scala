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

  def subsitution(s: String) = s.map { _ match {
    case '0' => "01"
    case _ => "10"
  } }.mkString
  def strSubstitutionCompute(n: Int): String = strSubstitutionCompute("0", n)
  def strSubstitutionCompute(acc: String, n: Int): String = {
    if(n > 0) strSubstitutionCompute(subsitution(acc), n-1)
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


  def count_set_bits(nn: Int) = {
    var count = 0
    var n = nn
    while(n != 0) {
      n &= (n-1)
      count += 1
    }
    count
  }
  def bitCompute(n: Int) = {
    val top = scala.math.pow(2.toInt, n).toInt - 1
    (0 to top).map { x =>
      if(count_set_bits(x) % 2 != 0) '1' else '0'
    }.mkString
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
      case "sub"  => time { ThueMorse.strSubstitutionCompute(nth) }//.resultAsString
      case "bit"  => time { ThueMorse.bitCompute(nth) }//.resultAsString
      case _  => {
        println("recursive caseclass")
        time { ThueMorse.compute(nth) }

        println("iterative")
        time { ThueMorse.iterCompute(nth) }

        println("recursive str")
        time { ThueMorse.strCompute(nth) }

        println("str substituion")
        time { ThueMorse.strSubstitutionCompute(nth) }

        println("bit style")
        time { ThueMorse.bitCompute(nth) }
      }
    }
  }
  def time[A](a: => A) = {
    val res = (1 to 25).map { i =>
      val now = System.nanoTime
      a
      ((System.nanoTime - now) / 1000 / 1000.0 / 1000.0)
    }

    println("took "+(res.sum/res.length)+" seconds")
  }

}
