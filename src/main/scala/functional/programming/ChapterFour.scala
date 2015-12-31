package functional.programming

/**
 * @author lmedina
 */
object ChapterFour {
  def main(args: Array[String]) {
    println("map = " + Some(3).map(x => x + 2))

    println("flatMap = " + Some(3).flatMap(x => Some(1)))
    println("flatMap = " + None.flatMap(x => Some(1)))

    println("getOrElse = " + Some(3).getOrElse(2))
    println("getOrElse = " + None.getOrElse(2))

    println("orElse = " + Some(3).orElse(Some(2)))
    println("orElse = " + None.orElse(Some(2)))
    println("orElse2 = " + Some(3).orElse2(Some(2)))
    println("orElse2 = " + None.orElse2(Some(2)))

    println("filter = " + Some(3).filter(x => x % 2 != 0))
    println("filter = " + Some(3).filter(x => x % 2 == 0))

    println("variance = " + variance(Seq(1, 2, 3)))
    println("variance = " + variance(Seq()))

    println("map2 = " + map2(Some(1), Some(2))((a, b) => a + b))

    println("sequence = " + sequence(List(Some(1), Some(2), Some(3))))

    println("traverse = " + traverse(List(1,2,3))((x) => Some(x)))

    println("sequenceViaTraverse = " + sequenceViaTraverse(List(Some(1), Some(2), Some(3))))

  }

  sealed trait Option[+A] {
    /*
        Exercise 4.1
        Implement all of the preceding functions on Option. As you implement each function, try to think about what it
        means and in what situations you�d use it. We�ll explore when to use each of these functions next.
     */
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(get) => Some(f(get))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(get) => get
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }

    def flatMap2[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(get) => f(get)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(x => this) getOrElse ob
    }

    def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(x => if (f(x)) Some(x) else None)
    }

    def filter2(f: A => Boolean): Option[A] = this match {
      case Some(get) if f(get) => this
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /*
      Exercise 4.2
      Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean
      of math.pow(x - m, 2) for each element x in the sequence.
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /*
      Exercise 4.3
      Write a generic function map2 that combines two Option values using a binary function. If either Option value is
      None, then the return value is too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(aa => b map(bb => f(aa, bb)))
  }

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(v1), Some(v2)) => Some(f(v1, v2))
  }

  /*
        Exercise 4.4
        Write a function sequence that combines a list of Options into one Option containing a list of all the Some
        values in the original list. If the original list contains None even once, the result of the function should be
        None; otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence(t) map(hh :: _))
  }

  /*
  sequence[A](List(Option[1],Option[2]))
    case h :: t => h flatMap(hh => sequence(t) map(hh :: _))
    case Option[1] :: List(Option[2]) => Option[1] flatMap(1 => sequence(List(Option[2])) map(1 :: _))
      case h :: t => h flatMap(hh => sequence(t) map(hh :: _))
      case Option[2] :: List(Nil) => Option[2] flatMap(2 => sequence(List(Nil)) map(2 :: _))
        case Nil => Some(Nil)
      case Option[2] :: List(Nil) => Option[2] flatMap(2 => Some(Nil) map(2 :: Nil))
    case Option[1] :: List(Option[2]) => Option[1] flatMap(1 => Some(2 :: Nil) map(1 :: _))
  Some(1 :: 2 :: Nil)
  */

  /*
      Exercise 4.5
      Implement this function. It�s straightforward to do using map and sequence, but try for a more efficient
      implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
}
