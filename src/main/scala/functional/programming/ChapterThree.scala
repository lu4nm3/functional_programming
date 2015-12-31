package functional.programming

/**
 * @author lmedina
 */
object ChapterThree {
  def main(args: Array[String]) {
    lists()
    trees()
  }

  def lists() = {
    import ChapterThree.List._

    println("tail = " + tail(List(1, 2, 3)))

    println("drop = " + drop(List(1, 2, 3, 4, 5), 2))
    println("drop = " + drop(List(1, 2, 3, 4, 5), 0))

    println("drdropWhileop = " + dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x % 2 == 0))

    println("init = " + init(List(1, 2, 3, 4, 5)))

    // Exercise 3.8
    println("foldRight = " + foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    println("length = " + length(List(1,2,3)))

    println("sumLeft = " + sumLeft(List(1,2,3)))
    println("productLeft = " + productLeft(List(1,2,3)))
    println("lengthLeft = " + lengthLeft(List(1,2,3)))

    println("reverse = " + reverse(List(1,2,3)))

    println("append = " + append(List(1,2,3), 4))

    println("concat = " + concat(List(List(1,2,3), List(4,5,6))))
    println("myConcat = " + myConcat(List(List(1,2,3), List(4,5,6))))

    println("addOne = " + addOne(List(1,2,3)))
    println("addOne2 = " + addOne2(List(1,2,3)))

    println("doubleToString = " + doubleToString(List(1,2,3)))
    println("doubleToString2 = " + doubleToString2(List(1,2,3)))

    println("multByTwo = " + map(List(1,2,3))(_ * 2))
    println("multByThree = " + map2(List(1,2,3))(_ * 3))

    println("filterOddNumbers = " + filterOddNumbers(List(1,2,3,4,5)))
    println("filterEvenNumbers = " + filterEvenNumbers(List(1,2,3,4,5)))

    println("flatMap = " + flatMap(List(1,2,3))(i => List(i,i)))
    println("flatMap2 = " + flatMap(List(1,2,3))(i => List(i,i)))

    println("addLists = " + addLists(List(1,2,3), List(4,5,6)))

    println("zipWith = " + zipWith(List(1,2,3), List(4,5,6))(_ + _))

    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(1,2,3)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(1,2)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(2,3)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(1)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), Nil))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(4,5)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(2,3,4)))
    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(1,2,4)))

    println("hasSubsequence = " + hasSubsequence(List(1,2,3), List(1,2,4)))
  }

  def trees() = {
    import ChapterThree.Tree._

    println("size = " + size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    println("maximum = " + maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    println("maxDepth = " + depth(Branch(Leaf(1), Branch(Branch(Branch(Leaf(4), Leaf(5)), Leaf(2)), Leaf(3)))))
    println("maxDepth = " + depth(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(2)), Leaf(3)))))
    println("maxDepth = " + depth(Leaf(3)))

    println("map = " + map(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(2)), Leaf(3))))(_ + 1))

    println("size2 = " + size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    println("maximum2 = " + maximum2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    println("maxDepth2 = " + depth2(Branch(Leaf(1), Branch(Branch(Branch(Leaf(4), Leaf(5)), Leaf(2)), Leaf(3)))))
    println("maxDepth2 = " + depth2(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(2)), Leaf(3)))))
    println("maxDepth2 = " + depth2(Leaf(3)))

    println("map2 = " + map2(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(2)), Leaf(3))))(_ + 1))
  }
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    /*
        Exercise 3.2
        Implement the function tail for removing the first element of a List.
     */
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Can't get tail of empty list.")
      case Cons(_, t) => t
    }

    /*
        Exercise 3.3
        Using the same idea, implement the function setHead for replacing the first element
        of a List with a different value.
     */
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => throw new Exception("Can't set head of empty list.")
      case Cons(_, t) => Cons(h, t)
    }

    /*
        Exercise 3.4
        Generalize tail to the function drop, which removes the first n elements from a list.
     */
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

    /*
        Exercise 3.5
        Implement dropWhile, which removes elements from the List prefix as long as they
        match a predicate.
     */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else Cons(h, dropWhile(t, f))
    }

    /*
        Exercise 3.6
        Implement a function, init, that returns a List consisting of all but the last element of
        a List. So, given List(1,2,3,4), init will return List(1,2,3).
     */
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    /*
    * Note that we're copying the entire list up until the last element. Besides being inefficient, the natural
    * recursive solution will use a stack frame for each element of the list, which can lead to stack overflows for
    * large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
    * function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
    * buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
    */
    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => List(buf.toList: _*)
        case Cons(h,t) => buf += h; go(t)
      }

      go(l)
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    /*
        Exercise 3.9
        Compute the length of a list using foldRight.
     */
    def length[A](l: List[A]): Int = {
      foldRight(l, 0)((_, b) => 1 + b)
    }

    /*
        Exercise 3.10
        Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists
        (we say it�s not stack-safe). Convince yourself that this is the case, and then write another general
        list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous
        chapter.
     */
    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /*
       foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
       foldLeft(Cons(2, Cons(3, Nil)), 1)((x,y) => x + y)
       foldLeft(Cons(3, Nil), 3)((x,y) => x + y)
       foldLeft(Nil, 6)((x,y) => x + y)
       6
    */

    /*
        Exercise 3.11
        Write sum, product, and a function to compute the length of a list using foldLeft.
     */
    def sumLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)
    def productLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
    def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

    /*
        Exercise 3.12
        Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
     */
    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))
    }

    /*
       foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil)((t, h) => Cons(h, t))
       foldLeft(Cons(2, Cons(3, Nil)), Cons(1, Nil))((t, h) => Cons(h, t))
       foldLeft(Cons(3, Nil), Cons(2, Cons(1, Nil)))((t, h) => Cons(h, t))
       Cons(3, Cons(2, Cons(1, Nil)))
    */

    /*
        Exercise 3.14
        Implement append in terms of either foldLeft or foldRight.
     */
    def append[A](l: List[A], a: A): List[A] = {
      foldRight(l, Cons(a, Nil))((h, t) => Cons(h, t))
    }

    /*
        Exercise 3.15
        Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in
        the total length of all lists. Try to use functions we have already defined.
     */
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil:List[A])(append)

    def myConcat[A](ll: List[List[A]]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      def loop(ll2: List[List[A]]): List[A] = ll2 match {
        case Nil => List(buf.toList: _*)
        case Cons(h, t) => internal(h); loop(t)
      }

      def internal(l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) => buf += x; internal(xs)
      }

      loop(ll)
    }

    /*
        Exercise 3.16
        Write a function that transforms a list of integers by adding 1 to each element.
     */
    def addOne(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

    def addOne2(l: List[Int]): List[Int] = {
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
    }

    /*
        Exercise 3.17
        Write a function that turns each value in a List[Double] into a String. You
     */
    def doubleToString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

    def doubleToString2(l: List[Double]): List[String] = {
      foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t) )
    }

    /*
        Exercise 3.18
        Write a function map that generalizes modifying each element in a list while maintaining the
        structure of the list.
     */
    def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def map2[A,B](l: List[A])(f: A => B): List[B] = {
      foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
    }

    /*
        Exercise 3.19
        Write a function filter that removes elements from a list unless they satisfy a given predicate.
        Use it to remove all odd numbers from a List[Int].
     */
    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) filter(t)(f)
        else Cons(h, filter(t)(f))
    }

    def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
      foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
    }

    def filterOddNumbers(l: List[Int]): List[Int] = {
      filter(l)(_ % 2 != 0)
    }

    def filterEvenNumbers(l: List[Int]): List[Int] = {
      filter(l)(_ % 2 == 0)
    }

    /*
        Exercise 3.20
        Write a function flatMap that works like map except that the function given will return a list instead
        of a single result, and that list should be inserted into the final resulting list.
        For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
     */
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
      def loop(l2: List[A]): List[List[B]] = l2 match {
        case Nil => Nil
        case Cons(h, t) => Cons(f(h), loop(t))
      }

      concat(loop(l))
    }

    def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = {
      concat(map(l)(f))
    }

    /*
        Exercise 3.21
        Use flatMap to implement filter.
     */
    def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
      flatMap(l)(a => if (f(a)) List(a) else Nil)
    }

    /*
        Exercise 3.22
        Write a function that accepts two lists and constructs a new list by adding corresponding
        elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
     */
    def addLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }

    /*
        Exercise 3.23
        Write a function that accepts two lists and constructs a new list by adding corresponding
        elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
     */
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    /*
        Exercise 3.24
        Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a
        subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among
        others. You may have difficulty finding a concise purely functional implementation that is also efficient.
        That�s okay. Implement the function however comes most naturally. We�ll return to this implementation in
        chapter 5 and hopefully improve on it. Note: Any two values x and y can be compared for equality in Scala using
        the expression x == y.
     */
    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) hasSubsequence(t1, t2)
        else hasSubsequence(t1, sub)
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    /*
        Exercise 3.25
        Write a function size that counts the number of nodes (leaves and branches) in a tree.
     */
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    /*
        Exercise 3.26
        Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use
        x.max(y) or x max y to compute the maximum of two integers x and y.)
     */
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    /*
        Exercise 3.27
        Write a function depth that returns the maximum path length from the root of a tree to any leaf.
     */
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

    /*
        Exercise 3.28
        Write a function map, analogous to the method of the same name on List, that modifies each element in a tree
        with a given function.
     */
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    /*
        Exercise 3.29
        Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
        Reimplement them in terms of this more general function. Can you draw an analogy between this fold function
        and the left and right folds for List?
     */
    def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
      case Leaf(v) => l(v)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

    def size2[A](t: Tree[A]): Int = {
      fold(t)(a => 1)(1 + _ + _)
    }

    def maximum2(t: Tree[Int]): Int = {
      fold(t)(x => x)(_ max _)
    }

    def depth2[A](t: Tree[A]): Int = {
      fold(t)(_ => 1)((b1, b2) => (1 + b1) max (1 + b2))
    }

    def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = {
      fold(t)(x => Leaf(f(x)): Tree[B])((b1, b2) => Branch(b1, b2))
    }
  }
}
