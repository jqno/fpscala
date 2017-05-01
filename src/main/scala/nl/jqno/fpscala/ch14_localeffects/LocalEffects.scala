package nl.jqno.fpscala.ch14_localeffects

import nl.jqno.fpscala.ch11_monads._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S,A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S,List[A]] = ST(value.toList)


  // Exercise 14.1: STArray.fill
  def fill(xs: Map[Int,A]): ST[S,Unit] =
    if (xs.isEmpty)
      ST(())
    else {
      val (k, v) = xs.head
      for {
        _ <- write(k, v)
        _ <- fill(xs.tail)
      } yield ()
    }


  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  def noop[S] = ST[S,Unit](())


  // Exercise 14.2: quicksort
  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    pivotVal <- a.read(pivot)
    _        <- a.swap(pivot, r)
    j        <- STRef(l)
    _        <- (l until r).foldLeft(noop[S])((s, i) => for {
                  _  <- s   // I don't complete get why this is needed but it doesn't work without it
                  x  <- a.read(i)
                  _  <- if (x < pivotVal) {
                          for {
                            j1 <- j.read
                            _  <- a.swap(i, j1)
                            _  <- j.write(j1 + 1)
                          } yield ()
                        }
                        else {
                          noop[S]
                        }
                } yield ())
    j1       <- j.read
    _        <- a.swap(j1, r)
  } yield j1

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] =
    if (l >= r) ST(())
    else for {
      pi <- partition(a, l, r, l + (r - l) / 2)
      _  <- qs(a, l, pi - 1)
      _  <- qs(a, pi + 1, r)
    } yield ()



  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
  })
}

import scala.collection.mutable.HashMap


// Exercise 14.3: mutable HashMap
sealed abstract class STMap[S,K,V] {
  protected def value: HashMap[K,V]
  def size: ST[S,Int] = ST(value.size)

  // Write a value for the given key in the map
  def write(k: K, v: V): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(k) = v
      ((), s)
    }
  }

  // Read the value for the given key in the map
  def read(i: K): ST[S,V] = ST(value(i))

  // Turn the map into an immutable map
  def freeze: ST[S,Map[K,V]] = ST(value.toMap)
}

object STMap {
  // Construct an array of the given size filled with the value v
  def apply[S,K,V](): ST[S, STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      lazy val value = HashMap.empty[K,V]
    })

  def fromMap[S,K,V](m: Map[K,V]): ST[S, STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      lazy val value = scala.collection.mutable.HashMap(m.toList: _*)
    })
}

