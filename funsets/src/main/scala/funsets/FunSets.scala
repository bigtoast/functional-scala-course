package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = { in :Int => in == elem }
  
  def add( s :Set, elem :Int ) :Set = { in :Int =>
    if ( contains(s, elem) )
       contains(s, in)
    else { 
       contains(singletonSet(elem),in ) || contains(s, in)  
    }
  }
  
  def remove( s :Set, elem :Int ): Set = { in :Int =>
    if ( elem == in )
      false
    else
      contains(s, in )
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = { in :Int =>
    contains( s, in ) || contains( t, in)  
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = { in :Int =>
    contains(s, in ) && contains( t, in )  
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = { in : Int =>
    union(s,t)(in) && ! intersect(s,t)(in)
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = { in :Int =>
    contains(s, in ) && p(in)
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == bound + 1) 
        true
      else if ( contains(s, a) && ! p(a) )
        false
      else iter(a + 1)
    }
    iter(- bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = 
    ! forall(s, x => ! p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = { in :Int => 
    exists(s, x => f(x) == in )
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
