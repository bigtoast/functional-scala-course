package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val evens = union( s2, x => x % 2 == 0 )
    val odds = union( s1, x => x % 2 == 1 )
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }
  
  test("add to set then contains both"){    
    new TestSets {
      val two = add( s1, 5 )
      assert(contains(two,5),"Contains added")
      assert(contains(two,1), "Contains original")
      assert(! contains(two,6), "Doesn't contain what it shouldn't")
    }
  }
  
  test("remove from a set then contains one"){    
    new TestSets {
      val two = add( s1, 5 )
      val one = remove( two, 5 )
      assert(!contains(one,5),"Doesn't contain removed")
      assert(contains(one,1), "Contains original")
      assert(! contains(one,6), "Doesn't contain what it shouldn't")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains only what is in both"){
    new TestSets {
      val a = add(add(add(union(union(s1, s2),s3),4),5),6)
      val b = add(add(add(add(add(add(s2, 4),5),6),7),8),9)
      val i = intersect(a,b)
      assert(!contains(i,1), "doesn't have 1")
      assert(!contains(i,3), "doesn't have 3")
      assert(!contains(i,7), "doesn't have 7")
      assert(!contains(i,8), "doesn't have 8")
      assert(!contains(i,9), "doesn't have 9")
      
      assert(contains(i,2), "does have 2")
      assert(contains(i,4), "does have 4")
      assert(contains(i,5), "does have 5")
      assert(contains(i,6), "does have 6")
      
    }
  }
  
  test("diff contains only what is not shared"){
    new TestSets {
      val a = add(add(add(union(union(s1, s2),s3),4),5),6)
      val b = add(add(add(add(add(add(s2, 4),5),6),7),8),9)
      val i = diff(a,b)
      assert(contains(i,1), "does have 1")
      assert(contains(i,3), "does have 3")
      
      assert(!contains(i,7), "doesn't have 7")
      assert(!contains(i,8), "doesn't have 8")
      assert(!contains(i,9), "doesn't have 9")
      
      assert(!contains(i,2), "doesn't have 2")
      assert(!contains(i,4), "doesn't have 4")
      assert(!contains(i,5), "doesn't have 5")
      assert(!contains(i,6), "doesn't have 6")
    }
  }
  
  test("diff {1,3,4,5,7,1000} and {1,2,3,4} should be {5,7,1000}"){
    new TestSets {
      val a = union(union(union(union(union(s1, s3),singletonSet(4)),singletonSet(5)),singletonSet(7)),singletonSet(1000))
      val b = union(union(union(s1, s2),singletonSet(3)),singletonSet(4))
      val i = diff(a,b)
      //assert(contains(i,2), "does have 2")
      assert(contains(i,5), "does have 5")
      assert(contains(i,7), "does have 7")
      assert(contains(i,1000), "does have 1000")
      
      assert(!contains(i,2), "doesn't have 2")
      assert(!contains(i,1), "doesn't have 1")
      assert(!contains(i,3), "doesn't have 3")
      assert(!contains(i,4), "doesn't have 4")
      
      assert(FunSets.toString(i) == "{5,7,1000}", "tostring matches")
    }
  }
  
  test("{-1000,0} and {1,2,3,4}"){
    new TestSets {
      val a = union(singletonSet(0),singletonSet(-1000))
      val b = union(union(union(s1, s2),singletonSet(3)),singletonSet(4))
      val i = diff(a,b)
      assert(!contains(i,2), "doesn't have 2")
      
      assert(contains(i,0), "does have 0")
      assert(contains(i,-1000), "does have -1000")
      
      assert(!contains(i,1), "doesn't have 1")
      assert(!contains(i,3), "doesn't have 3")
      assert(!contains(i,4), "doesn't have 4")
      
      assert(exists(i, _ == 0), "0 exists")
      assert(exists(i, _ == -1000), "-1000 exists" )
      
      assert(FunSets.toString(i) == "{-1000,0}","to string matches")
    }
  }
  
  test("forall passes when it should"){
    new TestSets {
      val even = add(add(add(add(add(s2,12),4),6),8),10)
      assert( forall(even, x => x % 2 == 0 ) )
      assert( forall(evens, x => x % 2 == 0 ) )
    }
  }
  
  test("forall doesn't pass when it shouldn't"){
    new TestSets {
      val mix = add(add(add(add(add(add(s2,12),4),6),8),10),11)
      
      assert(! forall(mix, x => x % 2 == 0 ) )
    }
  }
  
  test("exists should find  666"){
    val s = singletonSet(666)
    assert(exists(s, _ == 666 ) ) 
  }
  
  test("exists should not find out of bounds"){
    val s = add(singletonSet(-1111),1111)
    assert(! exists(s, x => math.abs(x) == 1111))
  }
  
  test("mapping should turn evens into odds"){
    new TestSets {
      val newOdd = map(evens, _ - 1 )
      assert(! forall( newOdd, _ % 2 == 0 ) )
    }
  }
  
  test("mapping should double values"){
    new TestSets {
      val s = add( s2, 4 )
      val d = map( s, _ * 2 )
      assert( contains(d, 4), "contains 4" )
      assert( contains(d, 8), "contains 8" )
      assert( ! contains(d, 2), "doesn't contain 2" )
      assert( forall(d, x => x == 4 || x == 8), "forall 4 or 8")    
    }
  }
  
  
}
