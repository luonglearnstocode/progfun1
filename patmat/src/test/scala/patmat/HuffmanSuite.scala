package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    new TestTrees {
      val l = times(string2Chars("aabbcccdeeee"))
      assert(l.contains(('a', 2)))
      assert(l.contains(('c', 3)))
      assert(l.contains(('d', 1)))
      assert(l.contains(('e', 4)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of 2 leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }

  test("decode") {
    println(decodedSecret)
  }

  test("encode") {
    println(encode(frenchCode)(string2Chars("abcdef")))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val table = ('a', List(1, 2, 3)) :: ('b', List(4,5,6)) :: Nil
    println(codeBits(table)('a'))
    println(codeBits(table)('b'))
    assert(codeBits(table)('a').equals(List(1, 2, 3)))
  }

  test("mergeCodeTables") {
    val tableA = ('a', List(1, 2, 3)) :: ('b', List(4,5,6)) :: Nil
    val tableB = ('c', List()) :: ('d', List()) :: Nil
    println(mergeCodeTables(tableA, tableB))
  }

  test("quickEncode") {
    println(quickEncode(frenchCode)(string2Chars("abcdef")))
  }

}
