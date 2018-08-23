package donovan.json

import donovan.{BaseJsonSpec, implicits}
import io.circe.{HCursor, Json}

import scala.language.implicitConversions

class JPathTest extends BaseJsonSpec {
  import implicits._

  "json.selectJson" should {
    "filter out just the json for the provided paths" in {

      val json =
        json"""{
              "root" : {
                 "array" : [
                 {
                   "nested" : [[
                     {
                       "obj" : 41
                     },
                     {
                       "obj" : 42
                     }
                   ]]
                 },
                 {
                   "x" : { "y" :  2}
                 },
                 true
                 ],
                 "valueOne" : 8
              }
            }"""

      val path   = JPath("root", "array") :+ 0 :+ "nested".asJField :+ 0 :+ 1
      val unique = path.selectJson(json)

      unique.get shouldBe json"""{
                                  "root" : {
                                    "array" : [
                                      {
                                        "nested" : [
                                          [
                                            {
                                              "obj" : 42
                                            }
                                          ]
                                        ]
                                      }
                                    ]
                                  }
                                }"""
    }
  }
  "json.filter(...)" should {
    "filter out json with just the provided paths" in {

      val json =
        json"""{
              "root" : {
                 "array" : [
                 {
                   "x" : { "y" :  1}
                 },
                 {
                   "x" : { "y" :  2}
                 },
                 true
                 ],
                 "valueOne" : 8,
                 "valueTwo" : 9,
                 "valueThree" : "some text",
                 "valueFour" : "more text"
              }
            }"""

      val whyIsGreaterThanOne = JPredicate("x".asJPath, ("y" gt 1)).inArray
      val path                = JPath("root", "array") :+ whyIsGreaterThanOne
      val path2               = JPath("root", "valueTwo")

      val selected = json.filter(path, path2)

      selected shouldBe json"""{
                                  "root" : {
                                    "array" : [
                                      {
                                        "x" : {
                                          "y" : 2
                                        }
                                      }
                                    ],
                                    "valueTwo" : 9
                                  }
                                }
                               """

    }
  }
  "JPath json" should {
    "marshal complex paths and from json" in {
      import io.circe.parser._
      import io.circe.syntax._

      val path        = JPath.forParts("groot", "list") :+ 3
      val complexPath = path :+ ("value".inArray) :++ ("someInt" gte 9)

      val json = complexPath.asJson.spaces4

      decode[JPath](json) shouldBe Right(complexPath)
    }
  }

  "JPath.appendTo" should {
    "append values to arrays" in {

      val json =
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3]
                 }
              }
            }"""

      val withFour = JPath("foo", "bar", "list").appendTo(json, json"4").get
      withFour shouldBe
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3,4]
                 }
              }
            }"""

      val withObj = JPath("foo", "bar", "list").appendTo(json, json""" {"hello" : "world"} """).get
      withObj shouldBe
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3, { "hello" : "world" } ]
                 }
              }
            }"""
    }
    "append fields to objects" in {
      val json =
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3]
                 }
              }
            }"""

      JPath("foo", "bar").appendTo(json, json""" {"hello" : "world"} """).get shouldBe
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3],
                   "hello" : "world"
                 }
              }
            }"""

      JPath("foo", "bar").appendTo(json, json"4").get shouldBe
        json"""{
              "foo" : {
                 "bar" : 4
              }
            }"""
    }
  }
  "JPath.remove" should {
    "remove entries from json" in {
      val json =
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3]
                 },
                 "hi" : "there"
              }
            }"""

      JPath("foo", "bar").removeFrom(json).get shouldBe
        json"""{
              "foo" : {
                 "hi" : "there"
              }
            }"""
    }
    "remove values at a fixed position in arrays" in {
      val json = json"""{ "list" : [1,2,3] }"""

      (JPath("list") :+ 0).removeFrom(json).get shouldBe json"""{ "list" : [2, 3] }"""
      (JPath("list") :+ 1).removeFrom(json).get shouldBe json"""{ "list" : [1, 3] }"""
      (JPath("list") :+ 2).removeFrom(json).get shouldBe json"""{ "list" : [1, 2] }"""
    }
    "remove entire matching arrays" in {
      val json =
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3]
                 }
              }
            }"""
      val removed = (JPath("foo", "bar") ++ ("list" includes 2)).removeFrom(json).get
      removed shouldBe
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123
                 }
              }
            }"""

      (JPath("foo", "bar") ++ ("list" includes 4)).removeFrom(json) shouldBe None

    }
    "remove values from an object array" in {
      val original = json"""{ "values" : [1,2] }"""
      val actual   = (JPath("values") :+ 2.inArray).removeFrom(original)
      actual shouldBe Some(json"""{ "values" : [1] }""")
    }
    "remove values from arrays" in {
      val json =
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,2,3]
                 }
              }
            }"""
      val removed = (JPath("foo", "bar", "list") :+ 2.inArray).removeFrom(json)

      removed.get shouldBe
        json"""{
              "foo" : {
                 "bar" : {
                   "baz" : 123,
                   "list" : [1,3]
                 }
              }
            }"""
    }
    "remove all matching values from arrays" in {
      val json = json"""["aaa", "abc", "bbb" ] """

      JPath(JRegex("a.*").inArray).removeFrom(json).get shouldBe json"""[ "abc" , "bbb" ] """
      JPath(JRegex("aa+").inArray).removeFrom(json).get shouldBe json"""[ "abc", "bbb" ] """
      JPath(JRegex("x+").inArray).removeFrom(json) shouldBe None
      JPath(JRegex("..b").inArray).removeFrom(json).get shouldBe json"""[ "aaa", "abc" ] """
    }
  }
  "JPath.select" should {
    "match JFields" in {
      val json: Json =
        json"""{ "foo" : 1 } """

      JPath.select(JField("foo") :: Nil, json.hcursor) match {
        case h: HCursor => h.value.asNumber.flatMap(_.toInt).get shouldBe 1
      }
      val a = JPath.select(JField("bar") :: Nil, json.hcursor)
      a.succeeded shouldBe false
      a.focus.toList should be(empty)
    }
    "match JPos" in {
      val json: Json = json"""[1,2,3]"""

      def intAt(n: Int) = JPath.select(JPos(n) :: Nil, json.hcursor) match {
        case h: HCursor => h.value.asNumber.flatMap(_.toInt).get
      }

      intAt(1) shouldBe 2
      intAt(0) shouldBe 1
      intAt(2) shouldBe 3

      val a = JPath.select(JPos(3) :: Nil, json.hcursor)
      a.succeeded shouldBe false
      a.focus.toList should be(empty)
    }
    "match JFilter" in {
      val json: Json = json"""{ "some-field" : 456 }"""

      import JPredicate.implicits._
      val found = JPath.select(("some-field" equalTo 456).path, json.hcursor)
      found.succeeded shouldBe true
      val found2 = JPath.select(("some-field" equalTo 789).path, json.hcursor)
      found2.succeeded shouldBe false
    }
  }

  "JPath.apply" should {

    val json =
      json"""{
              "root" : {
                 "array" : [
                 {
                   "x" : { "y" :  1}
                 },
                 {
                   "x" : { "y" :  2},
                   "value" : 42
                 },
                 true
                 ],
                 "valueOne" : 8,
                 "valueTwo" : 9,
                 "valueThree" : "some text",
                 "valueFour" : "more text"
              }
            }"""

    "select values nested in arrays" in {
      val whyIsGreaterThanOne = JPredicate("x".asJPath, ("y" gt 1)).inArray
      val path                = JPath("root", "array") :+ whyIsGreaterThanOne
      val actual              = path(json)
      actual shouldBe Some(json"""{ "x" : { "y" : 2 }, "value" : 42 }""")
    }
  }
}
