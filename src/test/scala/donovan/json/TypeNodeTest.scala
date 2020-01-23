package donovan.json

import donovan.BaseJsonSpec
import donovan.json.JType._
import io.circe.Json

class TypeNodeTest extends BaseJsonSpec {

  "TypeNode" should {
    List(
      TypeNodeObject(Map("foo" -> TypeNodeValue(JType.BooleanType))),
      TypeNodeValue(JType.NumericType),
      TypeNodeArray(Vector(TypeNodeArray(Vector()))),
      TypeNodeArray(Vector(TypeNodeArray(Vector(TypeNodeValue(JType.NumericType))))),
    ).foreach { expected: TypeNode =>
        import io.circe.syntax._
        val json: Json = expected.asJson
      s"serialize ${expected} to/from $json" in {
        json.as[TypeNode] shouldBe Right(expected)
      }
    }
  }
  "TypeNode.anonymize" should {
    "merge nested object arrays" in {
      val obj = json"""{
                          "root" : [
                            {
                              "a" : "one",
                              "b" : "two",
                              "c" : 3
                            },
                            {
                              "d" : false
                            }
                          ]
                        }"""

      import donovan.implicits._

      obj.schema.anonymize() shouldBe json"""{
                          "root" : [
                            {
                              "a" : "text",
                              "b" : "text",
                              "c" : 123
                            },
                            {
                              "d" : true
                            }
                          ]
                        }"""

      obj.anonymize shouldBe obj.anonymize.anonymize
    }
    "dedupe common elements" in {
      val sourceJson =
        hocon"""{
           base : {
              list : [1,2,3]
              doubleList : [[1,2], [3,4]]
              hybridList : [[1,2], [[[false]]], { "foo" : "bar" }]
              listOfObj : [
                { hi : false },
                { there : this },
                { there : field },
                { there : is },
                { there : redundant },
                { there : and },
                { there : so },
                { there : will },
                { there : only },
                { there : have },
                { there : one },
                { there : anonymized },
                { there : value }
              ]
            }
            number: 1
            empty: null
          }"""
      import donovan.implicits._

      sourceJson.anonymize shouldBe json"""{
          "number" : 123,
          "empty" : null,
          "base" : {
              "listOfObj" : [
                  {
                      "hi" : true
                  },
                  {
                      "there" : "text"
                  }
              ],
              "list" : [
                  123
              ],
              "hybridList" : [
                  [123],
                  [[[true]]],
                  {
                      "foo" : "text"
                  }
              ],
              "doubleList" : [
                  [123]
              ]
          }
      }"""
    }
  }
  "JPaths.apply" should {
    "return deeply nested arrays" in {
      val tn: TypeNode = TypeNode(
        json"""{
           "deep" : [[[[1]]]]
            }""")
      tn shouldBe TypeNodeObject(Map("deep" -> TypeNodeArray(Vector(TypeNodeArray(Vector(TypeNodeArray(Vector(TypeNodeArray(Vector(TypeNodeValue(JType.NumericType)))))))))))
    }
    "return deeply nested hybrids" in {
      val tn: TypeNode = TypeNode(
        json"""{
           "deep" : [[[[1], true]], "two"]
            }""")
      tn.flatten should contain only(
        "deep.[]:TextType",
        "deep.[].[].[]:BooleanType",
        "deep.[].[].[].[]:NumericType"
      )

      tn.pretty shouldBe
        """deep.[].[].[].[]:NumericType
          |deep.[].[].[]:BooleanType
          |deep.[]:TextType""".stripMargin
    }
    "return empty paths for null or scalars" in {
      TypeNode(Json.Null) shouldBe TypeNode(NullType)
      TypeNode(Json.fromBoolean(true)) shouldBe TypeNode(BooleanType)
      TypeNode(Json.fromInt(3)) shouldBe TypeNode(NumericType)
      TypeNode(Json.fromString("hi")) shouldBe TypeNode(TextType)
      TypeNode(json"""[]""") shouldBe TypeNodeArray(Vector.empty)
    }

    "return all the json paths for a given json object" in {
      val paths = TypeNode(
        json"""{
              "base" : {
                "nestedBoolean" : true,
                "nestedArray" : [1,2,3],
                "objArray" : [
                  {
                    "foo" : "bar",
                     "deepNestedArray" : [
                        {
                          "mysterious" : "buzz",
                          "buzz" : 12
                        },
                        {
                          "mysterious" : true,
                          "meh" : 1
                        },
                        {
                          "mysterious" : [1,2,3]
                        },
                        {
                          "mysterious" : [{ "nowItsAnObj" : true }]
                        },
                        {
                          "mysterious" : 19
                        },
                        3,
                        true
                     ]
                  },
                  {
                    "second" : "obj"
                  }
                ]
              },
              "ary" : [],
              "deep" : [[[[]]]],
              "deep2" : [[[]],[]],
              "dbl" : 12.34
              }""")

      val actual: Vector[String] = paths.flatten.sorted

      actual should contain inOrderOnly(
        "ary.[]",
        "base.nestedArray.[]:NumericType",
        "base.nestedBoolean:BooleanType",
        "base.objArray.[].deepNestedArray.[].buzz:NumericType",
        "base.objArray.[].deepNestedArray.[].meh:NumericType",
        "base.objArray.[].deepNestedArray.[].mysterious.[].nowItsAnObj:BooleanType",
        "base.objArray.[].deepNestedArray.[].mysterious.[]:NumericType",
        "base.objArray.[].deepNestedArray.[].mysterious:BooleanType",
        "base.objArray.[].deepNestedArray.[].mysterious:NumericType",
        "base.objArray.[].deepNestedArray.[].mysterious:TextType",
        "base.objArray.[].deepNestedArray.[]:BooleanType",
        "base.objArray.[].deepNestedArray.[]:NumericType",
        "base.objArray.[].foo:TextType",
        "base.objArray.[].second:TextType",
        "dbl:NumericType",
        "deep.[].[].[].[]",
        "deep2.[].[]",
        "deep2.[].[].[]"
      )
    }
  }
}
