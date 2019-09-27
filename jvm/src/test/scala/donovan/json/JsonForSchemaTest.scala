package donovan.json

import donovan.BaseJsonSpec

class JsonForSchemaTest extends BaseJsonSpec {

  "JsonForSchema.apply" should {
    "create some json based on a schema" in {
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
      val schema = sourceJson.typesByPath

      JsonForSchema(schema) shouldBe json"""{
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

}
