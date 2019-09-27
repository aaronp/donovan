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
              hybridList : [[1,2], [true], { "foo" : "bar" }]
              listOfObj : [
                { hi : true },
                { there : 1 }
              ]
            }
            number: 1
            empty: null
          }"""
      import donovan.implicits._
      val schema = sourceJson.typesByPath
      val back = JsonForSchema(schema)
      println(back.spaces4)
    }
  }

}
