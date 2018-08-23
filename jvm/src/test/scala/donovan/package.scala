import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import io.circe.Json
package object donovan {

  def hoconAsJson(jsonText: String): Json = {
    val jsonString = ConfigFactory
      .parseString(jsonText)
      .root
      .render(ConfigRenderOptions.concise().setJson(true))
    _root_.io.circe.parser.parse(jsonString).right.get
  }
}
