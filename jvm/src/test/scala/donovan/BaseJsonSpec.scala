package donovan

import _root_.io.circe.parser.parse
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

trait BaseJsonSpec extends WordSpec with Matchers with ScalaFutures with BeforeAndAfterAll {

  implicit class JsonHelper(sc: StringContext) {
    // TODO - use io.circe.literal._
    def json(args: Any*) = {
      val text = sc.s(args: _*).stripMargin
      parse(text).right.getOrElse(sys.error(s"Couldn't parse json '$text'"))
    }
  }

  implicit class HoconHelper(sc: StringContext) {
    def hocon(args: Any*) = {
      val jsonText: String = sc.s(args: _*)
      hoconAsJson(jsonText)
    }
  }

  implicit class ConfigHelper(sc: StringContext) {
    def conf(args: Any*): Config = {
      ConfigFactory.parseString(sc.s(args: _*))
    }
  }

}
