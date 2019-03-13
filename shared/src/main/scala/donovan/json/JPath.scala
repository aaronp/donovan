package donovan.json

import donovan.json.JPath.{JsonFormat, select}
import io.circe.Decoder.Result
import io.circe._

final case class JPath(path: List[JPart]) {

  import io.circe._
  import io.circe.syntax._

  def ++(other: JPath): JPath = copy(path = path ++ other.path)

  def :++[T](other: JPath): JPath = copy(path = path ++ other.path)
  def ++:[T](other: JPath): JPath = copy(path = other.path ++ path)

  def +:[T](other: T)(implicit ev: T <:< JPart): JPath = copy(path = ev(other) +: path)

  def :+(other: JPart): JPath = copy(path = path :+ other)

  /**
    * @param condition the condition to apply to the last field
    * @return a new JPath with the given condition applied to the last element of this JPath, if there is a last element
    */
  def :+(condition: JPredicate) = withCondition(condition)

  def withCondition(condition: JPredicate) = JPath(path :+ JFilter(condition))

  def json: Json = {
    new EncoderOps(this).asJson(JsonFormat)
  }

  def apply(json: Json): Option[Json] = selectValue(json)

  /** e.g. given the JPath ("x" :: "y" :: "z")
    *
    * and the json input
    * {{{
    *   x : {
    *     y : {
    *       z : 1
    *       flag : true
    *     }
    *   }
    * }}}
    *
    * this function would return JsonInt(1)
    *
    * @param json the json to select
    * @return the json value identified by the given path, if it exists
    */
  def selectValue(json: Json): Option[Json] = JPath.select(path, json.hcursor).focus

  /**  e.g. given the JPath ("x" :: "y" :: "z")
    *
    * and the json input
    * {{{
    *   x : {
    *     y : {
    *       z : 1
    *       flag : true
    *     }
    *   }
    * }}}
    *
    * this function would return the json object:
    *
    *      {{{
    *        x : {
    *          y : {
    *            z : 1
    *         }
    *        }
    *      }}}
    *
    * @param json the just to select
    * @return the full json path containing the json value, if it exists
    */
  def selectJson(json: Json): Option[Json] = {
    selectValue(json).map { value =>
      JPath.selectJson(path, value)
    }
  }

  /** Adds the given value T to the target json 'json' at this path.
    *
    * e.g. consider the json
    *
    * {{{
    * { foo : [1,2] }
    * }}}
    *
    * Then calling
    * {{{
    *   JPath("foo").appendTo(json, 3)
    *   // returns { foo : [1,2,3] }
    *
    *   JPath("bar", "baz").appendTo(json, { doc : { ument : true } })
    *   // returns :
    *   {
    *     foo : [1,2],
    *     bar : {
    *       baz : {
    *         doc : {
    *           ument : true
    *         }
    *       }
    *     }
    *   }
    * }}}
    *
    * @param json
    * @param value
    * @tparam T
    * @return
    */
  def appendTo[T: Encoder](json: Json, value: T): Option[Json] = {
    val opt = JPath.select(path, json.hcursor).withFocus { json =>
      deepMergeWithArrayConcat(json, implicitly[Encoder[T]].apply(value))
    }
    opt.top
  }

  /**
    * @param json the input json from which the json should be removed
    * @return a new json instance with the given path removed, if the path exists in the input json
    */
  def removeFrom(json: Json): Option[Json] = select(path, json.hcursor).delete.top

  def matches(json: Json, filter: JPredicate = JPredicate.matchAll): Boolean = {
    asMatcher(filter).matches(json)
  }

  def asMatcher(filter: JPredicate = JPredicate.matchAll): JPredicate = JPredicate(this, filter)

  def and(other: JPredicate, theRest: JPredicate*) = asMatcher().and(other, theRest: _*)

  def or(other: JPredicate, theRest: JPredicate*) = asMatcher().or(other, theRest: _*)

  def !(other: JPredicate): JPath = withCondition(Not(other))

  def =!=(value: Json): JPath = {
    withCondition(Not(Eq(value)))
  }

  def !==(value: Json): JPath = {
    =!=(value)
  }

  def ===(value: Json): JPath = equalTo(value)
  def ===(value: JPath)       = equalTo(value)

  def equalTo(value: Json): JPath = withCondition(Eq(value))
  def equalTo(value: JPath)       = ComparePredicate(this, value, Op.Equals)

  def isBefore(time: String): JPath = withCondition(Before(time))
  def isBefore(value: JPath)        = ComparePredicate(this, value, Op.Before)

  def isAfter(time: String): JPath = withCondition(After(time))
  def isAfter(value: JPath)        = ComparePredicate(this, value, Op.After)

  def gt(value: Json): JPath = withCondition(Gt(value))
  def gt(value: JPath)       = ComparePredicate(this, value, Op.GT)

  def lt(value: Json): JPath = withCondition(Lt(value))
  def lt(value: JPath)       = ComparePredicate(this, value, Op.LT)

  def gte[J](value: J)(implicit ev: J => Json): JPath = withCondition(Gte(ev(value)))
  def gte(value: JPath)                               = ComparePredicate(this, value, Op.GTE)

  def lte(value: Json): JPath       = withCondition(Lte(value))
  def lte(value: JPath): JPredicate = ComparePredicate(this, value, Op.LTE)

  def ~=(regex: String): JPath = withCondition(JRegex(regex))

  def includes[J](items: Set[J])(implicit ev: J => Json): JPath = withCondition(JIncludes(items.map(ev)))

  def includes[J](first: J, theRest: J*)(implicit ev: J => Json): JPath = includes(theRest.toSet + first)

}

object JPath {

  val root = JPath(Nil)

  import JPredicate.implicits._

  def apply(first: JPart, parts: JPart*): JPath = JPath(first :: parts.toList)

  def apply(only: String): JPath = {
    val segments = only.split("\\.", -1).map(_.trim).filterNot(_.isEmpty).toList
    forParts(segments)
  }

  def apply(first: String, second: String, parts: String*): JPath = {
    forParts(first :: second :: parts.toList)
  }

  implicit object JsonFormat extends Encoder[JPath] with Decoder[JPath] {
    override def apply(a: JPath): Json = {
      import io.circe.syntax._
      a.path.asJson
    }

    override def apply(c: HCursor): Result[JPath] = {
      val parts: Result[List[JPart]] = c.as[List[JPart]]
      parts.right.map(JPath.apply)
    }
  }

  def forTypesByPath(typeByPath: TypeByPath): JPath = forParts(typeByPath._1)

  def forParts(first: String, theRest: String*): JPath = forParts(first :: theRest.toList)

  def forParts(parts: List[String]): JPath =
    JPath(parts.flatMap(parseSegment))

  private def parseSegment(segment: String): List[JPart] = {
    segment match {
      case IntR(i)           => JPos(i.toInt).asPath.path
      case ValueR(f, v)      => (f.asJPath === Json.fromString(v)).path
      case ArrayR(name, "*") => parseSegment(name) :+ JArrayFind(JPredicate.matchAll)
      case ArrayR(name, num) => parseSegment(name) :+ JPos(num.toInt)
      case name              => JField(name).asPath.path
    }
  }

  def fromJson(jsonString: String): JPath = {

    import io.circe.parser._
    decode[JPath](jsonString) match {
      case Left(err)    => throw err
      case Right(jpath) => jpath
    }
  }

  private implicit class RichCursor(val a: ACursor) extends AnyVal {
    def withHCursor(f: HCursor => ACursor): ACursor = a.success.fold(a)(f)
  }

  private[json] def select(parts: List[JPart], cursor: HCursor): ACursor = {
    parts match {
      case Nil                   => cursor
      case JField(field) :: tail => cursor.downField(field).withHCursor(select(tail, _))
      case JPos(pos) :: tail =>
        cursor.downArray.withHCursor { ac =>
          ac.rightN(pos).withHCursor(select(tail, _))
        }
      case JArrayFind(predicate) :: tail =>
        cursor.downArray.withHCursor { c =>
          val found = c.find(predicate.matches)
          found.withHCursor(select(tail, _))
        }
      case JFilter(predicate) :: tail =>
        if (cursor.focus.exists(predicate.matches)) {
          select(tail, cursor)
        } else {
          new FailedCursor(cursor, CursorOp.Find(predicate.matches))
        }
    }
  }

  // used by selectJson to extract JParts which represent objects
  private object ObjectPart {
    def unapply(part: JPart): Option[String] = {
      part match {
        case JField(field) => Option(field)
//        case JFilter(field, _) => Option(field)
        case _ => None
      }
    }
  }

  // used by selectJson to extract JParts which represent arrays
  private object ArrayPart {
    def unapply(part: JPart): Boolean = {
      part match {
        case JPos(_)       => true
        case JArrayFind(_) => true
        case _             => false
      }
    }
  }

  def selectJson(parts: List[JPart], value: Json): Json = {
    parts match {
      case Nil                       => value
      case ObjectPart(field) :: tail => Json.obj(field -> selectJson(tail, value))
      case ArrayPart() :: tail       => Json.arr(selectJson(tail, value))
    }
  }

  private val ArrayR = "(.*)\\[(.+)\\]".r
  private val IntR   = "(\\d+)".r
  private val ValueR = "(.*)=(.*)".r
}
