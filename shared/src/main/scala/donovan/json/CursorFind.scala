package donovan.json

import io.circe.{ACursor, HCursor, Json}

import scala.annotation.tailrec

private[json] object CursorFind {

  final def apply(initial : HCursor)(p: Json => Boolean): ACursor = {
    @tailrec
    def go(c: ACursor): ACursor = c match {
      case success: HCursor => if (p(success.value)) success else go(success.right)
      case other            => other
    }

    go(initial)
  }


}
