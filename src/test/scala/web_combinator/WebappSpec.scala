package web_combinator

import org.http4s.{Request,Response,Status}
import org.scalatest._

class WebappSpec extends FlatSpec with Matchers {

  "The empty webapp" should "404" in {
    getResponse(emptyWebapp, Request()).should(be(Response(status=Status.NotFound)))
  }
}
