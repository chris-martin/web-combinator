package web_combinator

import org.http4s.{Method,Status,Uri}

import scodec.bits.ByteVector

import scalaz._, scalaz.Scalaz._

import org.scalatest._

import DSL._

class AppSpec extends FlatSpec with Matchers {

  val someArbitraryRequests: Seq[Request[Id]] = Seq(
    request(),
    request(method=Method.POST, uri=Uri(path="/foo"))
      .withBody(ByteVector("abc".getBytes("UTF-8"))))

  val basic404: Response[Id] = response(status=Status.NotFound)

  "The empty webapp" should "404" in {
    for (req <- someArbitraryRequests)
      getResponse[Id](emptyApp, req).should(be(basic404))
  }

  it should "have no static files" in {
    getStaticFiles(emptyApp).toSeq.should(be(empty))
  }
}
