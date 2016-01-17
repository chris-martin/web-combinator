package web_combinator

import scalaz._, scalaz.Scalaz._

import scala.language.{implicitConversions, higherKinds}

import org.http4s._

trait DSL {

  def request(
    method: Method = Method.GET,
    uri: Uri = Uri(path = "/"),
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty): RequestPrelude =
  RequestPrelude(
    method=method,
    uri=uri,
    httpVersion=httpVersion,
    headers=headers)

  def response(
    status: Status = Status.Ok,
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty): ResponsePrelude =
  ResponsePrelude(
    status=status,
    httpVersion=httpVersion,
    headers=headers)

  implicit def withEmptyBody[A, F[_] : Applicative](a: A): MessageF[A, F] =
    MessageF[A, F](a, emptyBody.point[F])

  implicit class ThingEnrichment[A](a: A) {

    def withBody[F[_]](body: BodyF[F]): MessageF[A, F] =
      MessageF[A, F](a, body)

    def withBody(body: Body1): MessageF[A, Id] =
      MessageF[A, Id](a, body.point[Id])
  }
}

object DSL extends DSL
