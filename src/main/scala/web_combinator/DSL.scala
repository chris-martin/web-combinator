package web_combinator

import scodec.bits.ByteVector

import scalaz._, scalaz.Scalaz._

import scala.language.higherKinds

import org.http4s._

trait DSL {

  def request(
    method: Method = Method.GET,
    uri: Uri = Uri(path = "/"),
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty): RequestWithoutBody =
  RequestWithoutBody(
    method=method,
    uri=uri,
    httpVersion=httpVersion,
    headers=headers)

  def response(
    status: Status = Status.Ok,
    httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
    headers: Headers = Headers.empty): ResponseWithoutBody =
  ResponseWithoutBody(
    status=status,
    httpVersion=httpVersion,
    headers=headers)

  implicit class _RequestHead(x: RequestWithoutBody) {

    def withBody[F[_]](body: RequestBody[F]): Request[F] =
      RequestWithBody[F](x, body)

    def withBody(body: ByteVector): Request[Id] =
      RequestWithBody[Id](x, body)
  }

  implicit class _ResponseHead(x: ResponseWithoutBody) {

    def withBody[F[_]](body: ResponseBody[F]): Response[F] =
      ResponseWithBody[F](x, body)

    def withBody(body: ByteVector): Response[Id] =
      ResponseWithBody[Id](x, body)
  }
}

object DSL extends DSL
