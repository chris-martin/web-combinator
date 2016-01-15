import scala.language.higherKinds
import org.http4s.{Status, Response, Request}
import scalaz._, Scalaz._

package object web_combinator {

  sealed trait Webapp[+F[_]]

  case object emptyWebapp extends Webapp[Id]

  def getResponse[F[_] : Applicative](webapp: Webapp[F], request: Request): F[Response] =
    webapp match {
      case emptyWebapp => Response(status=Status.NotFound).point[F]
    }

}
