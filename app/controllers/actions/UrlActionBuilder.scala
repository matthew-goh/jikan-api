package controllers.actions

import controllers.actions.ModifiedRequests._
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

trait UrlActionBuilder
  extends ActionBuilder[RequestWithUrl, AnyContent]
    with ActionFunction[Request, RequestWithUrl]

class UrlActionBuilderImpl @Inject()(val parser: BodyParsers.Default)(implicit val executionContext: ExecutionContext)
  extends UrlActionBuilder {

  override def invokeBlock[A](request: Request[A], block: RequestWithUrl[A] => Future[Result]): Future[Result] = {
    val reqBody: Option[Map[String, Seq[String]]] = request.body.asInstanceOf[AnyContent].asFormUrlEncoded
    val sourceUrl: Option[String] = reqBody.flatMap(_.get("url").flatMap(_.headOption))
    sourceUrl match {
      case None | Some("") => Future.successful(Results.BadRequest(views.html.unsuccessful("Failed to post source url")))
      case Some(url) => block(RequestWithUrl(url, request)) // Pass the custom request to the block
    }
  }
}
