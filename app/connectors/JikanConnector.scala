package connectors

import cats.data.EitherT
import models.APIError
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

class JikanConnector @Inject()(ws: WSClient) {
  def get[Response](url: String)(implicit rds: OFormat[Response], ec: ExecutionContext): EitherT[Future, APIError, Response] = {
    val request: WSRequest = ws.url(url)
    val response = request.get()

    // EitherT allows us to return either Future[APIError] or Future[Response]
    EitherT {
      response.map {
          result => {
            val resultJson: JsValue = Json.parse(result.body)
//            println(resultJson)
            resultJson.validate[Response] match {
              case JsSuccess(responseItem, _) => Right(responseItem)
              case JsError(_) =>
                result.status match {
                  case 404 => {
                    val message: Option[String] = (resultJson \ "message").asOpt[String]
                    Left(APIError.BadAPIResponse(404, message.getOrElse("Unknown error")))
                  }
                  case 400 => {
                    val messages: Option[String] = (resultJson \ "messages").asOpt[Map[String, Seq[String]]].map {
                      messageMap => messageMap.values.flatten.mkString(" ")
                    }
                    Left(APIError.BadAPIResponse(400, messages.getOrElse("Unknown error")))
                  }
                  case _ => Left(APIError.BadAPIResponse(result.status, "Unknown error"))
                }
            }
          }
        }
        .recover { //case _: WSResponse =>
          case _ => Left(APIError.BadAPIResponse(500, "Could not connect"))
        }
    }
  }
}

//{"status":400,"type":"ValidationException","messages":{"min_score":["The min score must be a number."]},
// "error":"Invalid or incomplete request. Make sure your request is correct. https:\/\/docs.api.jikan.moe\/"}

//{"status":404,"type":"HttpException","message":"Not Found","error":null}