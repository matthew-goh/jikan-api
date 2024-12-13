package baseSpec

import play.api.mvc.AnyContentAsEmpty
import play.api.test.CSRFTokenHelper.CSRFFRequestHeader
import play.api.test.FakeRequest
import play.api.test.Helpers.{DELETE, GET, POST, PUT}
import play.api.i18n.{Messages, MessagesApi}

class TestRequest(messagesApi: MessagesApi) {

  lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("", "").withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  implicit val messages: Messages = messagesApi.preferred(fakeRequest)

  def buildPost(url: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(POST, url).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  def buildGet(url: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(GET, url).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  def buildPut(url: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(PUT, url).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  def buildDelete(url: String): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(DELETE, url).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

}
