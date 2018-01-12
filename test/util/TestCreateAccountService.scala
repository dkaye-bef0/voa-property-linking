/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package util

import config.WSHttp
import org.mockito.ArgumentMatchers.{eq => matching, _}
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse}
import uk.gov.hmrc.play.test.WithFakeApplication

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.xml.XML

case class RegistrationRequest(fullName: String, plainTextPassword: String, affinityGroup: String = "organisation", emailAddress: String, description: Option[String] = None) {
  def toXml:scala.xml.Elem =
      <urn:GsoRegisterPrincipalUserIDXmlInput xmlns:urn="urn:GSO-System-Services:external:1.65:GsoRegisterPrincipalUserIDXmlInput">
        <urn:PortalIdentifier>
          HMRC_SMS
        </urn:PortalIdentifier>
        <urn:CredentialName>
          {fullName}
        </urn:CredentialName>
        <urn:EmailAddress>
          {emailAddress}
        </urn:EmailAddress>
        {description.fold(<urn:Description/>)(desc =>
          <urn:Description>{desc}</urn:Description>)}
        <urn:RegistrationCategory>
          {affinityGroup}
        </urn:RegistrationCategory>
        <urn:Password>
          {plainTextPassword}
        </urn:Password>
      </urn:GsoRegisterPrincipalUserIDXmlInput>
}

object TestCreateAccountService {
  val http = WSHttp
  val ggProxyUrl = "http://localhost:9907"
  implicit private val hc: HeaderCarrier = HeaderCarrier()

  def createOrganisationUser(username:String,password:String,organisationId:String): Future[String] = {
    registerUser(RegistrationRequest(fullName="Test User",plainTextPassword=password,emailAddress=s"$username@mail.com").toXml)
  }

  def registerUser(xml: scala.xml.Elem)(implicit hc: HeaderCarrier): Future[String] = {
    callProxy("portal", "GsoRegisterPrincipalUserID", xml.toString())
  }

  private def callProxy(api: String, action: String, xml: String)(implicit hc: HeaderCarrier): Future[String] = {
    println(xml)
    handleRegisterUserResponse(
      http.POSTString(
        s"$ggProxyUrl/government-gateway-proxy/api/$api/$action",
        xml,
        Seq("Content-Type" -> "application/xml")
      )(implicitly[HttpReads[HttpResponse]], hc, implicitly[ExecutionContext])
    )
  }

  private def handleRegisterUserResponse(httpResponse: Future[HttpResponse])(implicit ec: ExecutionContext): Future[String] = {
    httpResponse.map { response =>
      val responseBodyAsXML = XML.loadString(response.body)
      (responseBodyAsXML \ "UserID").text
    }
  }

  def createUserInBrdps(): Unit = {

  }
}

class TestCreateAccountServiceSpec extends FlatSpec with MustMatchers with ScalaFutures with WithFakeApplication {
  val organisationId = "123"
  val user = "new user"

  it should "Create user in BRDPS" in {
  }

  "GG user service" should "create user under a GG organisation" in {
    Await.result(TestCreateAccountService.createOrganisationUser("username1","password1","org1"),10 seconds)
  }

  "created user" should "not be asked to register when logging in" in {
  }

  "created user" should "be recognised as belonging to the existing organisation" in {
  }
}