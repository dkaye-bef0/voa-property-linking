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

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import connectors.{AddressConnector, IndividualAccountConnector}
import infrastructure.VOABackendWSHttp
import models.{IndividualAccountSubmission, IndividualDetails}
import org.mockito.ArgumentMatchers.{eq => matches, _}
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse}
import uk.gov.hmrc.play.config.inject.ServicesConfig
import uk.gov.hmrc.play.test.WithFakeApplication

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.xml.XML

object LOG {
  def apply(message:String): Unit = {
    println(s"\n\n$message\n\n")
  }
}

class DisabledMetrics extends Metrics {
  override def defaultRegistry: MetricRegistry = new MetricRegistry()
  override def toJson: String = "{}"
}

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

case class KnownFact(key:String,value:String)

object KnownFact {
  implicit val format = Json.format[KnownFact]
}

case class Enrolment(key: String, identifier: String, state: String)

object Enrolment {
  implicit val format = Json.format[Enrolment]

  val ACTIVATED = "Activated"
  val NOT_YET_ACTIVATED = "NotYetActivated"
}

case class GovernmentGatewayUser(
  name:String,username:String,password:String,role:String,affinityGroup:String,
  credentialIdentifier:String,groupIdentifier:String,enrolments:List[Enrolment],
  allEnrolments:List[Enrolment],knownFacts:List[KnownFact])

object GovernmentGatewayUser {
  implicit val format = Json.format[GovernmentGatewayUser]
}

object TestCreateAccountService extends MockitoSugar {
  // val http = WSHttp
  val ggStubUrl = "http://localhost:8082"
  val ggProxyUrl = "http://localhost:9907"
  val http = new VOABackendWSHttp(new DisabledMetrics())
  val config = mock[ServicesConfig]
  when(config.baseUrl(matches("external-business-rates-data-platform"))).thenReturn("http://localhost:9536")
  val addresses = new AddressConnector(http,config)
  val individuals = new IndividualAccountConnector(addresses,http)
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
        xml, Seq("Content-Type" -> "application/xml")
      )(implicitly[HttpReads[HttpResponse]], hc, implicitly[ExecutionContext])
    )
  }

  private def handleRegisterUserResponse(httpResponse: Future[HttpResponse])(implicit ec: ExecutionContext): Future[String] = {
    httpResponse.map { response =>
      val responseBodyAsXML = XML.loadString(response.body)
      (responseBodyAsXML \ "UserID").text
    }
  }

  def createUser(
    username:String="user1", firstName:String="Test", lastName:String="User", email:String="test.user@mail.com",
    externalId:String="EXT-1", trustId:String="TRUST-1", organisationId:Int=1000000000,addressId:Long=1000000005) = {
    val account = IndividualAccountSubmission(externalId=externalId,trustId=trustId,organisationId=organisationId,
      IndividualDetails(firstName=firstName, lastName=lastName, email=email, phone1="0123456789", phone2=None, addressId=addressId))
    // val json = Json.toJson(account.toAPIIndividualAccount)
    // LOG(s"JSON: ${Json.prettyPrint(json)}")
    individuals.create(account)
      .map { accountId => LOG(s"accountId: $accountId"); accountId.id }
      .map { accountId =>
        val enrolment = Enrolment(key="HMRC-VOA-CCA",identifier=accountId.toString,state=Enrolment.ACTIVATED)
        GovernmentGatewayUser(
          name=s"$firstName $lastName",username=username,password="password1",
          role="User",affinityGroup="Organisation",credentialIdentifier=accountId.toString,
          groupIdentifier="stub-group-3",enrolments=List(enrolment),allEnrolments=List(enrolment),
          knownFacts=List(KnownFact(key="VOAPersonID",value=accountId.toString),KnownFact(key="BusPostcode",value="ABC 123"))) }
        .flatMap { user =>
          println(s"user: ${Json.toJson(user)}")
          http.POST[GovernmentGatewayUser,HttpResponse](s"$ggStubUrl/test-only/users",user,Seq("Content-Type"->"application/json")) }
        .map(x => println(s"result: $x"))
  }
}

/*
personId: 398303440
organisationId: 171684128
personal address: 1197007448
business address: 1197007448

ownerId: 426291880
organisationId: 1000000005
personal address: -1630451358
business address: 1000000000

stub-group-3
personId: 515833925
organisationId: 5000000003
personal address: 1197007448
business address: 1000000000
 */

class TestCreateAccountServiceSpec extends FlatSpec with MustMatchers with MockitoSugar with ScalaFutures with WithFakeApplication {
  implicit private lazy val hc: HeaderCarrier = HeaderCarrier()
  val service = TestCreateAccountService
  val organisationId = "123"
  val user = "new user"

  val http = new VOABackendWSHttp(new DisabledMetrics())
  val config = mock[ServicesConfig]
  when(config.baseUrl(matches("external-business-rates-data-platform"))).thenReturn("http://localhost:9536")
  val addresses = new AddressConnector(http,config)
  val individuals = new IndividualAccountConnector(addresses,http)

  it should "Create user in BRDPS and GG stubs" in {
    val result = service.createUser()
    Await.result(result,10 seconds)
  }

  "GG user service" should "create user under a GG organisation" in {
    // Await.result(TestCreateAccountService.createOrganisationUser("username1","password1","org1"),10 seconds)
  }

  "created user" should "not be asked to register when logging in" in {
  }

  "created user" should "be recognised as belonging to the existing organisation" in {
  }
}