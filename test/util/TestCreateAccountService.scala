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

import java.time.{Clock, Instant, ZoneId}

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import connectors.{AddressConnector, GroupAccountConnector, IndividualAccountConnector}
import infrastructure.VOABackendWSHttp
import models.{IndividualAccountSubmission, IndividualDetails}
import org.mockito.ArgumentMatchers.{eq => matches}
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, MustMatchers}
import play.api.libs.json.Json
import uk.gov.hmrc.domain.{AgentCode, Nino}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.config.inject.ServicesConfig
import uk.gov.hmrc.play.test.WithFakeApplication

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object LOG {
  def apply(message:String): Unit = {
    println(s"\n\n$message\n\n")
  }
}

object TestCreateAccountService extends MockitoSugar {
  class DisabledMetrics extends Metrics {
    override def defaultRegistry: MetricRegistry = new MetricRegistry()
    override def toJson: String = "{}"
  }

  case class KnownFact(key:String, value:String)

  object KnownFact {
    implicit val format = Json.format[KnownFact]
  }

  case class AuthEnrolment(key: String,
                       identifiers: Seq[KnownFact] = Seq(),
                       state: String = Enrolment.ACTIVATED/*,
                       confidenceLevel: ConfidenceLevel = ConfidenceLevel.L0*/) {
  }

  object AuthEnrolment {
    implicit val format = Json.format[AuthEnrolment]
  }

  case class Enrolment(key:String, identifier:String, state:String)

  object Enrolment {
    implicit val format = Json.format[Enrolment]

    val ACTIVATED = "Activated"
    val NOT_YET_ACTIVATED = "NotYetActivated"
  }

  case class GovernmentGatewayLogin(
                                     credId: String,
                                     enrolments: List[AuthEnrolment],
                                     agentCode: Option[AgentCode] = None,
                                     nino: Option[Nino] = None,
                                     affinityGroup: String, // TODO - validate and/or create enum and create org/agent enrolments based on this info
                                     // credentialRole: Option[CredentialRole],
                                     sso: Boolean = false,
                                     twoFactorAuthEnabled: Boolean = false,
                                     userDetailsLink: Option[String] = None,
                                     trustId: Option[String] = None,
                                     // gatewayInformation: Option[GatewayInformation] = None,
                                     // mdtpInformation: Option[MdtpInformation] = None,
                                     groupIdentifier: Option[String] = None,
                                     name: Option[String] = None,
                                     email: Option[String] = None,
                                     agentId: Option[String] = None,
                                     agentFriendlyName: Option[String] = None,
                                     description: Option[String] = None,
                                     unreadMessageCount: Option[Int] = None
                                   )

  object GovernmentGatewayLogin {
    implicit val format = Json.format[GovernmentGatewayLogin]
  }

  case class GovernmentGatewayUser(
    name:String, username:String, password:String, role:String, affinityGroup:String,
    credentialIdentifier:String, groupIdentifier:String, enrolments:List[Enrolment],
    allEnrolments:List[Enrolment], knownFacts:List[KnownFact])

  object GovernmentGatewayUser {
    implicit val format = Json.format[GovernmentGatewayUser]
  }

  case class Session(externalId:String)

  object Session {
    implicit val format = Json.format[Session]
  }

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private implicit val clock:java.time.Clock = Clock.fixed(Instant.now, ZoneId.systemDefault)

  val ggStubUrl = "http://localhost:8082"
  val ggProxyUrl = "http://localhost:9907"
  val authUrl = "http://localhost:8500"

  val http = new VOABackendWSHttp(new DisabledMetrics())
  val config = mock[ServicesConfig]
  when(config.baseUrl(matches("external-business-rates-data-platform"))).thenReturn("http://localhost:9536")
  val addresses = new AddressConnector(http, config)
  val individuals = new IndividualAccountConnector(addresses, http)
  val groups = new GroupAccountConnector(http, config)

  def createUser(
    username:String, password:String, groupId:String, credential:String,
    firstName:String="Test", lastName:String, email:String="test.user@mail.com") = {

    val affinityGroup = "Organisation"
    def groupNotFound = throw new Exception(s"Group '$groupId' could not be found")
    groups.findByGGID(groupId).flatMap(_.fold(groupNotFound) { group =>
      val account = IndividualAccountSubmission(externalId=credential, trustId="NONIV", organisationId=group.id,
        IndividualDetails(firstName=firstName, lastName=lastName, email=email, phone1=group.phone, phone2=None, addressId=group.addressId))
      for {
        accountId <- individuals.create(account).map(_.id)
        enrolments = List(Enrolment(key="HMRC-VOA-CCA", identifier=accountId.toString, state=Enrolment.ACTIVATED))
        facts = List(KnownFact(key="VOAPersonID", value=accountId.toString), KnownFact(key="BusPostcode",value="ABC 123"))
        user = GovernmentGatewayUser(
          name=s"$firstName $lastName", username=username,password=password, role="User",
          affinityGroup=affinityGroup, credentialIdentifier=credential,knownFacts=facts,
          groupIdentifier=groupId, enrolments=enrolments, allEnrolments=enrolments)
        login = GovernmentGatewayLogin(credId = credential, affinityGroup=affinityGroup,
          enrolments=enrolments.map(x => AuthEnrolment(key=x.key,identifiers = facts)))
        _ <- http.POST[GovernmentGatewayUser, HttpResponse](s"$ggStubUrl/test-only/users", user, Seq("Content-Type"->"application/json"))
        auth <- http.POST[GovernmentGatewayLogin, HttpResponse](s"$authUrl/auth/sessions", login, Seq("Content-Type"->"application/json"))
        sessionLink = auth.header("Location").get
        session <- http.GET[Session](s"$authUrl${sessionLink}")
        _ <- individuals.update(accountId,account.copy(externalId=session.externalId))
      } yield (auth.status,auth.body,auth.allHeaders,session)
    }) map { auth => LOG(s"AUTH: $auth")}
  }
}


class TestCreateAccountServiceSpec extends FlatSpec with MustMatchers with MockitoSugar with ScalaFutures with WithFakeApplication {
  val service = TestCreateAccountService

  it should "Create user in Data Platform and GG stubs" in {
    val id = 5
    val result = service.createUser(username=s"user$id", password=s"pass$id", lastName=s"User $id",groupId="stub-group-3", credential=s"cred$id")
    Await.result(result, 10 seconds)
  }

  "GG user service" should "create user under a GG organisation" in {
  }

  "created user" should "not be asked to register when logging in" in {
  }

  "created user" should "be recognised as belonging to the existing organisation" in {
  }
}