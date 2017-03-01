/*
 * Copyright 2017 HM Revenue & Customs
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

package connectors.fileUpload

import connectors.{EvidenceConnector, WireMockSpec}
import play.api.libs.ws.ahc.AhcWSClient
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import uk.gov.hmrc.play.http.HeaderCarrier
import com.github.tomakehurst.wiremock.client.WireMock._
import config.ApplicationConfig
import uk.gov.hmrc.play.filters.MicroserviceFilterSupport

class EvidenceConnectorSpec extends WireMockSpec with MicroserviceFilterSupport {

  "Evidence connector" should {
    "be able to upload a file" in {
      val connector = new EvidenceConnector(AhcWSClient()) {
        override val url = mockServerUrl
      }

      implicit val fakeHc = HeaderCarrier()
      val file = getClass.getResource("/document.pdf").getFile

      stubFor(put(urlEqualTo("/customer-management-api/customer/evidence"))
        .withHeader("Ocp-Apim-Subscription-Key", matching(ApplicationConfig.apiConfigSubscriptionKeyHeader))
        .withHeader("Ocp-Apim-Trace", matching(ApplicationConfig.apiConfigTraceHeader))
        .withRequestBody(containing(file))
        .willReturn(aResponse().withStatus(200)))

      noException should be thrownBy await(connector.uploadFile("SubmissionId-ExternalId-FileName", Some(file.getBytes)))
    }
  }
}
