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

package connectors

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import uk.gov.hmrc.play.it.Port
import uk.gov.hmrc.play.test.UnitSpec

trait WireMockSpec extends UnitSpec with BeforeAndAfterAll with BeforeAndAfterEach {

  private lazy val wireMockServer = new WireMockServer(Port.randomAvailable)

  protected lazy val mockServerUrl = s"http://localhost:${wireMockServer.port}"

  override def beforeAll() {
    super.beforeAll
    wireMockServer.start()
    WireMock.configureFor("localhost", wireMockServer.port)
  }

  override def beforeEach() = {
    WireMock.reset()
  }

  override def afterAll(): Unit = {
    wireMockServer.stop()
    super.afterAll()
  }
}
