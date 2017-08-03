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

package helpers

import infrastructure.SimpleWSHttp
import org.scalatest.Suite
import play.api.{Application, Mode}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.play.http.ws.WSHttp
import uk.gov.hmrc.play.test.WithFakeApplication

trait WithSimpleWsHttpTestApplication extends WithFakeApplication {

  this: Suite =>

  override lazy val fakeApplication: Application = new GuiceApplicationBuilder()
    .in(Mode.Test)
    .overrides(bind[WSHttp].to[SimpleWSHttp])
    .configure("metrics.enabled" -> "false")
    .disable(classOf[play.modules.reactivemongo.ReactiveMongoHmrcModule])
    .disable(classOf[modules.FileTransferScheduler])
    .disable(classOf[modules.MongoStartup])
    .build()
}
