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

package utils

import play.api.libs.json.Writes
import uk.gov.hmrc.play.http._

import scala.concurrent.Future

object StubHttp extends HttpGet with HttpPost with HttpPut {
  private var stubbedGets: Seq[(String, HttpResponse)] = Nil

  def reset() = {
    stubbedGets = Nil
  }

  def stubGet(url: String, response: HttpResponse) = {
    stubbedGets :+= (url, response)
  }

  override protected def doGet(url: String)(implicit hc: HeaderCarrier) = stubbedGets.find(_._1 == url) match {
    case Some((u, r)) => Future.successful(r)
    case None => throw new NotFoundException(s"URL $url not stubbed\nStubbed GETs: $stubbedGets")
  }

  override protected def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit wts: Writes[A], hc: HeaderCarrier) = ???

  override protected def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier) = ???

  override protected def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier) = ???

  override protected def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier) = ???

  override val hooks = NoneRequired

  override protected def doPut[A](url: String, body: A)(implicit rds: Writes[A], hc: HeaderCarrier) = ???
}
