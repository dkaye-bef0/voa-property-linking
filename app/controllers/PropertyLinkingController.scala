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

package controllers

import config.Wiring
import models._
import play.api.libs.json.Json
import play.api.mvc.Action
import uk.gov.hmrc.play.http.Upstream5xxResponse

import scala.concurrent.Future

object PropertyLinkingController extends PropertyLinkingBaseController {
  val propertyLinksConnector = Wiring().propertyLinkingConnector
  val propAuthConnector = Wiring().propertyLinkingConnector
  val groupAccountsConnector = Wiring().groupAccounts

  def create() = Action.async(parse.json) { implicit request =>
    withJsonBody[PropertyLinkRequest] { linkRequest =>
      propertyLinksConnector.create(APIPropertyLinkRequest.fromPropertyLinkRequest(linkRequest))
        .map { _ => Created }
        .recover { case _: Upstream5xxResponse => InternalServerError }
    }
  }

  def find(organisationId: Int) = Action.async { implicit request => {
    (for {
      props <- propertyLinksConnector.find(organisationId)
      res <- Future.traverse(props)(prop => {
        for {
          optionalGroupAccounts <- Future.traverse(prop.parties)(party => groupAccountsConnector.get(party.authorisedPartyOrganisationId))
          groupAccounts = optionalGroupAccounts.flatten
        } yield DetailedPropertyLink.fromAPIAuthorisation(prop, groupAccounts)
      })
    } yield {
      res
    }).map(x => Ok(Json.toJson(x)))
  }
  }

  def get(authorisationId: Long) = Action.async { implicit request =>
    propertyLinksConnector.get(authorisationId) map { x => Ok(Json.toJson(x)) }
  }

  def assessments(authorisationId: Long) = Action.async { implicit request =>
    propertyLinksConnector.getAssessment(authorisationId) map { x => Ok(Json.toJson(x)) }
  }
}
