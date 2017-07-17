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

import javax.inject.Inject

import connectors.PropertyRepresentationConnector
import models._
import play.api.libs.json.Json
import play.api.mvc.Action

class PropertyRepresentationController @Inject() (representations: PropertyRepresentationConnector) extends PropertyLinkingBaseController {

  def validateAgentCode(agentCode:Long, authorisationId: Long) = Action.async { implicit request =>
    representations.validateAgentCode(agentCode, authorisationId).map(
      _.fold(
        orgId => Ok(s"""{"organisationId": $orgId}"""),
        errorString => Ok(s"""{"failureCode": "$errorString"}""")
      )
    )
  }

  def forAgent(status: String, organisationId: Long, pagination: PaginationParams) = Action.async { implicit request =>
    representations.forAgent(status, organisationId, pagination) map { reps =>
      val filtered = reps.propertyRepresentations.filterNot(rep => Seq("DECLINED", "REVOKED").contains(rep.status.toUpperCase))
      Ok(Json.toJson(reps.copy(propertyRepresentations = filtered)))
    }
  }

  def create() = Action.async(parse.json) { implicit request =>
    withJsonBody[RepresentationRequest] { reprRequest =>
      representations.create(APIRepresentationRequest.fromRepresentationRequest(reprRequest)).map(x => Ok(""))
    }
  }

  def response() = Action.async(parse.json) { implicit request =>
    withJsonBody[APIRepresentationResponse] { r =>
      representations.response(r) map { _ => Ok("") }
    }
  }

  def revoke(authorisedPartyId: Long) = Action.async { implicit request =>
    representations.revoke(authorisedPartyId)  map { _ => Ok("") }
  }

}
