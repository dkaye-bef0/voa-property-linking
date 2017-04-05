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

package models

import org.joda.time.DateTime
import play.api.libs.json.Json

case class BasicPropertyLink(authorisationId: Long,
                                submissionId: String,
                                uarn: Long,
                                organisationId: Long,
                                personId: Long,
                                capacityDeclaration: CapacityDeclaration,
                                linkedDate: DateTime,
                                pending: Boolean,
                                parties: Seq[APIParty]) {
}


object BasicPropertyLink {
  implicit val formats = Json.format[BasicPropertyLink]

  def fromAPIAuthorisation(prop: APIAuthorisation) = {
    val capacityDeclaration = CapacityDeclaration(prop.authorisationOwnerCapacity, prop.startDate, prop.endDate)
    BasicPropertyLink(
      prop.authorisationId,
      prop.submissionId,
      prop.uarn,
      prop.authorisationOwnerOrganisationId,
      prop.authorisationOwnerPersonId,
      capacityDeclaration,
      prop.createDatetime,
      prop.authorisationStatus != "APPROVED",
      prop.parties
    )
  }

  def fromAPIAuthorisationResult(prop: APIAuthorisationResult):Option[BasicPropertyLink] = {
    val capacityDeclaration = CapacityDeclaration(prop.authorisationOwnerCapacity, prop.startDate, prop.endDate)
    prop.authorisationOwnerPersonId.map {
      BasicPropertyLink(
        prop.id.get,
        prop.submissionId,
        prop.uarn,
        prop.authorisationOwnerOrganisationId,
        _,
        capacityDeclaration,
        prop.createDatetime,
        prop.authorisationStatus != "APPROVED",
        prop.parties.getOrElse(Seq[APIParty]())
      )
    }
  }
}
