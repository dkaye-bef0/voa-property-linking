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

import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json._

case class PropertiesViewResponse(resultCount: Option[Int], authorisations: Seq[PropertiesView])

object PropertiesViewResponse {
  implicit val format: Format[PropertiesViewResponse] = Json.format[PropertiesViewResponse]
}

case class PropertiesView(authorisationId: Long,
                          uarn: Long,
                          authorisationOwnerOrganisationId: Long,
                          authorisationOwnerPersonId: Long,
                          authorisationStatus: String,
                          authorisationMethod: String,
                          authorisationOwnerCapacity: String,
                          createDatetime: DateTime,
                          startDate: LocalDate,
                          endDate: Option[LocalDate],
                          submissionId: String,
                          NDRListValuationHistoryItems: Seq[APIValuationHistory],
                          parties: Seq[APIParty]) {

  def hasValidStatus: Boolean = {
    !Seq("DECLINED", "REVOKED", "MORE_EVIDENCE_REQUIRED").contains(authorisationStatus.toUpperCase)
  }
}

object PropertiesView {
  implicit val yourJodaDateTimeReads: Reads[DateTime] = Reads.jodaDateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  implicit val yourJodaDateTimeWrites: Writes[DateTime] = Writes.jodaDateWrites("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  implicit val yourJodaDateReads: Reads[LocalDate] = Reads.jodaLocalDateReads("yyyy-MM-dd")
  implicit val yourJodaDateWrites: Writes[LocalDate] = Writes.jodaLocalDateWrites("yyyy-MM-dd")
  implicit val format: Format[PropertiesView] = Json.format[PropertiesView]
}
