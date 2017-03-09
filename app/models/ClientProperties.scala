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

import play.api.libs.json.Json

case class ClientProperties (
                              clientOrganisationName: String,
                              uarn: Long,
                              billingAuthorityReference: String,
                              checkPermission: String,
                              challengePermission: String,
                              address: String
                            )
object ClientProperties {
  implicit val format = Json.format[ClientProperties]

  def build(prop: APIAuthorisation, userAccount: Option[GroupAccount]) = {
    ClientProperties(
      userAccount.map(_.companyName).getOrElse("Name not found"),
      prop.uarn,
      prop.NDRListValuationHistoryItems.headOption.map(_.billingAuthorityReference).getOrElse("BARef not found"),
      prop.parties.head.permissions.head.checkPermission,
      prop.parties.head.permissions.head.challengePermission,
      prop.NDRListValuationHistoryItems.headOption.map(_.address).getOrElse("Address not found")
    )

  }
}