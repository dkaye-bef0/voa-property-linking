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

package connectors

import controllers.ControllerSpec
import models.SimpleAddress
import play.api.libs.json.{JsArray, Json}
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}
import utils.StubHttp

class AddressConnectorSpec extends ControllerSpec {

  implicit val hc = HeaderCarrier()
  lazy val connector = new AddressConnector(StubHttp) {
    override lazy val url = "/address-management-api/address"
  }

  "The postcode lookup" should {

    "strip out empty lines to reduce the number of lines to 4" in {
      StubHttp.stubGet(
        """/address-management-api/address?pageSize=100&startPoint=1&searchparams={"postcode": "AA11 1AA"}""",
        HttpResponse(200, Some(address(subBuildingName = "The Thing", buildingNumber = "1", thoroughFareName = "The Road", postTown = "The Town", postcode = "AA11 1AA")))
      )

      await(connector.find("AA11 1AA")) must be(Seq(SimpleAddress(None, "The Thing", "1", "The Road", "The Town", "AA11 1AA")))
    }
  }

  def address(organisationName: String = "",
              departmentName: String = "",
              subBuildingName: String = "",
              buildingName: String = "",
              buildingNumber: String = "",
              dependentThoroughFareName: String = "",
              thoroughFareName: String = "",
              doubleDependentLocality: String = "",
              dependentLocality: String = "",
              postTown: String = "",
              postcode: String = "") = {
    Json.obj("addressDetails" -> JsArray(Seq(
      Json.obj(
        "organisationName" -> organisationName,
        "departmentName" -> departmentName,
        "subBuildingName" -> subBuildingName,
        "buildingName" -> buildingName,
        "buildingNumber" -> buildingNumber,
        "dependentThoroughFareName" -> dependentThoroughFareName,
        "thoroughFareName" -> thoroughFareName,
        "doubleDependentLocality" -> doubleDependentLocality,
        "dependentLocality" -> dependentLocality,
        "postTown" -> postTown,
        "postcode" -> postcode
      )
    )))
  }
}
