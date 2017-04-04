package models

import uk.gov.hmrc.play.test.UnitSpec
import util.ArbitraryDataGeneration

class AddressConversionSpec extends UnitSpec with ArbitraryDataGeneration {

  "Converting a backend address record to a simplified frontend address" when {
    "the address is 11 lines long" should {
      val detailed = DetailedAddress(
        addressUnitId = None,
        nonAbpAddressId = None,
        organisationName = Some(shortString),
        departmentName = Some(shortString),
        subBuildingName = Some(shortString),
        buildingName = Some(shortString),
        buildingNumber = Some(shortString),
        dependentThoroughfareName = Some(shortString),
        thoroughfareName = Some(shortString),
        doubleDependentLocality = Some(shortString),
        dependentLocality = Some(shortString),
        postTown = shortString,
        postcode = "AA11 1AA"
      )

      val simplified = detailed.simplify

      """join the organisation name and department name into an "organisation" line""" in {
        simplified.line1 shouldBe s"${detailed.organisationName.get}, ${detailed.departmentName.get}"
      }

      """join the sub building name, building name, and building number into a "premises" line""" in {
        simplified.line2 shouldBe s"${detailed.subBuildingName.get}, ${detailed.buildingName.get}, ${detailed.buildingNumber.get}"
      }

      """join the dependent thoroughfare and thoroughfare name into a "thoroughfare" line""" in {
        simplified.line3 shouldBe s"${detailed.dependentThoroughfareName.get}, ${detailed.thoroughfareName.get}"
      }

      """join the double dependent locality, dependent locality, and post town into a "locality" line""" in {
        simplified.line4 shouldBe s"${detailed.doubleDependentLocality.get}, ${detailed.dependentLocality.get}, ${detailed.postTown}"
      }
    }

    "the address does not have an organisation name or department name" should {
      "shift the address lines up such that the first line is not blank" in {
        fail()
      }
    }

    "the address does not have an organisation name" should {
      "populate the first line with the department name" in {
        fail()
      }
    }
  }
}
