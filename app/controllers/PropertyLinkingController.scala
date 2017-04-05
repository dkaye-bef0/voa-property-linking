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

import connectors.{GroupAccountConnector, PropertyLinkingConnector, PropertyRepresentationConnector}
import models._
import play.api.libs.json.Json
import play.api.mvc.Action
import uk.gov.hmrc.play.http.{HeaderCarrier, Upstream5xxResponse}

import scala.concurrent.Future

class PropertyLinkingController @Inject()(
                                           propertyLinksConnector: PropertyLinkingConnector,
                                           groupAccountsConnector: GroupAccountConnector,
                                           representationsConnector: PropertyRepresentationConnector
                                         ) extends PropertyLinkingBaseController {

  def create() = Action.async(parse.json) { implicit request =>
    withJsonBody[PropertyLinkRequest] { linkRequest =>
      propertyLinksConnector.create(APIPropertyLinkRequest.fromPropertyLinkRequest(linkRequest))
        .map { _ => Created }
        .recover { case _: Upstream5xxResponse => InternalServerError }
    }
  }

  private def getProperties(organisationId: Long, userActingAsAgent: Boolean = false)(implicit hc: HeaderCarrier): Future[Seq[DetailedPropertyLink]] = {
    for {
      props <- propertyLinksConnector.find(organisationId)
      res <- Future.traverse(props)(prop => {
        for {
          optionalGroupAccounts <- Future.traverse(prop.parties)(party => {
            groupAccountsConnector.get(party.authorisedPartyOrganisationId).map(_.map(groupAccount => (party, groupAccount)))
          })
          apiPartiesWithGroupAccounts = optionalGroupAccounts.flatten
          parties = apiPartiesWithGroupAccounts.flatMap { case (p: APIParty, g: GroupAccount) => Party.fromAPIParty(p, g) }
        } yield DetailedPropertyLink.fromAPIAuthorisation(prop, parties, userActingAsAgent)
      })
    } yield {
      res
    }
  }

  private def getPropertiesWithAgent(organisationId: Long, agentOrgId: Long)(implicit hc: HeaderCarrier): Future[Seq[DetailedPropertyLink]] = {
    for {
      props <- getProperties(organisationId, userActingAsAgent = true)
      filterProps = props
        .map(p => p.copy(agents = p.agents.filter(_.organisationId == agentOrgId)))
        .filter(_.agents.nonEmpty)
    } yield {
      filterProps
    }
  }

  def find(organisationId: Long) = Action.async { implicit request =>
    val eventualUserProps = getProperties(organisationId)
    val eventualNominations = representationsConnector.forAgent("APPROVED", organisationId)

    val allProps = for {
      userProps <- eventualUserProps
      nominations <- eventualNominations
      clientOrgIds = nominations.propertyRepresentations.map({
        _.organisationId
      }).distinct
      res <- Future.traverse(clientOrgIds)(userOrgId => getPropertiesWithAgent(userOrgId, organisationId))
      managedProperties = res.flatten
    } yield {
      managedProperties ++ userProps
    }

    allProps.map(x => Ok(Json.toJson(x)))
  }

  def clientProperties(userOrgId: Long, agentOrgId: Long) = Action.async { implicit request => {
    (for {
      props <- propertyLinksConnector.find(userOrgId)
      filteredProps = props.filter(_.parties.map(_.authorisedPartyOrganisationId).contains(agentOrgId))
      filteredPropsAgents = filteredProps.map(prop => prop.copy(parties = prop.parties.filter(_.authorisedPartyOrganisationId == agentOrgId)))
      userAccount <- groupAccountsConnector.get(props.head.authorisationOwnerOrganisationId)
    } yield {
      filteredPropsAgents.map(x => ClientProperty.build(x, userAccount))
    }).map(x => Ok(Json.toJson(x)))
  }
  }

  def assessments(authorisationId: Long) = Action.async { implicit request =>
    propertyLinksConnector.getAssessment(authorisationId) map { x => Ok(Json.toJson(x)) }
  }

  private def resolveGroupIds(ids: Seq[Long])(implicit hc: HeaderCarrier): Future[Map[Long, Option[GroupAccount]]] = {
    Future.sequence(ids.map(id => groupAccountsConnector.get(id).map((id, _)))).map(_.toMap)
  }

  private def createDetailedPropertyLink(apiAuth: APIAuthorisation, groupAccountsMap: Map[Long, Option[GroupAccount]], isAgent:Boolean): DetailedPropertyLink = {
    val parties = apiAuth.parties.flatMap(apiParty => groupAccountsMap(apiParty.authorisedPartyOrganisationId).map(Party.fromAPIParty(apiParty, _))).flatten
    DetailedPropertyLink.fromAPIAuthorisation(apiAuth, parties, isAgent)
  }

  private def getPropertyLinksFor(organisationId: Long, uarn: Option[Long])(implicit hc: HeaderCarrier): Future[Seq[BasicPropertyLink]] = {
    val eventualPersonalProperties = propertyLinksConnector.findFor(organisationId, uarn)
    val eventualRepresentations = representationsConnector.forAgent("APPROVED", organisationId)
    for {
      representations <- eventualRepresentations
      managedProperties <- Future.sequence(representations.propertyRepresentations.map(representation => propertyLinksConnector.getBySubmissionId(representation.submissionId)))
      personalProperties <- eventualPersonalProperties
      filteredManagedProperties = uarn.fold(managedProperties.flatten)(u => managedProperties.flatten.filter(_.uarn==u))
    } yield (personalProperties ++ filteredManagedProperties).flatMap(BasicPropertyLink.fromAPIAuthorisationResult(_))
  }

  def findFor(organisationId: Long, uarn: Long) = Action.async { implicit request => {
    val eventualPropertyLinks = getPropertyLinksFor(organisationId, Some(uarn))
    eventualPropertyLinks.map(x => Ok(Json.toJson(x)))
  } }

  def findForOrg(organisationId: Long) = Action.async { implicit request => {
    val eventualPropertyLinks = getPropertyLinksFor(organisationId, None)
    eventualPropertyLinks.map(x => Ok(Json.toJson(x)))
  } }

  def get(authorisationId: Long) = Action.async { implicit request => {
    for {
      apiAuth <- propertyLinksConnector.get(authorisationId)
    } yield BasicPropertyLink.fromAPIAuthorisationResult(apiAuth) match {
      case Some(link: BasicPropertyLink) => Ok(Json.toJson(link))
      case None => NotFound
    }
  } }

  def setEnd(authorisationId: Long) = Action.async(parse.json) { implicit request =>
    withJsonBody[PropertyLinkEndDateRequest] { endRequest =>
      propertyLinksConnector.setEnd(authorisationId, APIPropertyLinkEndDateRequest.fromPropertyLinkEndDateRequest(endRequest)) map { _ =>
        Ok
      }
    }
  }

}

