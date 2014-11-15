package controllers

import java.util.Date
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.mvc._

import models._

case class EmployeeInfo(dateOfBirth: Date, citizenship: String, wages: BigDecimal)

object Epf extends Controller {

  val infoForm = Form(
    mapping(
      "dateOfBirth" -> date,
      "citizenship" -> nonEmptyText,
      "wages" -> bigDecimal(precision = 20, scale = 2)
    )(EmployeeInfo.apply)(EmployeeInfo.unapply)
  )
  val citizenships = Seq("M" -> "Malaysian", "PR" -> "Permanent Resident", "O" -> "Others")

  def index() = DBAction { implicit rs =>
    Ok(views.html.epf(infoForm, citizenships, Parts.list))
  }

  def check() = DBAction { implicit rs =>
    infoForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(views.html.epf(formWithErrors, citizenships, Parts.list))
      },
      info => {
        info match {
          case EmployeeInfo(dob, "M", wages) =>
            Redirect(routes.Epf.rate("A", info.wages.toDouble))
          case EmployeeInfo(dob, "O", wages) =>
            Redirect(routes.Epf.rate("B", info.wages.toDouble))
          case _ => Redirect(routes.Epf.rate("D", info.wages.toDouble))
        }
      }
    )
  }

  def part(partName: String) = DBAction { implicit rs =>
    try {
      val partInfo = Parts.list.filter(_.name == partName)(0)
      val partRates = Rates.list(partName)
      Ok(views.html.epfPart(partInfo, partRates))
    } catch {
      case e: NoSuchElementException =>
        NotFound("Unknown part")
    }
  }

  def rate(part: String, wages: Double) = DBAction { implicit rs =>
    try {
      val partInfo = Parts.list.filter(_.name == part)(0)
      val rates = Rates.list(part)
      val rateInfo = rates.filter(r => r.wagesFrom <= wages && r.wagesTo >= wages)(0)
      Ok(views.html.epfRate(wages, partInfo, rateInfo))
    } catch {
      case e: IndexOutOfBoundsException =>
        Ok(s"Unknown rate for $wages")
    }
  }

}
