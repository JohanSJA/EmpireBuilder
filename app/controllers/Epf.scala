package controllers

import java.util.Date
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.mvc._

import models._

object Epf extends Controller {

  val infoForm = Form(
    mapping(
      "citizenship" -> nonEmptyText,
      "dateOfBirth" -> date,
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
          case EmployeeInfo("M", dob, wages) =>
            Redirect(routes.Epf.rate("A", info.wages.toDouble))
          case EmployeeInfo("O", dob, wages) =>
            Redirect(routes.Epf.rate("B", info.wages.toDouble))
          case _ => Redirect(routes.Epf.rate("D", info.wages.toDouble))
        }
      }
    )
  }

  def part(partName: String) = DBAction { implicit rs =>
    val partInfo = Parts.get(partName)
    val partRates = Rates.list(partName)
    val result = partInfo map { p => Ok(views.html.epfPart(p, partRates)) }
    result getOrElse {
      val partInfo = Part(partName, "This is an unknown part.")
      NotFound(views.html.epfPart(partInfo, partRates))
    }
  }

  def rate(part: String, wages: Double) = DBAction { implicit rs =>
    val partInfo = Parts.get(part)
    val result = partInfo map { p =>
      val rateInfo = Rates.get(part, wages)
      val result = rateInfo map { r => Ok(views.html.epfRate(wages, p, r)) }
      result getOrElse {
        val rateInfo = Rate(p.name, wages, wages, None, None)
        NotFound(views.html.epfRate(wages, p, rateInfo))
      }
    }
    result getOrElse {
      val partInfo = Part(part, "This is an unknown part.")
      val rateInfo = Rate(part, wages, wages, None, None)
      NotFound(views.html.epfRate(wages, partInfo, rateInfo))
    }
  }

}
