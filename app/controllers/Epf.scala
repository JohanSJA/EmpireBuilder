package controllers

import play.api._
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.mvc._

import models._

object Epf extends Controller {

  val parts = TableQuery[Parts]

  def index() = DBAction { implicit rs =>
    Ok(views.html.epf(parts.list))
  }

  def part(partName: String) = DBAction { implicit rs =>
    try {
      val partInfo = parts.filter(_.name === partName).first
      val partRates = Rates.list(partName)
      Ok(views.html.epfPart(partInfo, partRates))
    } catch {
      case e: NoSuchElementException =>
        NotFound("Unknown part")
    }
  }

  def rate(part: String, wages: Double) = DBAction { implicit rs =>
    try {
      val rates = Rates.list(part)
      val rateInfo = rates.filter(r => r.wagesFrom <= wages && r.wagesTo >= wages)(0)
      Ok(views.html.epfRate(wages, rateInfo))
    } catch {
      case e: IndexOutOfBoundsException =>
        Ok(s"Unknown rate for $wages")
    }
  }

}
