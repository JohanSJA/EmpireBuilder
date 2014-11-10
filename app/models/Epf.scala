package models

import play.api.db.slick.Config.driver.simple._

case class Part(name: String, description: String)

class Parts(tag: Tag) extends Table[Part](tag, "PARTS") {
  def name = column[String]("name", O.PrimaryKey)
  def description = column[String]("description")

  def * = (name, description) <> (Part.tupled, Part.unapply _)
}


case class Rate(partName: String, wagesTo: Double,
    contributionEmployer: Option[Double], contributionEmployee: Option[Double])

class Rates(tag: Tag) extends Table[Rate](tag, "RATES") {
  def partName = column[String]("partName")
  def wagesTo = column[Double]("wagesTo")
  def contributionEmployer = column[Option[Double]]("contributionEmployer")
  def contributionEmployee = column[Option[Double]]("contributionEmployee")

  def * = (partName, wagesTo, contributionEmployer, contributionEmployee) <> (Rate.tupled, Rate.unapply _)
}
