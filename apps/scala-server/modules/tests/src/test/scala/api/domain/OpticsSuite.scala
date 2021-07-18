package api.domain

import java.util.UUID

import api.domain.language.LanguageId
import api.domain.category.CategoryId
import api.domain.healthcheck.Status
import api.generators._
import api.optics.IsUUID

import monocle.law.discipline._
import org.scalacheck.{ Arbitrary, Cogen, Gen }
import weaver.FunSuite
import weaver.discipline.Discipline

object OpticsSuite extends FunSuite with Discipline {}
