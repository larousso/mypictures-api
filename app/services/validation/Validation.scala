package services.validation

import cats.SemigroupK


//import scalaz._

/**
  * Created by adelegue on 28/05/2016.
  */
object Validation {

  import cats.{Applicative, Semigroup}
  import cats.data.Validated._
  import cats.data.{NonEmptyList, Validated, ValidatedNel}

  type Result[S] = ValidatedNel[Validation.Error, S]

  type ResultT[F[_], S] = ValidatedT[F, NonEmptyList[Validation.Error], S]

  case class Error(message: String)

  private val nel: Result[String] = valid("")
  private val nel2: Result[String] = invalidNel(Validation.Error(""))

  implicit val nelSemigroup: Semigroup[NonEmptyList[Validation.Error]] =
    SemigroupK[NonEmptyList].algebra[Validation.Error]


  implicit def validatedApplicative[E : Semigroup]: Applicative[Validated[E, ?]] =
    new Applicative[Validated[E, ?]] {
      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        (fa, f) match {
          case (Valid(a), Valid(fab)) => Valid(fab(a))
          case (i@Invalid(_), Valid(_)) => i
          case (Valid(_), i@Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }

      def pure[A](x: A): Validated[E, A] = Validated.valid(x)
    }
  //  def f1(v1: Valid[String], v2: Valid[String]) =
  //    Apply[Valid].map2(v1, v2)( _ ++ _ )
//
//  Applicative[Result].map2(nel, nel2)( _ ++ _ )
//
//  val res = (nel |@| nel2).map { _ ++ _ }

}
