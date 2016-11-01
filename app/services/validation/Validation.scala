package services.validation

import scalaz._

/**
  * Created by adelegue on 28/05/2016.
  */
object Validation {
//
//  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] = OneAnd.oneAndSemigroupK[List].algebra[A]
//
//  implicit val nelSemigroup: Semigroup[NonEmptyList[Validation.Error]] = SemigroupK[NonEmptyList].algebra[Validation.Error]
//
//  implicit def validatedApplicative[E : Semigroup]: Applicative[Validated[E, ?]] =
//    new Applicative[Validated[E, ?]] {
//      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
//        (fa, f) match {
//          case (Valid(a), Valid(fab)) => Valid(fab(a))
//          case (i@Invalid(_), Valid(_)) => i
//          case (Valid(_), i@Invalid(_)) => i
//          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
//        }
//
//      def pure[A](x: A): Validated[E, A] = Validated.valid(x)
//    }


  type Result[S] = ValidationNel[Error, S]

  case class Error(message: String)
//
//
//  private val nel: Validated[NonEmptyList[Error], String] = valid("")
//  private val nel2: Validated[NonEmptyList[Error], String] = invalidNel(Error(""))
//  Applicative[Result].map2(nel, nel2) { _ ++ _ }
//  //(nel |@| nel2).map { _ ++ _ }

}
