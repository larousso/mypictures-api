package services.validation

import cats._
import cats.data._
import cats.functor.Bifunctor
import cats.instances.either._
//import cats.syntax.either._
import cats.data.Validated._
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsError, JsPath, JsResult, JsSuccess}

/**
  * Created by adelegue on 04/11/2016.
  */

case class ValidatedT[F[_], E, A](value: F[Validated[E, A]]) {
  import cats.data.Validated._

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = F.map(value)(_.getOrElse(default))

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] = {
    F.flatMap(value) {
      case Invalid(_) => default
      case Valid(b) => F.pure(b)
    }
  }

  def orElse(default: => ValidatedT[F, E, A])(implicit F: Monad[F]): ValidatedT[F, E, A] = {
    ValidatedT(F.flatMap(value) {
      case Invalid(_) => default.value
      case r @ Valid(_) => F.pure(r)
    })
  }

  def recover(pf: PartialFunction[E, A])(implicit F: Functor[F]): ValidatedT[F, E, A] =
    ValidatedT(F.map(value){
      case Invalid(a) if pf.isDefinedAt(a) => Valid(pf(a))
      case other => other
    })

  def recoverWith(pf: PartialFunction[E, ValidatedT[F, E, A]])(implicit F: Monad[F]): ValidatedT[F, E, A] =
    ValidatedT(F.flatMap(value) {
      case Invalid(a) if pf.isDefinedAt(a) => pf(a).value
      case other => F.pure(other)
    })

  def valueOr(f: E => A)(implicit F: Functor[F]): F[A] = fold(f, identity)

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

//  def ensure(onFailure: => A)(f: A => Boolean)(implicit F: Functor[F]): ValidatedT[F, E, A] = ValidatedT(F.map(value)(_.ensure(onFailure)(f)))

  def toOption(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(value)(_.toOption))

//  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[A]] =
//    F.map(value)(_.to[G])
//
//  def collectValid(implicit F: MonadCombine[F]): F[A] =
//    F.flatMap(value)(_.to[F])

  def bimap[C, D](fa: E => C, fb: A => D)(implicit F: Functor[F]): ValidatedT[F, C, D] = ValidatedT(F.map(value)(_.bimap(fa, fb)))
//
//  def applyAlt[D](ff: ValidatedT[F, E, A => D])(implicit F: Apply[F]): ValidatedT[F, E, D] =
//    ValidatedT[F, E, D](F.map2(this.value, ff.value)((xb, xbd) => Applicative[Validated[E, ?]].ap(xbd)(xb)))

  def fold[B](fe: E => B, fa: A => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.fold(fe, fa))

  def flatMap[B](f: A => ValidatedT[F, E, B])(implicit F: Monad[F]): ValidatedT[F, E, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Validated[E, B]])(implicit F: Monad[F]): ValidatedT[F, E, B] =
    ValidatedT(
      F.flatMap(value){
        case Valid(a) => f(a)
        case Invalid(e) => F.pure(invalid(e))
      })

  def map[B](f: A => B)(implicit F: Functor[F]): ValidatedT[F, E, B] =
    ValidatedT(F.map(value)(_.map(f)))

  def invalidMap[C](f: E => C)(implicit F: Functor[F]): ValidatedT[F, C, A] = bimap(f, identity)

  def compare(that: ValidatedT[F, E, A])(implicit o: Order[F[Validated[E, A]]]): Int =
    o.compare(value, that.value)

  def bitraverse[G[_], C, D](f: E => G[C], g: A => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[ValidatedT[F, C, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Bitraverse[Validated].bitraverse(axb)(f, g)))(ValidatedT.apply _ )

  def ===(that: ValidatedT[F, E, A])(implicit eq: Eq[F[Validated[E, A]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: A => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[ValidatedT[F, E, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => applicativeG.map(Traverse[Either[E, ?]].traverse(axb.toEither)(f))(Validated.fromEither)))(ValidatedT.apply _)

  def foldLeft[C](c: C)(f: (C, A) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (A, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((axb, lc) => axb.foldRight(lc)(f))

  def partialCompare(that: ValidatedT[F, E, A])(implicit p: PartialOrder[F[Validated[E, A]]]): Double =
    p.partialCompare(value, that.value)

  def merge(implicit ev: A <:< E, F: Functor[F]): F[E] = F.map(value)(_.fold(identity, ev.apply))


//  def withFilter(f: A => Boolean)(implicit F: Monad[F]) = F.filter(value){
//    case Valid(e) => f(e)
//    case Invalid(_) => false
//  }
}

object ValidatedT extends ValidatedTInstances with ValidatedTFunctions { }


private[validation] trait ValidatedTFunctions {

  final def validT[F[_], E, A](fa: F[A])(implicit F: Functor[F]): ValidatedT[F, E, A] = ValidatedT(F.map(fa)(Validated.valid))

  final def invalidT[F[_], E, A](fb: F[E])(implicit F: Functor[F]): ValidatedT[F, E, A] = ValidatedT(F.map(fb)(Validated.invalid))

  def pure[F[_], E, A](a: A)(implicit F: Applicative[F]): ValidatedT[F, E, A] =
    ValidatedT(F.pure(Valid(a)))

  final def liftT[F[_], E, A](fb: F[A])(implicit F: Functor[F]): ValidatedT[F, E, A] = validT(fb)


  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

  final class FromEitherPartiallyApplied[F[_]] private[ValidatedTFunctions] {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): ValidatedT[F, E, A] =
      either match {
        case Right(r) => ValidatedT(F.pure(Validated.valid(r)))
        case Left(e) => ValidatedT(F.pure(Validated.invalid(e)))
      }
  }

  final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  final class FromOptionPartiallyApplied[F[_]] {
    def apply[E, A](opt: Option[A], ifNone: => E)(implicit F: Applicative[F]): ValidatedT[F, E, A] =
      ValidatedT(F.pure(Validated.fromOption(opt, ifNone)))
  }

  final def fromOptionF[F[_]]: FromOptionFPartiallyApplied[F] = new FromOptionFPartiallyApplied

  final class FromOptionFPartiallyApplied[F[_]] {
    def apply[E, A](opt: F[Option[A]], ifNone: => E)(implicit F: Applicative[F]): ValidatedT[F, E, A] =
      ValidatedT(F.map(opt)(o => Validated.fromOption(o, ifNone)))
  }

  final def fromJsResult[F[_]]: FromJsResultPartiallyApplied[F] = new FromJsResultPartiallyApplied

  final class FromJsResultPartiallyApplied[F[_]] private[ValidatedTFunctions] {
    def apply[E, A](jsResult: JsResult[A], onErrors: Seq[(JsPath, Seq[ValidationError])] => E)(implicit F: Applicative[F]): ValidatedT[F, E, A] =
      jsResult match {
        case JsSuccess(success, _) => ValidatedT(F.pure(Validated.valid(success)))
        case JsError(errors) =>
          ValidatedT(F.pure(Validated.invalid(onErrors(errors))))
      }
  }

  final def fromJsResultF[F[_]]: FromJsResultFPartiallyApplied[F] = new FromJsResultFPartiallyApplied

  final class FromJsResultFPartiallyApplied[F[_]] private[ValidatedTFunctions] {
    def apply[E, A](jsResult: F[JsResult[A]], onErrors: Seq[(JsPath, Seq[ValidationError])] => E)(implicit F: Applicative[F]): ValidatedT[F, E, A] =
      ValidatedT(F.map(jsResult) {
        case JsSuccess(success, _) => Validated.valid(success)
        case JsError(errors) => Validated.invalid(onErrors(errors))
      })
  }
}

private[validation] abstract class ValidatedTInstances {

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

//  implicit def validatedMonoid[F[_], E, A](implicit M: Monoid[F]): Monoid[F[Validated[E, ?]]] =
//    new Monoid[F[Validated[E, ?]]]{
//      override def empty: F[Validated[E, ?]] = M.empty[F[]]
//
//      override def combine(x: F[Validated[E, ?]], y: F[Validated[E, ?]]): F[Validated[E, ?]] = (x, y) match {
//
//      }
//    }

  implicit def catsDataOrderForValidatedT[F[_], L, R](implicit F: Order[F[Validated[L, R]]]): Order[ValidatedT[F, L, R]] =
    new ValidatedTOrder[F, L, R] {
      val F0: Order[F[Validated[L, R]]] = F
    }

  implicit def catsDataShowForValidatedT[F[_], L, R](implicit sh: Show[F[Validated[L, R]]]): Show[ValidatedT[F, L, R]] =
    functor.Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForValidatedT[F[_]](implicit F: Functor[F]): Bifunctor[ValidatedT[F, ?, ?]] =
    new Bifunctor[ValidatedT[F, ?, ?]] {
      override def bimap[A, B, C, D](fab: ValidatedT[F, A, B])(f: A => C, g: B => D): ValidatedT[F, C, D] = fab.bimap(f, g)
    }

  implicit def catsDataTraverseForValidatedT[F[_], L](implicit F: Traverse[F]): Traverse[ValidatedT[F, L, ?]] =
    new ValidatedTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataTransLiftForValidatedT[E]: TransLift.Aux[ValidatedT[?[_], E, ?], Functor] =
    new TransLift[ValidatedT[?[_], E, ?]] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): ValidatedT[M, E, A] =
        ValidatedT.liftT(ma)
    }

  implicit def catsMonoidForValidatedT[F[_], E, A](implicit F: Monoid[F[Validated[E, A]]]): Monoid[ValidatedT[F, E, A]] =
    new ValidatedTMonoid[F, E, A] { implicit val F0 = F }




  implicit def catsSemigroupForValidatedT[F[_], L, A](implicit F: Semigroup[F[Validated[L, A]]]): Semigroup[ValidatedT[F, L, A]] =
    new ValidatedTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForValidatedT[F[_], L](implicit F: Foldable[F]): Foldable[ValidatedT[F, L, ?]] =
    new ValidatedTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForValidatedT[F[_], L, R](implicit F: PartialOrder[F[Validated[L, R]]]): PartialOrder[ValidatedT[F, L, R]] =
    new ValidatedTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[Validated[L, R]]] = F
    }

  implicit def catsDataBitraverseForValidatedT[F[_]](implicit F: Traverse[F]): Bitraverse[ValidatedT[F, ?, ?]] =
    new ValidatedTBitraverse[F] {
      val F0: Traverse[F] = F
    }



  implicit def catsDataMonadErrorForValidatedT[F[_], L](implicit F0: Monad[F]): MonadError[ValidatedT[F, L, ?], L] =
    new ValidatedTMonadError[F, L] { implicit val F = F0 }

  implicit def catsDataSemigroupKForValidatedT[F[_], L](implicit F0: Monad[F]): SemigroupK[ValidatedT[F, L, ?]] =
    new ValidatedTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForValidatedT[F[_], L, R](implicit F: Eq[F[Validated[L, R]]]): Eq[ValidatedT[F, L, R]] =
    new ValidatedTEq[F, L, R] {
      val F0: Eq[F[Validated[L, R]]] = F
    }


  implicit def catsDataFunctorForValidatedT[F[_], L](implicit F0: Functor[F]): Functor[ValidatedT[F, L, ?]] =
    new ValidatedTFunctor[F, L] { implicit val F = F0 }


}


private[validation] trait ValidatedTSemigroup[F[_], E, A] extends Semigroup[ValidatedT[F, E, A]] {
  implicit val F0: Semigroup[F[Validated[E, A]]]
  def combine(x: ValidatedT[F, E , A], y: ValidatedT[F, E , A]): ValidatedT[F, E , A] =
    ValidatedT(F0.combine(x.value, y.value))
}

private[validation] trait ValidatedTMonoid[F[_], E, A] extends Monoid[ValidatedT[F, E, A]] with ValidatedTSemigroup[F, E, A] {
  implicit val F0: Monoid[F[Validated[E, A]]]
  def empty: ValidatedT[F, E, A] = ValidatedT(F0.empty)
}

private[validation] trait ValidatedTSemigroupK[F[_], E] extends SemigroupK[ValidatedT[F, E, ?]] {
  implicit val F: Monad[F]
  def combineK[A](x: ValidatedT[F, E, A], y: ValidatedT[F, E, A]): ValidatedT[F, E, A] =
    ValidatedT(F.flatMap(x.value) {
      case l @ Invalid(_) => y.value
      case r @ Valid(_) => F.pure(r)
    })
}

private[validation] trait ValidatedTFunctor[F[_], E] extends Functor[ValidatedT[F, E, ?]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: ValidatedT[F, E, A])(f: A => B): ValidatedT[F, E, B] = fa map f
}

private[validation] trait ValidatedTMonad[F[_], E] extends Monad[ValidatedT[F, E, ?]] with ValidatedTFunctor[F, E] {
  implicit val F: Monad[F]

  override def pure[A](x: A): ValidatedT[F, E, A] = ValidatedT.pure(x)

  override def flatMap[A, B](fa: ValidatedT[F, E, A])(f: (A) => ValidatedT[F, E, B]): ValidatedT[F, E, B] =
    fa.flatMap(f)

  override def tailRecM[A, B](a: A)(f: (A) => ValidatedT[F, E, Either[A, B]]): ValidatedT[F, E, B] =
    ValidatedT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
      case Invalid(l)         => Right(Invalid(l))
      case Valid(Left(a1)) => Left(a1)
      case Valid(Right(b)) => Right(Valid(b))
    }))
}

private[validation] trait ValidatedTMonadError[F[_], E] extends MonadError[ValidatedT[F, E, ?], E] with ValidatedTMonad[F, E] {
  implicit val F: Monad[F]

  def handleErrorWith[A](fea: ValidatedT[F, E, A])(f: E => ValidatedT[F, E, A]): ValidatedT[F, E, A] =
    ValidatedT(F.flatMap(fea.value) {
      case Invalid(e) => f(e).value
      case r @ Valid(_) => F.pure(r)
    })
  override def handleError[A](fea: ValidatedT[F, E, A])(f: E => A): ValidatedT[F, E, A] =
    ValidatedT(F.flatMap(fea.value) {
      case Invalid(e) => F.pure(Valid(f(e)))
      case r @ Valid(_) => F.pure(r)
    })
  def raiseError[A](e: E): ValidatedT[F, E, A] = ValidatedT.invalidT[F, E, A](F.pure(e))

  override def attempt[A](fla: ValidatedT[F, E, A]): ValidatedT[F, E, Either[E, A]] = {
    ValidatedT(F.map(fla.value) {
      case Valid(e) => Valid(Right(e))
      case Invalid(e) => Valid(Left(e))
    })
  }

  override def recover[A](fla: ValidatedT[F, E, A])(pf: PartialFunction[E, A]): ValidatedT[F, E, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: ValidatedT[F, E, A])(pf: PartialFunction[E, ValidatedT[F, E, A]]): ValidatedT[F, E, A] =
    fla.recoverWith(pf)
}

private[validation] trait ValidatedTMonadFilter[F[_], E] extends MonadFilter[ValidatedT[F, E, ?]] with ValidatedTMonadError[F, E] {
  implicit val F: Monad[F]
  implicit val E: Monoid[E]
  def empty[A]: ValidatedT[F, E, A] = ValidatedT(F.pure(Validated.invalid(E.empty)))
}


private[validation] sealed trait ValidatedTFoldable[F[_], L] extends Foldable[ValidatedT[F, L, ?]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: ValidatedT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: ValidatedT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[validation] sealed trait ValidatedTTraverse[F[_], L] extends Traverse[ValidatedT[F, L, ?]] with ValidatedTFoldable[F, L] {
  override implicit def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: ValidatedT[F, L, A])(f: A => G[B]): G[ValidatedT[F, L, B]] =
    fa traverse f
}

private[validation] sealed trait ValidatedTBifoldable[F[_]] extends Bifoldable[ValidatedT[F, ?, ?]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: ValidatedT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)( (acc, axb) => Bifoldable[Validated].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: ValidatedT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F0.foldRight(fab.value, c)( (axb, acc) => Bifoldable[Validated].bifoldRight(axb, acc)(f, g))
}

private[validation] sealed trait ValidatedTBitraverse[F[_]] extends Bitraverse[ValidatedT[F, ?, ?]] with ValidatedTBifoldable[F] {
  override implicit def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](fab: ValidatedT[F, A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[ValidatedT[F, C, D]] =
    fab.bitraverse(f, g)
}

private[validation] sealed trait ValidatedTEq[F[_], L, A] extends Eq[ValidatedT[F, L, A]] {
  implicit def F0: Eq[F[Validated[L, A]]]

  override def eqv(x: ValidatedT[F, L, A], y: ValidatedT[F, L, A]): Boolean = x === y
}

private[validation] sealed trait ValidatedTPartialOrder[F[_], L, A] extends PartialOrder[ValidatedT[F, L, A]] with ValidatedTEq[F, L, A]{
  override implicit def F0: PartialOrder[F[Validated[L, A]]]

  override def partialCompare(x: ValidatedT[F, L, A], y: ValidatedT[F, L, A]): Double =
    x partialCompare y
}

private[validation] sealed trait ValidatedTOrder[F[_], E, A] extends Order[ValidatedT[F, E, A]] with ValidatedTPartialOrder[F, E, A]{
  override implicit def F0: Order[F[Validated[E, A]]]

  override def compare(x: ValidatedT[F, E, A], y: ValidatedT[F, E, A]): Int = x compare y
}
