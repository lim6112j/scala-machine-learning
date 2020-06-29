import scala.util.Try

trait ITransform[T, A] {
 self =>
  def |> : PartialFunction[T, Try[A]]
  def map[B](f: A => B): ITransform[T, B] = new ITransform[T, B] {
    override def |> : PartialFunction[T, Try[B]] =
      new PartialFunction[T, Try[B]] {
        override def isDefinedAt(x: T): Boolean = self.|>.isDefinedAt(x)

        override def apply(v1: T): Try[B] = self.|>(v1).map(f)
      }
  }
  def flatMap[B](f: A => ITransform[T, B]): ITransform[T, B] = new ITransform[T, B] {
    override def |> : PartialFunction[T, Try[B]] = new PartialFunction[T, Try[B]] {
      override def isDefinedAt(x: T): Boolean = self.|>.isDefinedAt(x)

      override def apply(v1: T): Try[B] = self.|>(v1).flatMap(f(_).|>(v1))
    }
  }
  def compose[B](tr: ITransform[A, B]): ITransform[T, B] = new ITransform[T, B] {
    override def |> : PartialFunction[T, Try[B]] = new PartialFunction[T, Try[B]] {
      override def isDefinedAt(x: T): Boolean = self.|>.isDefinedAt(x) && tr.|>.isDefinedAt(self.|>(x).get)

      override def apply(v1: T): Try[B] = tr.|>(self.|>(v1).get)
    }
  }
}
object ITransform extends App {
  val sample = 1 to 10
  val isEven: PartialFunction[Int, Any] = {case x if x %2 == 0 => x + "is Even"}
  println(sample collect isEven)
}