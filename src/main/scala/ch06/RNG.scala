package ch06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SmipleRNG(seed:Long) extends RNG {
  override def nextInt: (Int, RNG) = ???
}