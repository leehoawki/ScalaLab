package ch08

trait Prop0 {
  def check: Boolean

  def &&(p: Prop0): Prop0 = new Prop0 {
    override def check: Boolean = Prop0.this.check && p.check
  }
}