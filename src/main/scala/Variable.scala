implicit class Variable(val name: String) extends Expr:
  override def eval(using env: Env): Expr = env.getOrElse(name, this)
  
  override def toString: String = s"Var($name)"

  override def equals(that: Any): Boolean = that match
    case that: Variable => this.name == that.name
    case _ => false

  override def hashCode: Int = name.hashCode
