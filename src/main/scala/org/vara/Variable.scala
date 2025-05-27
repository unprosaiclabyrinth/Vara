package org.vara

implicit class Variable(val name: String) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr = env.getOrElse(name, this)
  
  def ast: String = s"Var($name)"
  
  override def toString: String = name

  override def equals(that: Any): Boolean = that match
    case that: Variable => this.name == that.name
    case that: String => this.name == that
    case _ => false

  override def hashCode: Int = name.hashCode
