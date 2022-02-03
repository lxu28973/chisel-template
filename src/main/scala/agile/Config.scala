package agile

object config {
  // All config parameters' name need to be type Field[T] where T is the actual parameter's type we need in Module,
  // default is the actual parameter's value
  abstract class Field[T] private (val default: Option[T])
  {
    def this() = this(None)
    def this(default: T) = this(Some(default))
  }

  //View is the place to find parameter's value
  abstract class View {
    // Set site to this object
    final def apply[T](pname: Field[T]): T = apply(pname, this)
    final def apply[T](pname: Field[T], site: View): T = {
      val out = find(pname, site)
      require (out.isDefined, s"Key ${pname} is not defined in Parameters")
      out.get
    }

    // Just cast value type to T
    final def lift[T](pname: Field[T]): Option[T] = lift(pname, this)
    final def lift[T](pname: Field[T], site: View): Option[T] = find(pname, site).map(_.asInstanceOf[T])

    protected[config] def find[T](pname: Field[T], site: View): Option[T]
  }

  abstract class Parameters extends View {
    // Combine two Parameterss
    // x alter y: settings in 'y' overrule settings in 'x'
    final def alter(rhs: Parameters): Parameters =
      new ChainParameters(rhs, this)

    final def alter(f: (View, View, View) => PartialFunction[Any,Any]): Parameters =
      alter(Parameters(f))

    final def alterPartial(f: PartialFunction[Any,Any]): Parameters =
      alter(Parameters((_,_,_) => f))

    final def alterMap(m: Map[Any,Any]): Parameters =
      alter(new MapParameters(m))

    protected[config] def chain[T](site: View, tail: View, pname: Field[T]): Option[T]
    protected[config] def find[T](pname: Field[T], site: View) = chain(site, new TerminalView, pname)

    // x orElse y: settings in 'x' overrule settings in 'y'
    final def orElse(x: Parameters): Parameters = x.alter(this)

    // Please use 'alter' or 'orElse' instead of '++'.
    // People expect this to be alter (like Map ++ Map), but it's orElse.
    final def ++ (x: Parameters): Parameters = orElse(x)
  }

  object Parameters {
    def empty: Parameters = new EmptyParameters
    def apply(f: (View, View, View) => PartialFunction[Any,Any]): Parameters = new PartialParameters(f)
  }

  class Config(p: Parameters) extends Parameters {
    def this(f: (View, View, View) => PartialFunction[Any,Any]) = this(Parameters(f))

    protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = p.chain(site, tail, pname)
    override def toString = this.getClass.getSimpleName
    def toInstance = this
  }

  // Internal implementation:

  // This view is at the end, use default value
  private class TerminalView extends View {
    // Get parameter's default value, site is useless
    def find[T](pname: Field[T], site: View): Option[T] = pname.default
  }

  private class ChainView(head: Parameters, tail: View) extends View {
    def find[T](pname: Field[T], site: View) = head.chain(site, tail, pname)
  }

  private class ChainParameters(x: Parameters, y: Parameters) extends Parameters {
    // First to find in x, then y, then tail.
    // Save object x's and object y's info.
    def chain[T](site: View, tail: View, pname: Field[T]) = x.chain(site, new ChainView(y, tail), pname)
  }

  private class EmptyParameters extends Parameters {
    def chain[T](site: View, tail: View, pname: Field[T]) = tail.find(pname, site)
  }

  private class PartialParameters(f: (View, View, View) => PartialFunction[Any,Any]) extends Parameters {
    // Actual chain used in Config built with partial function. tail is TerminalView or ChainView
    protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = {
      // g saves this parameter's info, here is set to this object
      val g = f(site, this, tail)
      if (g.isDefinedAt(pname)) Some(g.apply(pname).asInstanceOf[T]) else tail.find(pname, site)
    }
  }

  // Use Map to store settings
  private class MapParameters(map: Map[Any, Any]) extends Parameters {
    protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = {
      val g = map.get(pname)
      if (g.isDefined) Some(g.get.asInstanceOf[T]) else tail.find(pname, site)
    }
  }
}
