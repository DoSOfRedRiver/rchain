package coop.rchain.syntax.rholang.Absyn; // Java Package generated by the BNF Converter.

public abstract class ValPattern implements java.io.Serializable {
  public abstract <R,A> R accept(ValPattern.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStruct p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTuple p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTrue p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtFalse p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtInt p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtDbl p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStr p, A arg);

  }

}
