package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProgramNameWithParams extends BlockCodeName {

    public Identifier procName;
    public Parameter params;

    public ProgramNameWithParams(SourcePosition srcPos, Identifier procName, Parameter params) {
        super(srcPos);
        this.procName = procName;
        this.params = params;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProgramWithParams(this, o);
    }
}
