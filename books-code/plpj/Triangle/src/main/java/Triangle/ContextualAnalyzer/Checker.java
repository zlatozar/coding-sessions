/*
 * @(#)Checker.java                        2.1 2003/10/07
 *
 * Copyright (C) 1999, 2003 D.A. Watt and D.F. Brown
 * Dept. of Computing Science, University of Glasgow, Glasgow G12 8QQ Scotland
 * and School of Computer and Math Sciences, The Robert Gordon University,
 * St. Andrew Street, Aberdeen AB25 1HG, Scotland.
 * All rights reserved.
 *
 * This software is provided free for educational use only. It may
 * not be used for commercial purposes without the prior written permission
 * of the authors.
 */

package Triangle.ContextualAnalyzer;

import Triangle.AbstractSyntaxTrees.*;
import Triangle.ErrorReporter;
import Triangle.StdEnvironment;
import Triangle.SyntacticAnalyzer.SourcePosition;

/**
 * p. 136-168
 *
 * Checks whether the source program, represented by its AST, satisfies the
 * language's scope rules and type rules.
 *
 * Also decorates the AST as follows:
 *  (a) Each applied occurrence of an identifier or operator is linked to
 *      the corresponding declaration of that identifier or operator.
 *  (b) Each expression and value-or-variable-name is decorated by its type.
 *  (c) Each type identifier is replaced by the type it denotes.
 *
 * Standard types are represented by small ASTs.
 *
 * Reports that the identifier or operator used at a leaf of the AST has not been declared.
 */
public final class Checker implements Visitor {

    // Always returns null. Does not use the given object.
    private static SourcePosition dummyPos = new SourcePosition();

    private final static Identifier dummyI = new Identifier("", dummyPos);

    private IdentificationTable indTable;
    private ErrorReporter reporter;

    public Checker(ErrorReporter reporter) {
        this.reporter = reporter;
        this.indTable = new IdentificationTable();

        establishStdEnvironment();
    }

    private static TypeDenoter checkFieldIdentifier(FieldTypeDenoter ast, Identifier I) {

        if (ast instanceof MultipleFieldTypeDenoter) {
            MultipleFieldTypeDenoter ft = (MultipleFieldTypeDenoter) ast;

            if (ft.I.spelling.compareTo(I.spelling) == 0) {
                I.decl = ast;
                return ft.T;

            } else {
                return checkFieldIdentifier(ft.FT, I);
            }

        } else if (ast instanceof SingleFieldTypeDenoter) {
            SingleFieldTypeDenoter ft = (SingleFieldTypeDenoter) ast;

            if (ft.I.spelling.compareTo(I.spelling) == 0) {
                I.decl = ast;
                return ft.T;
            }
        }

        return StdEnvironment.errorType;
    }

//_____________________________________________________________________________
//                                                                    COMMANDS

    // 1. Check that the given command is well formed
    // 2. Always returns null and does not use the given subtree(phrase) 'o'

    public Object visitAssignCommand(AssignCommand ast, Object o) {

        TypeDenoter vType = (TypeDenoter) ast.V.visit(this, null);
        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        // only variables could be assigned
        if (!ast.V.variable) {
            reporter.reportError("LHS of assignment is not a variable", "", ast.V.position);
        }

        if (!eType.equals(vType)) {
            reporter.reportError("assignment incompatibility", "", ast.position);
        }

        return null;
    }

    public Object visitCallCommand(CallCommand ast, Object o) {

        Declaration binding = (Declaration) ast.I.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.I);

        } else if (binding instanceof ProcDeclaration) {
            ast.APS.visit(this, ((ProcDeclaration) binding).FPS);

        } else if (binding instanceof ProcFormalParameter) {
            ast.APS.visit(this, ((ProcFormalParameter) binding).FPS);

        } else {
            reporter.reportError("\"%\" is not a procedure identifier",
                    ast.I.spelling, ast.I.position);
        }

        return null;
    }

    public Object visitEmptyCommand(EmptyCommand ast, Object o) {
        return null;
    }

    public Object visitIfCommand(IfCommand ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        // 'if' expression should be boolean type
        if (!eType.equals(StdEnvironment.booleanType)) {
            reporter.reportError("Boolean expression expected here", "", ast.E.position);
        }

        ast.C1.visit(this, null);
        ast.C2.visit(this, null);

        return null;
    }

    public Object visitLetCommand(LetCommand ast, Object o) {
        indTable.openScope();

        ast.D.visit(this, null);
        ast.C.visit(this, null);

        indTable.closeScope();

        return null;
    }

    public Object visitSequentialCommand(SequentialCommand ast, Object o) {
        ast.C1.visit(this, null);
        ast.C2.visit(this, null);

        return null;
    }

    public Object visitWhileCommand(WhileCommand ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        // 'while' expression should be boolean type
        if (!eType.equals(StdEnvironment.booleanType)) {
            reporter.reportError("Boolean expression expected here", "", ast.E.position);
        }

        ast.C.visit(this, null);

        return null;
    }

//_____________________________________________________________________________
//                                                                 EXPRESSIONS

    // 1. Checks that the expression is well formed
    // 2. Decorates the expression node with its inferred type
    // 3. Return that type

    public Object visitArrayExpression(ArrayExpression ast, Object o) {

        TypeDenoter elemType = (TypeDenoter) ast.AA.visit(this, null);

        IntegerLiteral il = new IntegerLiteral(new Integer(ast.AA.elemCount).toString(), ast.position);

        // V[E] could not be found(dynamic) in the AST so we create one
        ast.type = new ArrayTypeDenoter(il, elemType, ast.position);

        return ast.type;
    }

    public Object visitBinaryExpression(BinaryExpression ast, Object o) {

        TypeDenoter e1Type = (TypeDenoter) ast.E1.visit(this, null);
        TypeDenoter e2Type = (TypeDenoter) ast.E2.visit(this, null);

        // return pointer to the 'declaration' of O so
        Declaration binding = (Declaration) ast.O.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.O);

        } else {

            if (!(binding instanceof BinaryOperatorDeclaration)) {
                reporter.reportError("\"%\" is not a binary operator", ast.O.spelling, ast.O.position);
            }

            BinaryOperatorDeclaration bbinding = (BinaryOperatorDeclaration) binding;

            if (bbinding.ARG1 == StdEnvironment.anyType) {

                // this operator must be "=" or "\="
                if (!e1Type.equals(e2Type)) {
                    reporter.reportError("incompatible argument types for \"%\"", ast.O.spelling, ast.position);
                }

            } else if (!e1Type.equals(bbinding.ARG1)) {
                reporter.reportError("wrong argument type for \"%\"", ast.O.spelling, ast.E1.position);

            } else if (!e2Type.equals(bbinding.ARG2)) {
                reporter.reportError("wrong argument type for \"%\"", ast.O.spelling, ast.E2.position);
            }

            ast.type = bbinding.RES;
        }

        return ast.type;
    }

    public Object visitCallExpression(CallExpression ast, Object o) {

        Declaration binding = (Declaration) ast.I.visit(this, null);

        // NOTE that the subtree is passed when visit ActualParameterSequence (APS)

        if (binding == null) {
            reportUndeclared(ast.I);
            ast.type = StdEnvironment.errorType;

        } else if (binding instanceof FuncDeclaration) {
            ast.APS.visit(this, ((FuncDeclaration) binding).FPS);
            ast.type = ((FuncDeclaration) binding).T;

        } else if (binding instanceof FuncFormalParameter) {
            ast.APS.visit(this, ((FuncFormalParameter) binding).FPS);
            ast.type = ((FuncFormalParameter) binding).T;

        } else {
            reporter.reportError("\"%\" is not a function identifier", ast.I.spelling, ast.I.position);
        }

        return ast.type;
    }

    public Object visitCharacterExpression(CharacterExpression ast, Object o) {
        ast.type = StdEnvironment.charType;

        return ast.type;
    }

    public Object visitEmptyExpression(EmptyExpression ast, Object o) {
        ast.type = null;

        return ast.type;
    }

    public Object visitIfExpression(IfExpression ast, Object o) {

        TypeDenoter e1Type = (TypeDenoter) ast.E1.visit(this, null);

        if (!e1Type.equals(StdEnvironment.booleanType)) {
            reporter.reportError("Boolean expression expected here", "", ast.E1.position);
        }

        TypeDenoter e2Type = (TypeDenoter) ast.E2.visit(this, null);
        TypeDenoter e3Type = (TypeDenoter) ast.E3.visit(this, null);

        if (!e2Type.equals(e3Type)) {
            reporter.reportError("incompatible limbs in if-expression", "", ast.position);
        }

        // e2Type is equal to e3Type so take one of them
        ast.type = e2Type;

        return ast.type;
    }

    public Object visitIntegerExpression(IntegerExpression ast, Object o) {
        ast.type = StdEnvironment.integerType;

        return ast.type;
    }

    public Object visitLetExpression(LetExpression ast, Object o) {
        indTable.openScope();

        ast.D.visit(this, null);
        ast.type = (TypeDenoter) ast.E.visit(this, null);

        indTable.closeScope();

        return ast.type;
    }

    public Object visitRecordExpression(RecordExpression ast, Object o) {

        FieldTypeDenoter rType = (FieldTypeDenoter) ast.RA.visit(this, null);
        ast.type = new RecordTypeDenoter(rType, ast.position);

        return ast.type;
    }

    public Object visitUnaryExpression(UnaryExpression ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        Declaration binding = (Declaration) ast.O.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.O);
            ast.type = StdEnvironment.errorType;

        } else if (!(binding instanceof UnaryOperatorDeclaration)) {
            reporter.reportError("\"%\" is not a unary operator", ast.O.spelling, ast.O.position);

        } else {
            UnaryOperatorDeclaration ubinding = (UnaryOperatorDeclaration) binding;

            if (!eType.equals(ubinding.ARG)) {
                reporter.reportError("wrong argument type for \"%\"", ast.O.spelling, ast.O.position);
            }

            ast.type = ubinding.RES;
        }
        return ast.type;
    }

    public Object visitVnameExpression(VnameExpression ast, Object o) {

        // Expression return types so cast
        ast.type = (TypeDenoter) ast.V.visit(this, null);

        return ast.type;
    }

//_____________________________________________________________________________
//                                                                DECLARATIONS

    // 1. Always returns null and does not use the given subtree(phrase) 'o'.
    // 2. Enters all declared identifiers into the identification table

    public Object visitBinaryOperatorDeclaration(BinaryOperatorDeclaration ast, Object o) {
        return null;
    }

    public Object visitConstDeclaration(ConstDeclaration ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        return null;
    }

    public Object visitFuncDeclaration(FuncDeclaration ast, Object o) {

        // eliminate type identifiers
        ast.T = (TypeDenoter) ast.T.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        indTable.openScope();

        ast.FPS.visit(this, null);
        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        indTable.closeScope();

        if (!ast.T.equals(eType)) {
            reporter.reportError("body of function \"%\" has wrong type", ast.I.spelling, ast.E.position);
        }

        return null;
    }

    public Object visitProcDeclaration(ProcDeclaration ast, Object o) {

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        indTable.openScope();

        ast.FPS.visit(this, null);
        ast.C.visit(this, null);

        indTable.closeScope();

        return null;
    }

    public Object visitSequentialDeclaration(SequentialDeclaration ast, Object o) {

        ast.D1.visit(this, null);
        ast.D2.visit(this, null);

        return null;
    }

    public Object visitTypeDeclaration(TypeDeclaration ast, Object o) {

        ast.T = (TypeDenoter) ast.T.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        return null;
    }

    // Always returns null
    public Object visitUnaryOperatorDeclaration(UnaryOperatorDeclaration ast, Object o) {
        return null;
    }

    public Object visitVarDeclaration(VarDeclaration ast, Object o) {

        // eliminate type identifiers
        ast.T = (TypeDenoter) ast.T.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        return null;
    }

//_____________________________________________________________________________
//                                                            ARRAY AGGREGATES

    public Object visitMultipleArrayAggregate(MultipleArrayAggregate ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);
        TypeDenoter elemType = (TypeDenoter) ast.AA.visit(this, null);

        ast.elemCount = ast.AA.elemCount + 1;

        if (!eType.equals(elemType)) {
            reporter.reportError("incompatible array-aggregate element", "", ast.E.position);
        }

        return elemType;
    }

    public Object visitSingleArrayAggregate(SingleArrayAggregate ast, Object o) {

        TypeDenoter elemType = (TypeDenoter) ast.E.visit(this, null);

        ast.elemCount = 1;  // decorate

        return elemType;
    }

    public Object visitMultipleRecordAggregate(MultipleRecordAggregate ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);
        FieldTypeDenoter rType = (FieldTypeDenoter) ast.RA.visit(this, null);

        // !!!
        TypeDenoter fType = checkFieldIdentifier(rType, ast.I);

        if (fType != StdEnvironment.errorType) {
            reporter.reportError("duplicate field \"%\" in record", ast.I.spelling, ast.I.position);
        }

        // ???
        ast.type = new MultipleFieldTypeDenoter(ast.I, eType, rType, ast.position);

        return ast.type;
    }

    public Object visitSingleRecordAggregate(SingleRecordAggregate ast, Object o) {

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);
        ast.type = new SingleFieldTypeDenoter(ast.I, eType, ast.position);

        return ast.type;
    }

//_____________________________________________________________________________
//                                                           FORMAL PARAMETERS

    public Object visitConstFormalParameter(ConstFormalParameter ast, Object o) {

        ast.T = (TypeDenoter) ast.T.visit(this, null);
        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("duplicated formal parameter \"%\"", ast.I.spelling, ast.position);
        }

        return null;
    }

    public Object visitFuncFormalParameter(FuncFormalParameter ast, Object o) {

        indTable.openScope();

        ast.FPS.visit(this, null);

        indTable.closeScope();

        ast.T = (TypeDenoter) ast.T.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("duplicated formal parameter \"%\"", ast.I.spelling, ast.position);
        }

        return null;
    }

    public Object visitProcFormalParameter(ProcFormalParameter ast, Object o) {

        indTable.openScope();

        ast.FPS.visit(this, null);

        indTable.closeScope();

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("duplicated formal parameter \"%\"", ast.I.spelling, ast.position);
        }

        return null;
    }

    public Object visitVarFormalParameter(VarFormalParameter ast, Object o) {

        ast.T = (TypeDenoter) ast.T.visit(this, null);

        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("duplicated formal parameter \"%\"", ast.I.spelling, ast.position);
        }

        return null;
    }

    public Object visitEmptyFormalParameterSequence(EmptyFormalParameterSequence ast, Object o) {
        return null;
    }

    public Object visitMultipleFormalParameterSequence(MultipleFormalParameterSequence ast, Object o) {
        ast.FP.visit(this, null);
        ast.FPS.visit(this, null);

        return null;
    }

    public Object visitSingleFormalParameterSequence(SingleFormalParameterSequence ast, Object o) {
        ast.FP.visit(this, null);

        return null;
    }

//_____________________________________________________________________________
//                                                           ACTUAL PARAMETERS


    public Object visitConstActualParameter(ConstActualParameter ast, Object o) {

        // use passed FormalParameter
        FormalParameter fp = (FormalParameter) o;

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        if (!(fp instanceof ConstFormalParameter)) {
            reporter.reportError("const actual parameter not expected here", "", ast.position);

        } else if (!eType.equals(((ConstFormalParameter) fp).T)) {
            reporter.reportError("wrong type for const actual parameter", "", ast.E.position);
        }

        return null;
    }

    public Object visitFuncActualParameter(FuncActualParameter ast, Object o) {

        FormalParameter fp = (FormalParameter) o;

        Declaration binding = (Declaration) ast.I.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.I);

        } else if (!(binding instanceof FuncDeclaration || binding instanceof FuncFormalParameter)) {
            reporter.reportError("\"%\" is not a function identifier", ast.I.spelling, ast.I.position);

        } else if (!(fp instanceof FuncFormalParameter)) {
            reporter.reportError("func actual parameter not expected here", "", ast.position);

        } else {

            FormalParameterSequence FPS;
            TypeDenoter T;

            if (binding instanceof FuncDeclaration) {
                FPS = ((FuncDeclaration) binding).FPS;
                T = ((FuncDeclaration) binding).T;

            } else {
                FPS = ((FuncFormalParameter) binding).FPS;
                T = ((FuncFormalParameter) binding).T;
            }

            if (!FPS.equals(((FuncFormalParameter) fp).FPS)) {
                reporter.reportError("wrong signature for function \"%\"", ast.I.spelling, ast.I.position);

            } else if (!T.equals(((FuncFormalParameter) fp).T)) {
                reporter.reportError("wrong type for function \"%\"", ast.I.spelling, ast.I.position);

            }
        }

        return null;
    }

    public Object visitProcActualParameter(ProcActualParameter ast, Object o) {

        // use passed FormalParameter
        FormalParameter fp = (FormalParameter) o;

        Declaration binding = (Declaration) ast.I.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.I);

        } else if (!(binding instanceof ProcDeclaration || binding instanceof ProcFormalParameter)) {
            reporter.reportError("\"%\" is not a procedure identifier", ast.I.spelling, ast.I.position);

        } else if (!(fp instanceof ProcFormalParameter)) {
            reporter.reportError("proc actual parameter not expected here", "", ast.position);

        } else {

            FormalParameterSequence FPS;

            if (binding instanceof ProcDeclaration) {
                FPS = ((ProcDeclaration) binding).FPS;

            } else {
                FPS = ((ProcFormalParameter) binding).FPS;
            }

            if (!FPS.equals(((ProcFormalParameter) fp).FPS)) {
                reporter.reportError("wrong signature for procedure \"%\"", ast.I.spelling, ast.I.position);
            }
        }

        return null;
    }

    public Object visitVarActualParameter(VarActualParameter ast, Object o) {

        // Use passed FormalParameter
        FormalParameter fp = (FormalParameter) o;

        TypeDenoter vType = (TypeDenoter) ast.V.visit(this, null);

        if (!ast.V.variable) {
            reporter.reportError("actual parameter is not a variable", "", ast.V.position);

        } else if (!(fp instanceof VarFormalParameter)) {
            reporter.reportError("var actual parameter not expected here", "", ast.V.position);

        } else if (!vType.equals(((VarFormalParameter) fp).T)) {
            reporter.reportError("wrong type for var actual parameter", "", ast.V.position);
        }

        return null;
    }

    public Object visitEmptyActualParameterSequence(EmptyActualParameterSequence ast, Object o) {

        // use passed FormalParameterSequence
        FormalParameterSequence fps = (FormalParameterSequence) o;

        if (!(fps instanceof EmptyFormalParameterSequence)) {
            reporter.reportError("too few actual parameters", "", ast.position);
        }

        return null;
    }

    public Object visitMultipleActualParameterSequence(MultipleActualParameterSequence ast, Object o) {

        // use passed FormalParameterSequence
        FormalParameterSequence fps = (FormalParameterSequence) o;

        if (!(fps instanceof MultipleFormalParameterSequence)) {
            reporter.reportError("too many actual parameters", "", ast.position);

        } else {
            ast.AP.visit(this, ((MultipleFormalParameterSequence) fps).FP);
            ast.APS.visit(this, ((MultipleFormalParameterSequence) fps).FPS);
        }

        return null;
    }

    public Object visitSingleActualParameterSequence(SingleActualParameterSequence ast, Object o) {

        // use passed FormalParameterSequence
        FormalParameterSequence fps = (FormalParameterSequence) o;

        if (!(fps instanceof SingleFormalParameterSequence)) {
            reporter.reportError("incorrect number of actual parameters", "", ast.position);

        } else {
            ast.AP.visit(this, ((SingleFormalParameterSequence) fps).FP);
        }

        return null;
    }

//_____________________________________________________________________________
//                                                               TYPE DENOTERS


    public Object visitAnyTypeDenoter(AnyTypeDenoter ast, Object o) {
        return StdEnvironment.anyType;
    }

    public Object visitArrayTypeDenoter(ArrayTypeDenoter ast, Object o) {

        ast.T = (TypeDenoter) ast.T.visit(this, null);

        if ((Integer.valueOf(ast.IL.spelling).intValue()) == 0) {
            reporter.reportError("arrays must not be empty", "", ast.IL.position);
        }

        return ast;
    }

    public Object visitBoolTypeDenoter(BoolTypeDenoter ast, Object o) {
        return StdEnvironment.booleanType;
    }

    public Object visitCharTypeDenoter(CharTypeDenoter ast, Object o) {
        return StdEnvironment.charType;
    }

    public Object visitErrorTypeDenoter(ErrorTypeDenoter ast, Object o) {
        return StdEnvironment.errorType;
    }

    public Object visitSimpleTypeDenoter(SimpleTypeDenoter ast, Object o) {

        Declaration binding = (Declaration) ast.I.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.I);

            return StdEnvironment.errorType;

        } else if (!(binding instanceof TypeDeclaration)) {
            reporter.reportError("\"%\" is not a type identifier", ast.I.spelling, ast.I.position);

            return StdEnvironment.errorType;
        }

        return ((TypeDeclaration) binding).T;
    }

    public Object visitIntTypeDenoter(IntTypeDenoter ast, Object o) {
        return StdEnvironment.integerType;
    }

    public Object visitRecordTypeDenoter(RecordTypeDenoter ast, Object o) {
        ast.FT = (FieldTypeDenoter) ast.FT.visit(this, null);

        return ast;
    }

    // ???
    public Object visitMultipleFieldTypeDenoter(MultipleFieldTypeDenoter ast, Object o) {

        ast.T = (TypeDenoter) ast.T.visit(this, null);
        ast.FT.visit(this, null);

        return ast;
    }

    public Object visitSingleFieldTypeDenoter(SingleFieldTypeDenoter ast, Object o) {
        ast.T = (TypeDenoter) ast.T.visit(this, null);
        return ast;
    }

//_____________________________________________________________________________
//                                         Literals, Identifiers and Operators

    public Object visitCharacterLiteral(CharacterLiteral CL, Object o) {
        return StdEnvironment.charType;
    }

    /**
     * Links an applied occurrence of an identifier to the corresponding declaration (if any).
     * Its result is a pointer to that declaration.
     */
    public Object visitIdentifier(Identifier I, Object o) {
        Declaration binding = indTable.retrieve(I.spelling);

        if (binding != null) {
            I.decl = binding;
        }

        return binding;
    }

    public Object visitIntegerLiteral(IntegerLiteral IL, Object o) {
        return StdEnvironment.integerType;
    }

    public Object visitOperator(Operator O, Object o) {
        // Binary or Unary Operators could be declared above
        Declaration binding = indTable.retrieve(O.spelling);

        if (binding != null) {
            O.decl = binding;
        }

        return binding;
    }

//_____________________________________________________________________________
//                                                      VALUE or VARIABLE NAMES

    // 1. Checks that the value-or-variable-name is well-formed
    // 2. Decorates it with its inferred type
    // 3. Add indication of whether it is a variable or not
    // 4. The method's result is the inferred type

    public Object visitDotVname(DotVname ast, Object o) {
        ast.type = null;

        TypeDenoter vType = (TypeDenoter) ast.V.visit(this, null);

        ast.variable = ast.V.variable;

        if (!(vType instanceof RecordTypeDenoter)) {
            reporter.reportError("record expected here", "", ast.V.position);

        } else {
            ast.type = checkFieldIdentifier(((RecordTypeDenoter) vType).FT, ast.I);

            if (ast.type == StdEnvironment.errorType) {
                reporter.reportError("no field \"%\" in this record type", ast.I.spelling, ast.I.position);
            }
        }

        return ast.type;
    }

    public Object visitSimpleVname(SimpleVname ast, Object o) {
        ast.variable = false;
        ast.type = StdEnvironment.errorType;

        Declaration binding = (Declaration) ast.I.visit(this, null);

        if (binding == null) {
            reportUndeclared(ast.I);

        } else if (binding instanceof ConstDeclaration) {
            ast.type = ((ConstDeclaration) binding).E.type;
            ast.variable = false;

        } else if (binding instanceof VarDeclaration) {
            ast.type = ((VarDeclaration) binding).T;
            ast.variable = true;

        } else if (binding instanceof ConstFormalParameter) {
            ast.type = ((ConstFormalParameter) binding).T;
            ast.variable = false;

        } else if (binding instanceof VarFormalParameter) {
            ast.type = ((VarFormalParameter) binding).T;
            ast.variable = true;

        } else {
            reporter.reportError("\"%\" is not a const or var identifier", ast.I.spelling, ast.I.position);
        }

        return ast.type;
    }

    public Object visitSubscriptVname(SubscriptVname ast, Object o) {

        TypeDenoter vType = (TypeDenoter) ast.V.visit(this, null);
        ast.variable = ast.V.variable;

        TypeDenoter eType = (TypeDenoter) ast.E.visit(this, null);

        if (vType != StdEnvironment.errorType) {

            if (!(vType instanceof ArrayTypeDenoter)) {
                reporter.reportError("array expected here", "", ast.V.position);

            } else {

                if (!eType.equals(StdEnvironment.integerType)) {
                    reporter.reportError("Integer expression expected here", "", ast.E.position);
                }

                ast.type = ((ArrayTypeDenoter) vType).T;
            }
        }

        return ast.type;
    }

//_____________________________________________________________________________
//                                                                    PROGRAMS

    public Object visitProgram(Program ast, Object o) {
        ast.C.visit(this, null);

        return null;
    }

    // Checker entry point
    public void check(Program ast) {
        ast.visit(this, null);
    }

    private void reportUndeclared(Terminal leaf) {
        reporter.reportError("\"%\" is not declared", leaf.spelling, leaf.position);
    }

//_____________________________________________________________________________
//                                                             STD ENVIRONMENT

    // Creates 'small' ASTs to represent the standard types.
    //
    // Creates small ASTs to represent "declarations" of standard types, constants, procedures, functions, and operators.
    // Enters these "declarations" in the identification table.
    //
    // This "declaration" summarises the operator's type info.

    private TypeDeclaration declareStdType(String id, TypeDenoter typedenoter) {

        TypeDeclaration binding;

        binding = new TypeDeclaration(new Identifier(id, dummyPos), typedenoter, dummyPos);
        indTable.enter(id, binding);

        return binding;
    }

    private ConstDeclaration declareStdConst(String id, TypeDenoter constType) {

        IntegerExpression constExpr;
        ConstDeclaration binding;

        // constExpr used only as a placeholder for constType
        constExpr = new IntegerExpression(null, dummyPos);
        constExpr.type = constType;

        binding = new ConstDeclaration(new Identifier(id, dummyPos), constExpr, dummyPos);

        indTable.enter(id, binding);

        return binding;
    }

    private ProcDeclaration declareStdProc(String id, FormalParameterSequence fps) {

        ProcDeclaration binding;

        binding = new ProcDeclaration(new Identifier(id, dummyPos), fps, new EmptyCommand(dummyPos), dummyPos);

        indTable.enter(id, binding);
        return binding;
    }


    private FuncDeclaration declareStdFunc(String id, FormalParameterSequence fps, TypeDenoter resultType) {

        FuncDeclaration binding;

        binding = new FuncDeclaration(new Identifier(id, dummyPos), fps, resultType, new EmptyExpression(dummyPos), dummyPos);
        indTable.enter(id, binding);

        return binding;
    }

    private UnaryOperatorDeclaration declareStdUnaryOp(String op, TypeDenoter argType, TypeDenoter resultType) {

        UnaryOperatorDeclaration binding;

        binding = new UnaryOperatorDeclaration(new Operator(op, dummyPos), argType, resultType, dummyPos);
        indTable.enter(op, binding);

        return binding;
    }

    private BinaryOperatorDeclaration declareStdBinaryOp(String op, TypeDenoter arg1Type, TypeDenoter arg2type,
                                                         TypeDenoter resultType) {

        BinaryOperatorDeclaration binding;

        binding = new BinaryOperatorDeclaration(new Operator(op, dummyPos), arg1Type, arg2type, resultType, dummyPos);
        indTable.enter(op, binding);

        return binding;
    }

    private void establishStdEnvironment() {

        // idTable.startIdentification();
        StdEnvironment.booleanType = new BoolTypeDenoter(dummyPos);
        StdEnvironment.integerType = new IntTypeDenoter(dummyPos);
        StdEnvironment.charType = new CharTypeDenoter(dummyPos);
        StdEnvironment.anyType = new AnyTypeDenoter(dummyPos);
        StdEnvironment.errorType = new ErrorTypeDenoter(dummyPos);

        StdEnvironment.booleanDecl = declareStdType("Boolean", StdEnvironment.booleanType);
        StdEnvironment.falseDecl = declareStdConst("false", StdEnvironment.booleanType);
        StdEnvironment.trueDecl = declareStdConst("true", StdEnvironment.booleanType);
        StdEnvironment.notDecl = declareStdUnaryOp("\\", StdEnvironment.booleanType, StdEnvironment.booleanType);
        StdEnvironment.andDecl = declareStdBinaryOp("/\\", StdEnvironment.booleanType, StdEnvironment.booleanType, StdEnvironment.booleanType);
        StdEnvironment.orDecl = declareStdBinaryOp("\\/", StdEnvironment.booleanType, StdEnvironment.booleanType, StdEnvironment.booleanType);

        StdEnvironment.integerDecl = declareStdType("Integer", StdEnvironment.integerType);
        StdEnvironment.maxintDecl = declareStdConst("maxint", StdEnvironment.integerType);
        StdEnvironment.addDecl = declareStdBinaryOp("+", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.subtractDecl = declareStdBinaryOp("-", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.multiplyDecl = declareStdBinaryOp("*", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.divideDecl = declareStdBinaryOp("/", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.moduloDecl = declareStdBinaryOp("//", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.lessDecl = declareStdBinaryOp("<", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.notgreaterDecl = declareStdBinaryOp("<=", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.greaterDecl = declareStdBinaryOp(">", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.notlessDecl = declareStdBinaryOp(">=", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);

        StdEnvironment.charDecl = declareStdType("Char", StdEnvironment.charType);
        StdEnvironment.chrDecl = declareStdFunc("chr", new SingleFormalParameterSequence(new ConstFormalParameter(dummyI, StdEnvironment.integerType, dummyPos), dummyPos), StdEnvironment.charType);
        StdEnvironment.ordDecl = declareStdFunc("ord", new SingleFormalParameterSequence(new ConstFormalParameter(dummyI, StdEnvironment.charType, dummyPos), dummyPos), StdEnvironment.integerType);
        StdEnvironment.eofDecl = declareStdFunc("eof", new EmptyFormalParameterSequence(dummyPos), StdEnvironment.booleanType);
        StdEnvironment.eolDecl = declareStdFunc("eol", new EmptyFormalParameterSequence(dummyPos), StdEnvironment.booleanType);
        StdEnvironment.getDecl = declareStdProc("get", new SingleFormalParameterSequence(new VarFormalParameter(dummyI, StdEnvironment.charType, dummyPos), dummyPos));
        StdEnvironment.putDecl = declareStdProc("put", new SingleFormalParameterSequence(new ConstFormalParameter(dummyI, StdEnvironment.charType, dummyPos), dummyPos));
        StdEnvironment.getintDecl = declareStdProc("getint", new SingleFormalParameterSequence(new VarFormalParameter(dummyI, StdEnvironment.integerType, dummyPos), dummyPos));
        StdEnvironment.putintDecl = declareStdProc("putint", new SingleFormalParameterSequence(new ConstFormalParameter(dummyI, StdEnvironment.integerType, dummyPos), dummyPos));
        StdEnvironment.geteolDecl = declareStdProc("geteol", new EmptyFormalParameterSequence(dummyPos));
        StdEnvironment.puteolDecl = declareStdProc("puteol", new EmptyFormalParameterSequence(dummyPos));
        StdEnvironment.equalDecl = declareStdBinaryOp("=", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);
        StdEnvironment.unequalDecl = declareStdBinaryOp("\\=", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);

    }
}
