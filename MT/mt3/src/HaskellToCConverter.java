import haskell.HaskellBaseVisitor;
import haskell.HaskellParser;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class HaskellToCConverter extends HaskellBaseVisitor<String> {

    public static final String DELIMITER = ", ";

    private static class FunctionWithDeclarations {
        String name;
        HaskellParser.Func_typeContext typeInfo;
        List<HaskellParser.Func_decContext> declarations;


        public FunctionWithDeclarations(String name) {
            this.name = name;
            this.declarations = new ArrayList<>();
        }

        public void setTypeInfo(HaskellParser.Func_typeContext typeInfo) {
            this.typeInfo = typeInfo;
        }
    }


    @Override
    public String visitFile(HaskellParser.FileContext ctx) {
        if (ctx == null) {
            return "";
        }
        List<HaskellParser.LineContext> lines = ctx.line();
        if (lines == null || lines.isEmpty()) {
            return "";
        }
        Map<String, FunctionWithDeclarations> funcToInfo = new HashMap<>();

        StringBuilder sb = new StringBuilder();
        for (var line : lines) {
            var funType = line.func_type();

            if (funType != null) {
                var funName = funType.NAME();
                String name = funName.getText();
                funcToInfo.putIfAbsent(name, new FunctionWithDeclarations(name));
                funcToInfo.get(name).setTypeInfo(funType);
            } else {
                var func_dec = line.func_dec();
                var funName = func_dec.NAME();
                String name = funName.getText();
                funcToInfo.putIfAbsent(name, new FunctionWithDeclarations(name));
                funcToInfo.get(name).declarations.add(func_dec);
            }
        }

        for (Map.Entry<String, FunctionWithDeclarations> entry : funcToInfo.entrySet()) {
            if (entry.getValue().declarations.isEmpty()) {
                throw new IllegalArgumentException("no declarations");
            } else if (entry.getValue().typeInfo == null) {
                throw new IllegalArgumentException("no type");
            }
            sb.append("static ")
                    .append(visitFunc_type(entry.getValue().typeInfo))
                    .append(" {\n");

            StringBuilder declaration = new StringBuilder();
            for (var dec : entry.getValue().declarations) {
                declaration.append(visitFunc_dec(dec));
                declaration.append('\n');
            }
            sb.append(shiftString(declaration.toString()));
            sb.append("\n");
            sb.append("}\n");
        }
        return sb.toString();
    }


    @Override
    public String visitLine(HaskellParser.LineContext ctx) {
        return super.visitLine(ctx);
    }


    @Override
    public String visitFunc_type(HaskellParser.Func_typeContext ctx) {
        List<String> listOfTypes = getListOfTypes(ctx.type());
        String returnValue = listOfTypes.remove(listOfTypes.size() - 1);
        return returnValue +
                " " +
                ctx.NAME().getText() +
                "(" +
                IntStream.range(0, listOfTypes.size())
                        .mapToObj(i -> listOfTypes.get(i) + " v" + i)
                        .collect(Collectors.joining(DELIMITER)) +
                ")";
    }


    @Override
    public String visitType(HaskellParser.TypeContext ctx) {
        List<String> listOfTypes = getListOfTypes(ctx);
        return toFunctionType(listOfTypes);
    }

    private List<String> getListOfTypes(HaskellParser.TypeContext ctx) {
        String t1 =  visitType0(ctx.type0());
        List<String> listOfTypes = new ArrayList<>();
        listOfTypes.add(t1);
        var t_cont = ctx.type_cont();
        while (!t_cont.isEmpty()) {
            var newCtx = t_cont.type();
            if (newCtx == null) {
                break;
            }
            listOfTypes.add(visitType0(newCtx.type0()));
            t_cont = newCtx.type_cont();
        }
        return listOfTypes;
    }


    @Override
    public String visitType0(HaskellParser.Type0Context ctx) {
        var type0 = ctx.TYPE();
        if (type0 != null) {
            return type0.getText();
        } else {
            return visitType(ctx.type());
        }
    }


    @Override
    public String visitType_cont(HaskellParser.Type_contContext ctx) {
        // realized
        return super.visitType_cont(ctx);
    }


    @Override
    public String visitFunc_dec(HaskellParser.Func_decContext ctx) {
        StringBuilder sb = new StringBuilder();

        List<HaskellParser.ValueContext> vars = ctx.value();
        List<String> conditions = new ArrayList<>();

        StringBuilder replacings = new StringBuilder();
        int index = 0;
        for (var v : vars) {
            String vString = visitValue(v);
            if (!isInt(vString)) {
                replacings.append("var ")
                        .append(vString)
                        .append(" = ")
                        .append("v")
                        .append(index)
                        .append(";\n");
            } else {
                conditions.add("v" + index + " == " + vString);
            }
            index++;
        }
        if (!conditions.isEmpty()) {
            sb.append("if(")
                    .append(String.join(" & ", conditions))
                    .append(") {\n\t");
            sb.append(replacings.toString().replaceAll("\n", "\n\t"));
            String body = visitFunc_body(ctx.func_body()).replaceAll("\n", "\n\t");
            sb.append(body);
            sb.append('\n');
            return sb.append('}').toString();
        } else {
            sb.append(replacings);
            sb.append(visitFunc_body(ctx.func_body()));
            return sb.toString();
        }
    }


    @Override
    public String visitValue(HaskellParser.ValueContext ctx) {
        var t = ctx.INT();
        if (t != null) {
            return t.getText();
        } else {
            return ctx.NAME().getText();
        }
    }


    @Override
    public String visitFunc_body(HaskellParser.Func_bodyContext ctx) {
        var math = ctx.math();
        if (math != null) {
            return "return " + visitMath(math) + ";";
        }
        var let = ctx.let_in();
        if (let != null) {
            return visitLet_in(let);
        }
        var if_ = ctx.if_();
        if (if_ != null) {
            return visitIf_(if_);
        }
        return super.visitFunc_body(ctx);
    }


    @Override
    public String visitLet_in(HaskellParser.Let_inContext ctx) {
        String newVar = ctx.NAME().getText();
        List<HaskellParser.MathContext> mathExprs = ctx.math();
        String varInit = visitMath(mathExprs.get(0));

        StringBuilder sb = new StringBuilder();
        sb.append("var ")
                .append(newVar)
                .append(" = ")
                .append(varInit)
                .append(";\n");

        if (mathExprs.size() > 1) {
            sb.append("return ")
                    .append(visitMath(mathExprs.get(1)))
                    .append(";");
        } else {
            sb.append(visitLet_in(ctx.let_in()));
        }
        return sb.toString();
    }


    @Override
    public String visitIf_(HaskellParser.If_Context ctx) {
        return getString(ctx.if_block(), ctx.elif_block());
    }

    private String getString(HaskellParser.If_blockContext if_blockContext,
                             HaskellParser.Elif_blockContext elif_blockContext) {
        StringBuilder sb = new StringBuilder();

        sb.append(visitIf_block(if_blockContext));

        if (elif_blockContext != null) {

            String elif_cont = visitElif_block(elif_blockContext);

            if (!elif_cont.isBlank()) {
                sb.append("else\n");
                sb.append(elif_cont);
            } else {
                sb.append("else { return null; }\n");
            }
        }
        return sb.toString();
    }


    @Override
    public String visitIf_block(HaskellParser.If_blockContext ctx) {
        StringBuilder sb = new StringBuilder();

        List<HaskellParser.MathContext> mathExprs = ctx.math();

        sb.append("if(")
                .append(visitMath(mathExprs.get(0)))
                .append(") {\n");

        String body = "";
        if (mathExprs.size() > 1) {
            body += "return " + visitMath(mathExprs.get(1)) + ";";
        } else {
            body += visitLet_in(ctx.let_in());
        }

        sb.append(shiftString(body));
        sb.append("\n}\n");

        return sb.toString();
    }


    @Override
    public String visitElif_block(HaskellParser.Elif_blockContext ctx) {
        var if_first = ctx.if_block();
        if (if_first == null) {
            return "";
        }
        return getString(if_first, ctx.elif_block());
    }


    @Override
    public String visitMath(HaskellParser.MathContext ctx) {
        var mathExprs = ctx.math();
        if (mathExprs.size() == 1) {
            return "!" + visitMath(mathExprs.get(0));
        } else if (mathExprs.size() > 1) {
            String operator =  ctx.getChild(1).getText();
            return visitMath(mathExprs.get(0)) +
                    " " + operator + " " +
                    visitMath(mathExprs.get(1));
        }
        var arg = ctx.argument();
        if (arg != null) {
            return visitArgument(arg);
        }
        return super.visitMath(ctx);
    }


    @Override
    public String visitArgument(HaskellParser.ArgumentContext ctx) {
        var t = ctx.INT();
        if (t != null) {
            return t.getText();
        }
        t = ctx.NAME();
        List<HaskellParser.MathContext> args = ctx.math();
        if (t != null) {
            if (args == null || args.isEmpty()) {
                return t.getText();
            }
            return t.getText() +
                    "(" +
                    args.stream()
                            .map(this::visitMath)
                            .collect(Collectors.joining(",")) +
                    ")";
        }
        return "(" + visitMath(args.get(0)) + ")";
    }

    private String toFunctionType(List<String> args) {
        return "func<" + String.join(DELIMITER, args) + ">";
    }

    private boolean isInt(String s) {
        return s.matches("\\d+");
    }

    public static String shiftString(String s) {
        return ('\t' + s.replaceAll("\n", "\n\t"));
    }
}
