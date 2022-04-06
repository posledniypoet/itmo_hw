// Generated from C:/Users/sanev/OneDrive/Рабочий стол/my/src\Haskell.g4 by ANTLR 4.9.2


package haskell;

import java.util.*;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link HaskellParser}.
 */
public interface HaskellListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link HaskellParser#file}.
	 * @param ctx the parse tree
	 */
	void enterFile(HaskellParser.FileContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#file}.
	 * @param ctx the parse tree
	 */
	void exitFile(HaskellParser.FileContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#line}.
	 * @param ctx the parse tree
	 */
	void enterLine(HaskellParser.LineContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#line}.
	 * @param ctx the parse tree
	 */
	void exitLine(HaskellParser.LineContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#func_type}.
	 * @param ctx the parse tree
	 */
	void enterFunc_type(HaskellParser.Func_typeContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#func_type}.
	 * @param ctx the parse tree
	 */
	void exitFunc_type(HaskellParser.Func_typeContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(HaskellParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(HaskellParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#type0}.
	 * @param ctx the parse tree
	 */
	void enterType0(HaskellParser.Type0Context ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#type0}.
	 * @param ctx the parse tree
	 */
	void exitType0(HaskellParser.Type0Context ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#type_cont}.
	 * @param ctx the parse tree
	 */
	void enterType_cont(HaskellParser.Type_contContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#type_cont}.
	 * @param ctx the parse tree
	 */
	void exitType_cont(HaskellParser.Type_contContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#func_dec}.
	 * @param ctx the parse tree
	 */
	void enterFunc_dec(HaskellParser.Func_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#func_dec}.
	 * @param ctx the parse tree
	 */
	void exitFunc_dec(HaskellParser.Func_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(HaskellParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(HaskellParser.ValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#func_body}.
	 * @param ctx the parse tree
	 */
	void enterFunc_body(HaskellParser.Func_bodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#func_body}.
	 * @param ctx the parse tree
	 */
	void exitFunc_body(HaskellParser.Func_bodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#newif}.
	 * @param ctx the parse tree
	 */
	void enterNewif(HaskellParser.NewifContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#newif}.
	 * @param ctx the parse tree
	 */
	void exitNewif(HaskellParser.NewifContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#ifCont}.
	 * @param ctx the parse tree
	 */
	void enterIfCont(HaskellParser.IfContContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#ifCont}.
	 * @param ctx the parse tree
	 */
	void exitIfCont(HaskellParser.IfContContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#let_in}.
	 * @param ctx the parse tree
	 */
	void enterLet_in(HaskellParser.Let_inContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#let_in}.
	 * @param ctx the parse tree
	 */
	void exitLet_in(HaskellParser.Let_inContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#if_}.
	 * @param ctx the parse tree
	 */
	void enterIf_(HaskellParser.If_Context ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#if_}.
	 * @param ctx the parse tree
	 */
	void exitIf_(HaskellParser.If_Context ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#if_block}.
	 * @param ctx the parse tree
	 */
	void enterIf_block(HaskellParser.If_blockContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#if_block}.
	 * @param ctx the parse tree
	 */
	void exitIf_block(HaskellParser.If_blockContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#elif_block}.
	 * @param ctx the parse tree
	 */
	void enterElif_block(HaskellParser.Elif_blockContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#elif_block}.
	 * @param ctx the parse tree
	 */
	void exitElif_block(HaskellParser.Elif_blockContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#math}.
	 * @param ctx the parse tree
	 */
	void enterMath(HaskellParser.MathContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#math}.
	 * @param ctx the parse tree
	 */
	void exitMath(HaskellParser.MathContext ctx);
	/**
	 * Enter a parse tree produced by {@link HaskellParser#argument}.
	 * @param ctx the parse tree
	 */
	void enterArgument(HaskellParser.ArgumentContext ctx);
	/**
	 * Exit a parse tree produced by {@link HaskellParser#argument}.
	 * @param ctx the parse tree
	 */
	void exitArgument(HaskellParser.ArgumentContext ctx);
}