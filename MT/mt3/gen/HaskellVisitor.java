// Generated from C:/Users/sanev/OneDrive/Рабочий стол/my/src\Haskell.g4 by ANTLR 4.9.2


package haskell;

import java.util.*;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link HaskellParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface HaskellVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link HaskellParser#file}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFile(HaskellParser.FileContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#line}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLine(HaskellParser.LineContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#func_type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunc_type(HaskellParser.Func_typeContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType(HaskellParser.TypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#type0}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType0(HaskellParser.Type0Context ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#type_cont}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType_cont(HaskellParser.Type_contContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#func_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunc_dec(HaskellParser.Func_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValue(HaskellParser.ValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#func_body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunc_body(HaskellParser.Func_bodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#newif}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNewif(HaskellParser.NewifContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#ifCont}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfCont(HaskellParser.IfContContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#let_in}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLet_in(HaskellParser.Let_inContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#if_}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIf_(HaskellParser.If_Context ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#if_block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIf_block(HaskellParser.If_blockContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#elif_block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElif_block(HaskellParser.Elif_blockContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMath(HaskellParser.MathContext ctx);
	/**
	 * Visit a parse tree produced by {@link HaskellParser#argument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgument(HaskellParser.ArgumentContext ctx);
}