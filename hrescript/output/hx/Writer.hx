/*
 * Copyright (c) 2008, Nicolas Cannasse, 'hrescript contributors'
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package hrescript.output.hx;

import hrescript.input.hx.Expr;
import hrescript.input.hx.Parser;
import hrescript.output.hx.WriteUtil;

enum Content {
	Tok( s : String );
	Indent( count : Int );
	White( e : Expr, prop : String );
	Semi;
	Newline;
}

class Writer {
	
	var out : Array<String>;
	var indent : Int;
	var noSemiExpr : Hash<Bool>;
	var comments : Hash<TComment>;
	var tokIndx : Int;
	
	public function new() {
		init();
	}
	
	private function init() {
		out = new Array();
		indent = tokIndx = 0;
		noSemiExpr = new Hash();
		var noSemiArr = [
			"ETypeDef", "EEnum", "EBlock", "EIf", "ETyped", "EFunction", 
			"EModifier", "ETry", "ECond", "ECondIf", "ECondElse", "ECondElseif",
			"EFor", "EClass", "EInterface", "ESwitch", "ECase", "EDefault",
			"EWhile"
		];
		for( e in noSemiArr )
			noSemiExpr.set(e,true);
	}
	
	public function parse( eArr : Array<Expr>, comments : Hash<TComment> ) {
		this.comments = comments;
		var huh : Array<Content> = doExprArr( eArr, {} );
		//out = doExprArr( eArr, {} );
		//trace("out = \n" + Std.string(out));
		var wu = WriteUtil.getInstance();
		var out = wu.explode( huh, comments );
		//trace("out = \n" + Std.string(out));
		return out;//.join("");
	}
	
	private function flatten( a : Array<Content> ) :Array<String> {
		var out = new Array<String>();
		for( i in a )
			out.push( Std.string(i) );
		return out;
	}
	
	public function write( eArr : Array<Expr>, comments : Hash<TComment> ) {
		return parse(eArr,comments);
	}
	
	private function doExprArr( eArr : Array<Expr>, ctx : Dynamic ) {
		//trace("eArr = "+ eArr);
		var a = new Array<Content>();
		var usedAsValue :Bool = ctx.usedAsValue;
		var transToLast :Bool = ctx.transToLast;
		var i = 0;
		for( e in eArr ) {
			var isLast = (++i == eArr.length);
			if( (ctx.breakBeforeMethods && (getInnerExprName(e) == "EFunction") && i > 1 ) || 
				(getInnerExprName(e) == "EClass") || (getInnerExprName(e) == "EInterface") ||
				(getInnerExprName(e) == "ETypeDef") || (getInnerExprName(e) == "EEnum")
			)
				a.push( Newline );
			a.push( getIndent() );
			a = a.concat( doExpr(e, { usedAsValue : if(transToLast) usedAsValue && isLast else usedAsValue }) );
			a = addCondSemi(a, e, (transToLast && isLast));
			a.push( Newline );
			if( getInnerExprName(e) == "EPackage" )
				a.push( Newline );
		}
		return a;
	}
	
	private inline function exprName( e : Expr ) {
		return Type.enumConstructor(e);
	}
	
	private function cmpInnerExpr( inner : Expr, e : Expr ) {
		return (getInnerExprName(inner) == exprName(e));
	}
	
	private function getInnerExprName( e : Expr ) {
		return switch( e ) {
			case EVar(_,_,_,_): "EVar";
			case EModifier(e1,_): getInnerExprName(e1);
			case ETyped(e1,_): getInnerExprName(e1);
			case EFunction(_,e,_): if( e == null ) null else "EFunction"; //no gaps for interface definitions
			case EClass(_,_,_,_): "EClass";
			case EPackage(_): "EPackage";
			case EInterface(_,_,_): "EInterface";
			case ETypeDef(_,_,_): "ETypeDef";
			case EEnum(_,_): "EEnum";
			default: null;
		};
	}
	
	private function addCondSemi( a : Array<Content>, e : Expr, ?overrde : Bool ) : Array<Content> {
		if( overrde || ! noSemiExpr.exists( Std.string(Type.enumConstructor(e)) ) )
			a.push( Semi );
		return a;
	}
	
	private function doExpr( e : Expr, ctx : Dynamic ) {
		return Reflect.callMethod(
			this, Reflect.field( this, "do" + Type.enumConstructor(e) ), [e,ctx]
		);
	}
	
	private function getIndent( ?ind : Int ) : Content {
		if( ind == null ) ind = indent;
		return Indent( ind );
	}

	private function isNullVector( v : Expr ) : Bool {
		var params = Type.enumParameters(v);
		return ( params[0] == null && params[1] == [] && params[2] == [] );
	}
	
	private function doEConst( e : Expr, ctx : Dynamic ) {
		//EConst( c : Const );
		var const : String = Type.enumParameters(e)[0];
		return [ Tok(Type.enumParameters(const)[0]) ];
	}
	
	private function doEIdent( e : Expr, ctx : Dynamic ) {
		//EIdent( v : String );
		var a = [ Tok(Type.enumParameters(e)[0]) ];
		if( ctx.type != null ) {
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			a = a.concat( doExpr(ctx.type,{}) );
		}
		return a;
	}
	
	private function doEVar( e : Expr, ctx : Dynamic ) {
		//EVar( n : String, get : String, set : String, ?e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		if( ! ctx.collection )
			a = a.concat( [ Tok("var"), White(e,"postKeyword") ] );
		a.push( Tok(params[0]) );
		if( params[1] != null || params[2] != null )
			a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket"), 
				Tok(params[1]), Tok(","), White(e,"postComma"), Tok(params[2]), White(e,"preRightBracket"), Tok(")") ] );
		if( ctx.type != null ) {
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			a = a.concat( doExpr(ctx.type, {}) );
		}
		if( params[3] != null ) {
			a = a.concat( [ White(e,"preAssign"), Tok("="), White(e,"postAssign") ] );
			a = a.concat( doExpr(params[3], { usedAsValue : true }) );
		}
		if( ! ctx.collection )
			a.push( Semi );
		return a;
	}
	
	private function doEParent( e : Expr, ctx : Dynamic ) {
		//EParent( e : Expr );
		var a = new Array();
		a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		a = a.concat( doExpr(Type.enumParameters(e)[0], {}) );
		a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		return a;
	}
	
	private function doEBlock( e : Expr, ctx : Dynamic ) {
		//EBlock( e : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		if( params[0].length == 0 )
			//return [ Tok("{"), Tok("}") ];
			return [ Tok("{"), Indent(indent+1), Indent(indent), Tok("}") ];
		if( ctx.blockStartSpace == null ) ctx.blockStartSpace = true;
		if( ctx.blockBraces == null ) ctx.blockBraces = true;
		if( ctx.blockBreakAfterTopBrace == null ) ctx.blockBreakAfterTopBrace = true;
		if( ctx.blockBreakAfterBottomBrace == null ) ctx.blockBreakAfterBottomBrace = true;
		if( ctx.blockIndent == null ) ctx.blockIndent = true;
		if( ctx.blockIndent )
			indent++;
		if( ctx.blockStartSpace )
			a.push( White(e,"preLeftBrace") );
		if( ctx.blockBraces )
			a.push( Tok("{") );
		if( ctx.owner == "EClass" || ctx.owner == "EInterface")
			a.push( Newline );
		if( ctx.blockBreakAfterTopBrace )
			a.push( Newline );
		a = a.concat( doExprArr(params[0], { usedAsValue : ctx.usedAsValue, breakBeforeMethods : true }) );
		if( ctx.blockIndent )
			indent--;
		if( ctx.blockBraces ) {
			a.push( getIndent() );
			if( ctx.owner == "EClass" || ctx.owner == "EInterface")
				a.push( Newline );
			a.push( Tok("}") );
		}
		if( ctx.blockBreakAfterBottomBrace )
			a.push( Newline );
		return a;
	}
	
	private function doEField( e : Expr, ctx : Dynamic ) {
		//EField( e : Expr, f : String );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ Tok("."), Tok(params[1]) ] );
		return a;
	}
	
	private function doEBinop( e : Expr, ctx : Dynamic ) {
		//EBinop( op : String, e1 : Expr, e2 : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		var op :String = ( ctx.op != null ) ? ctx.op : params[0];
		ctx.inln = true;
		a = a.concat( doExpr(params[1], {}) );
		if( ctx.type != null ) {
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			a = a.concat( doExpr(ctx.type, {}) );
		}
		if( op != "..." )
			a.push( White(e,"preOp") );
		a.push( Tok(op) );
		if( op != "..." )
			a.push( White(e,"postOp") );
		a = a.concat( doExpr(params[2], { usedAsValue : (params[0] == "="), blockStartSpace : ctx.val_blockStartSpace }) );
		return a;
	}
	
	private function doEUnop( e : Expr, ctx : Dynamic ) {
		//EUnop( op : String, prefix : Bool, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		var prefix = params[1];
		if( prefix )
			a = a.concat( [ Tok(params[0]), White(e,"preExpr") ] );
		a = a.concat( doExpr(params[2], {}) );
		if( ! prefix )
			a = a.concat( [ White(e,"postExpr"), Tok(params[0])  ] );
		return a;
	}
	
	private function doECall( e : Expr, ctx : Dynamic ) {
		//ECall( e : Expr, params : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var arr :Array<Expr> = params[1];
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		for( exp in arr ) {
			a = a.concat( doExpr(exp, { usedAsValue : true }) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( arr.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		return a;
	}
	
	private function doEIf( e : Expr, ctx : Dynamic ) {
		//EIf( cond : Expr, e1 : Expr, ?e2 : Expr, ternary : Bool );
		var a = new Array();
		var params = Type.enumParameters(e);
		var cond = params[0];
		var ifExpr = params[1];
		var elseExpr = params[2];
		var ternary = params[3];
		var usedAsValue = ( ctx.usedAsValue == null ) ? false : ctx.usedAsValue;
		ctx.usedAsValue = null;
		if( ctx.ifInln == null ) ctx.ifInln = false;
		if( ternary ) {
			a = a.concat( doExpr(params[0], {}) );
			a = a.concat( [ White(e,"preQuestion"), Tok("?"), White(e,"postQuestion") ] );
			a = a.concat( doExpr(params[1], {}) );
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			a = a.concat( doExpr(params[2], {}) );
		} else {
			a.push( Tok("if") );
			a = a.concat( doExpr(cond, {}) );
			a.push( White(e,"postCondition") );
			var ifExprIsBlock = ( Type.enumConstructor(ifExpr) == "EBlock" );
			ctx.blockBraces = ifExprIsBlock;
			ctx.blockStartSpace = false;
			ctx.blockBreakAfterTopBrace = true;
			ctx.blockBreakAfterBottomBrace = false;
			ctx.blockIndent = true;
			if( ! ifExprIsBlock && ! usedAsValue ) {
				a.push( Newline );
				a.push( getIndent( ++indent ) );
			}
			a = a.concat( doExpr(ifExpr, ctx) );
			ctx.usedAsValue = null; // careful for these
			if( ! ifExprIsBlock ) {
				if( ! usedAsValue ) {
					a = addCondSemi(a,ifExpr);
					if( elseExpr != null )
						a.push( Newline );
					a.push( getIndent( --indent ) );
				}
			}
			if( elseExpr != null ) {
				var elseExprIsBlock = ( Type.enumConstructor(elseExpr) == "EBlock" );
				var elseExprIsIf = ( Type.enumConstructor(elseExpr) == "EIf" );
				ctx.ifInln = elseExprIsIf;
				ctx.blockBraces = elseExprIsBlock;
				ctx.blockStartSpace = false;
				if( ifExprIsBlock || usedAsValue )
					a.push( White(e,"preCondition") );
				a.push( Tok("else") );
				if( elseExprIsIf || elseExprIsBlock || usedAsValue )
					a.push( White(e,"postCondition") );
				else {
					a.push( Newline );
					a.push( getIndent( ++indent ) );
				}
				a = a.concat( doExpr(elseExpr, ctx) );
				if( ! (elseExprIsIf || elseExprIsBlock) ) {
					if( ! usedAsValue ) {
						a = addCondSemi(a,elseExpr);
						indent--;
					}
				}
			}
			ctx.usedAsValue = null;
		}
		return a;
	}
	
	private function doECond( e : Expr, ctx : Dynamic ) {
		//ECond( a : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var arr :Array<Expr> = params[0];
		ctx.blockBraces = false;
		ctx.blockBreakAfterTopBrace = true;
		ctx.blockBreakAfterBottomBrace = false;
		ctx.blockIndent = false;
		var i = 0;
		for( exp in arr ) {
			if( i++ > 0 )
				a.push( getIndent() );
			a = a.concat( doExpr(exp, ctx) );
		}
		a.push( getIndent() );
		a.push( Tok("#end") );
		return a;
	}
	
	private function doECondIf( e : Expr, ctx : Dynamic ) {
		//ECondIf( cond : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		ctx.blockBraces = false;
		ctx.blockBreakAfterBottomBrace = false;
		ctx.blockIndent = false;
		a = a.concat( [ Tok("#if"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], ctx) );
		a = a.concat( doExpr(params[1], ctx) );
		return a;
	}
	
	private function doECondElse( e : Expr, ctx : Dynamic ) {
		//ECondElse( e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		ctx.blockBraces = false;
		ctx.blockBreakAfterBottomBrace = false;
		ctx.blockIndent = false;
		a = a.concat( [ Tok("#else"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], ctx) );
		return a;
	}
	
	private function doECondElseif( e : Expr, ctx : Dynamic ) {
		//ECondElseif( cond : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		ctx.blockBraces = false;
		ctx.blockBreakAfterBottomBrace = false;
		ctx.blockIndent = false;
		a = a.concat( [ Tok("#elseif"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], ctx) );
		a = a.concat( doExpr(params[1], ctx) );
		return a;
	}
	
	private function doEWhile( e : Expr, ctx : Dynamic ) {
		//EWhile( cond : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a.push( Tok("while") );
		a = a.concat( doExpr(params[0], {}) );
		if( Type.enumConstructor(params[1]) != "EBlock" ) {
			a.push( Newline );
			a.push( getIndent( ++indent ) );
		}
		a = a.concat( doExpr(params[1], { blockBreakAfterBottomBrace : false, blockStartSpace : true }) );
		if( Type.enumConstructor(params[1]) != "EBlock" ) {
			addCondSemi(a, params[1]);
			indent--;
		}
		return a;
	}
	
	private function doEFor( e : Expr, ctx : Dynamic ) {
		//EFor( v : String, it : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("for"), White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket"), 
			Tok(params[0]), White(e,"preIn"), Tok("in"), White(e,"postIn") ] );
		a = a.concat( doExpr(params[1], {}) );
		a = a.concat( [ White(e,"preRightBracket"), Tok(")"), White(e,"postRightBracket")  ] );
		if( Type.enumConstructor(params[2]) != "EBlock" ) {
			a.push( Newline );
			a.push( getIndent( ++indent ) );
		}
		a = a.concat( doExpr(params[2], { blockBreakAfterBottomBrace : false }) );
		if( Type.enumConstructor(params[2]) != "EBlock" ) { // add semi for non-block. use blocks for all fors?
			addCondSemi(a, params[2]);
			indent--;
		}
		return a;
	}
	
	private function doEBreak( e : Expr, ctx : Dynamic ) {
		//EBreak;
		return [ Tok("break") ];
	}
	
	private function doEContinue( e : Expr, ctx : Dynamic ) {
		//EContinue;
		return [ Tok("continue") ];
	}
	
	private function doEFunction( e : Expr, ctx : Dynamic ) {
		//EFunction( params : Array<Expr>, e : Expr, ?name : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		var name :Expr = params[2];
		var args :Array<Expr> = params[0];
		a = a.concat( [ Tok("function"), White(e,"postKeyword") ] );
		if( name != null )
			a = a.concat( doExpr(name, {}) );
		a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		for( arg in args ) {
			a = a.concat( doExpr(arg, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( args.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		if( ctx.type != null ) {
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			a = a.concat( doExpr(ctx.type, {}) );
		}
		if( params[1] != null ) { // has block
			a.push( White(e,"preLeftBrace") );
			a = a.concat( doExpr(params[1], { blockBreakAfterBottomBrace : false, blockStartSpace : false }) );
		} else
			a.push( Semi ); // func declaration in interface
		return a;
	}
	
	private function doEReturn( e : Expr, ctx : Dynamic ) {
		//EReturn( ?e : Expr );
		ctx.usedAsValue = true;
		var a = [ Tok("return"), White(e,"postKeyword") ];
		a = a.concat( doExpr(Type.enumParameters(e)[0], ctx) );
		return a;
	}
	
	private function doEArray( e : Expr, ctx : Dynamic ) {
		//EArray( e : Expr, index : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( doExpr(params[0], ctx) );
		a = a.concat( [ White(e,"preLeftBlockBracket"), Tok("["), White(e,"postLeftBlockBracket") ] );
		a = a.concat( doExpr(params[1], ctx) );
		a = a.concat( [ White(e,"preRightBlockBracket"), Tok("]") ] );
		return a;
	}
	
	private function doEArrayDecl( e : Expr, ctx : Dynamic ) {
		//EArrayDecl( e : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var args :Array<Expr> = params[0];
		a = a.concat( [ White(e,"preLeftBlockBracket"), Tok("["), White(e,"postLeftBlockBracket") ] );
		for( arg in args ) {
			a = a.concat( doExpr(arg, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( args.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBlockBracket"), Tok("]") ] );
		return a;
	}
	
	private function doENew( e : Expr, ctx : Dynamic ) {
		//ENew( cl : Expr, params : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var args :Array<Expr> = params[1];
		a = a.concat( [ Tok("new"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		for( arg in args ) {
			a = a.concat( doExpr(arg, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( args.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		if( ctx.postFixArr != null )
			a = a.concat( ctx.postFixArr );
		return a;
	}
	
	private function doEThrow( e : Expr, ctx : Dynamic ) {
		//EThrow( e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("throw"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		return a;
	}
	
	private function doETry( e : Expr, ctx : Dynamic ) {
		//ETry( e : Expr, catchArr : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var catchArr :Array<Expr> = params[1];
		var inln = false;
		var usedAsValue = ( ctx.usedAsValue == null ) ? false : ctx.usedAsValue;
		a.push( Tok("try") );
		if( Type.enumConstructor(params[0]) != "EBlock" ) {
			a.push( Newline );
			a.push( getIndent( ++indent ) );	
		} else {
			inln = true;
		}
		a = a.concat( doExpr(params[0], { blockBreakAfterBottomBrace : false, blockStartSpace : true }) );
		if( Type.enumConstructor(params[0]) != "EBlock" ) {
			if( ! ctx.usedAsValue )
				addCondSemi(a,params[0]);
			a.push( Newline );
			a.push( getIndent( --indent ) );
		}
		for( c in catchArr ) {
			a = a.concat( doExpr(c, { inln : inln, blockBreakAfterBottomBrace : false, usedAsValue : usedAsValue }) );
			inln = Type.enumEq( a[a.length-1], Tok("}") );
		}
		return a;
	}
	
	private function doECatch( e : Expr, ctx : Dynamic ) {
		//ECatch( err : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		ctx.blockBraces = true;
		ctx.blockBreakAfterTopBrace = true;
		ctx.blockBreakAfterBottomBrace = false;
		if( ctx.inln ) {
			a.push( White(e,"preKeyword") );
			ctx.inln = null;
		}
		a = a.concat( [ Tok("catch"), White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ White(e,"preRightBracket"), Tok(")"), White(e,"postRightBracket") ] );
		if( Type.enumConstructor(params[1]) != "EBlock" ) {
			a.push( Newline );
			a.push( getIndent( ++indent ) );
		}
		a = a.concat( doExpr(params[1], ctx) );
		if( Type.enumConstructor(params[1]) != "EBlock" )
			indent--;
		return a;
	}
	
	private function doETyped( e : Expr, ctx : Dynamic ) {
		//ETyped( e : Expr, ?v : Expr );
		var params = Type.enumParameters(e);
		ctx.inln = false;
		ctx.type = params[1];
		var a = doExpr( params[0], ctx );
		return a;
	}
	
	private function doEModifier( e : Expr, ctx : Dynamic ) {
		//EModifier( e : Expr, f : String );
		ctx.inln = true;
		var a = [ Tok(Type.enumParameters(e)[1]), White(e,"postKeyword") ];
		a = a.concat( doExpr(Type.enumParameters(e)[0], ctx) );
		return a;
	}
	
	private function doEImport( e : Expr, ctx : Dynamic ) {
		//EImport( v : String );
		var a = [ Tok("import"), White(e,"postKeyword") ];
		a = a.concat( makeDotArr(Type.enumParameters(e)[0]) ); 
		return a;
	}
	
	private function doEAObj( e : Expr, ctx : Dynamic ) {
		//EAObj( a : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var exps :Array<Expr> = params[0];
		a = a.concat( [ Tok("{"), White(e,"postLeftBrace") ] );
		for( exp in exps ) {
			a = a.concat( doExpr(exp, { op : ":", val_blockStartSpace : false }) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( exps.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBrace"), Tok("}") ] );
		return a;
	}
	
	private function doEClass( e : Expr, ctx : Dynamic ) {
		//EClass( n : Expr, ext : Expr, i : Array<Expr>, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		var imps :Array<Expr> = params[2];
		a = a.concat( [ Tok("class"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		if( params[1] != null ) {
			a = a.concat( [ White(e,"preExtends"), Tok("extends"), White(e,"postExtends") ] );
			a = a.concat( doExpr(params[1], {}) );
		}
		if( imps.length > 0 ) {
			if( params[1] != null )
				a = a.concat( [ Tok(","), White(e,"postComma") ] );
			else
				a.push( White(e,"postExtendsName") );
			for( imp in imps ) {
				a = a.concat( [ Tok("implements"), White(e,"postImplements") ] );
				a = a.concat( doExpr(imp, {}) );
				a = a.concat( [ Tok(","), White(e,"postComma") ] );
			}
			if( imps.length > 0 ) {
				a.pop(); a.pop();
			}
		}
		a.push( White(e,"preLeftBrace") );
		a = a.concat( doExpr(params[3], { blockBreakAfterBottomBrace : false, owner : "EClass" }) );
		return a;
	}

	private function doESwitch( e : Expr, ctx : Dynamic ) {
		//ESwitch( e1 : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a.push( Tok("switch") );
		a = a.concat( doExpr(params[0], {}) );
		a.push( White(e,"preLeftBrace") );
		a = a.concat( doExpr(params[1], { blockBreakAfterBottomBrace : false, usedAsValue : ctx.usedAsValue }) );
		return a;
	}
	
	private function doECase( e : Expr, ctx : Dynamic ) {
		//ECase( matchArr : Array<Expr>, eArr : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var matchArr :Array<Expr> = params[0];
		var eArr : Array<Expr> = params[1];
		a = a.concat( [ Tok("case"), White(e,"postKeyword") ] );
		for( match in matchArr ) {
			a = a.concat( doExpr(match, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		a.pop(); a.pop();
		a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
		indent++;
		if( eArr.length > 1 ) {
			a.push( Newline  );
			a = a.concat( doExprArr( eArr, { usedAsValue : ctx.usedAsValue, transToLast : true } ) );
		} else if( eArr.length > 0 ) {
			a = a.concat( doExpr( eArr[0], { usedAsValue : ctx.usedAsValue } ) );
			a = addCondSemi(a,eArr[0],ctx.usedAsValue);
		}
		indent--;
		if( a[a.length-1] == Newline ) // bad hack for current switch/case/default structure
			a.pop();
		return a;
	}
	
	private function doEDefault( e : Expr, ctx : Dynamic ) {
		//EDefault( eArr : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var eArr : Array<Expr> = params[0];
		a = a.concat( [ Tok("default"), White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
		indent++;
		if( eArr.length > 1 ) {
			a.push( Newline  );
			a = a.concat( doExprArr( eArr, { usedAsValue : ctx.usedAsValue, transToLast : true } ) );
		} else if( eArr.length > 0 ) {
			a = a.concat( doExpr( eArr[0], { usedAsValue : ctx.usedAsValue } ) );
			a = addCondSemi(a,eArr[0],ctx.usedAsValue);
		}
		indent--;
		if( a[a.length-1] == Newline ) // bad hack for current switch/case/default structure
			a.pop();
		return a;
	}
	
	private function doEDoWhile( e : Expr, ctx : Dynamic ) {
		//EDoWhile( e : Expr, cond : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("do"), White(e,"postKeyword") ] );
		if( Type.enumConstructor(params[0]) != "EBlock" ) {
			a.push( Newline );
			a.push( getIndent( ++indent ) );
		}
		a = a.concat( doExpr(params[0], { blockBreakAfterBottomBrace : false, blockStartSpace : false }) );
		if( Type.enumConstructor(params[0]) != "EBlock" ) {
			addCondSemi(a, params[0]);
			a.push( Newline );
			indent--;
		} else
			a.push( White(e,"postRightBrace") );
		a.push( Tok("while") );
		a = a.concat( doExpr(params[1], {}) ); 
		return a;
	}
	
	private function doEVarCollection( e : Expr, ctx : Dynamic ) {
		//EVarCollection( a : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var idents : Array<Expr> = params[0];
		a = a.concat( [ Tok("var"), White(e,"postKeyword") ] );
		for( i in idents ) {
			a = a.concat( doExpr(i, { collection : true }) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( idents.length > 0 ) {
			a.pop(); a.pop();
		}
		return a;
	}
	
	private function doEInterface( e : Expr, ctx : Dynamic ) {
		//EInterface( n : Expr, i : Array<Expr>, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		var iArr :Array<Expr> = params[1];
		a = a.concat( [ Tok("interface"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		for( i in iArr ) {
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
			a = a.concat( doExpr(i, {}) );
		}
		a = a.concat( doExpr(params[2], { blockBreakAfterBottomBrace : false, owner : "EInterface" }) );
		return a;
	}
	
	private function doEUntyped( e : Expr, ctx : Dynamic ) {
		//EUntyped( e : Expr );
		var a = [ Tok("untyped"), White(e,"postKeyword") ];
		a = a.concat( doExpr(Type.enumParameters(e)[0], ctx) );
		return a;
	}
	
	private function doEVector( e : Expr, ctx : Dynamic ) {
		//EVector( n : String, tparams : Array<Expr>, cparams : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var tparams :Array<Expr> = params[1];
		var cparams :Array<Expr> = params[2];
		a = a.concat( makeDotArr(params[0]) );
		if( tparams.length > 0 ) {
			a.push( White(e,"preLeftAngleBracket") );
			a.push( Tok("<") );
			a.push( White(e,"postLeftAngleBracket") );
			for( tp in tparams ) {
				a = a.concat( doExpr(tp, {}) );
				if( tparams.length > 1 )
					a.push( Tok(",") );
			}
			if( tparams.length > 1 )
				a.pop();
			a.push( White(e,"preRightAngleBracket") );
			a.push( Tok(">") );
		}
		if( cparams.length > 0 ) {
			a = a.concat( [ White(e,"preColon"), Tok(":"), White(e,"postColon") ] );
			if( cparams.length > 1 )
				a = a.concat( [ Tok("("), White(e,"postLeftBracket") ] );
			for( cp in cparams ) {
				a = a.concat( doExpr(cp, {}) );
				if( cparams.length > 1 )
					a = a.concat( [ Tok(","), White(e,"postComma") ] );
			}
			if( cparams.length > 1 )
				a.pop();
			if( cparams.length > 1 )
				a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		}
		return a;
	}
	
	private function doEDuckType( e : Expr, ctx : Dynamic ) {
		//EDuckType( ext : Expr, a : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var ext :Null<Expr> = params[0];
		var args :Array<Expr> = params[1];
		a = a.concat( [ Tok("{"), White(e,"postLeftBrace") ] );
		if( ext != null ) {
			a = a.concat( [ Tok(">"), White(e,"postExtends") ] );
			a = a.concat( doExpr(ext, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		for( arg in args ) {
			a = a.concat( doExpr(arg, {}) );
			a = a.concat( [ Tok(","), White(e,"postComma") ] );
		}
		if( args.length > 0 ) {
			a.pop(); a.pop();
		}
		a = a.concat( [ White(e,"preRightBrace"), Tok("}") ] );
		return a;
	}
	
	private function doETypeDef( e : Expr, ctx : Dynamic ) {
		//ETypeDef( n : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("typedef"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ White(e,"preAssign"), Tok("="), White(e,"postAssign") ] );
		a = a.concat( doExpr(params[1], { blockBreakAfterBottomBrace : false }) );
		return a;
	}
	
	private function doEEnum( e : Expr, ctx : Dynamic ) {
		//EEnum( n : Expr, e : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("enum"), White(e,"postKeyword") ] );
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( doExpr(params[1], { blockBreakAfterBottomBrace : false }) );
		return a;
	}
	
	private function doEEnumConstr( e : Expr, ctx : Dynamic ) {
		//EEnumConstr( n : Expr, a : Array<Expr> );
		var a = new Array();
		var params = Type.enumParameters(e);
		var args :Array<Expr> = params[1];
		a = a.concat( doExpr(params[0], {}) );
		if( args.length > 0 ) {
			a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
			for( arg in args ) { 
				a = a.concat( doExpr(arg, {}) );
				a = a.concat( [ Tok(","), White(e,"postComma") ] );
			}
			if( args.length > 0 )
				a.pop();
			a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		}
		return a;
	}
	
	private function doEPackage( e : Expr, ctx : Dynamic ) {
		//EPackage( n : String );
		var a = [ Tok("package"), White(e,"postKeyword") ];
		a = a.concat( makeDotArr(Type.enumParameters(e)[0]) );
		return a;
	}
	
	private function doEOptional( e : Expr, ctx : Dynamic ) {
		//EOptional( e : Expr );
		var a = [ Tok("?"), White(e,"postQuestion") ];
		a = a.concat( doExpr(Type.enumParameters(e)[0], ctx) );
		return a;
	}
	
	private function doEFuncPoint( e : Expr, ctx : Dynamic ) {
		//EFuncPoint( e : Expr, nxt : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ White(e,"preArrow"), Tok("->"), White(e,"postArrow") ] );
		a = a.concat( doExpr(params[1], {}) );
		return a;
	}
	
	private function doECast( e : Expr, ctx : Dynamic ) {
		//ECast( e : Expr, type : Expr );
		var a = new Array();
		var params = Type.enumParameters(e);
		a = a.concat( [ Tok("cast") ] );
		if( params[1] == null ) {
			a.push( White(e,"postKeyword") );
			a = a.concat( doExpr(params[0], {}) );
			return a;
		}
		a = a.concat( [ White(e,"preLeftBracket"), Tok("("), White(e,"postLeftBracket") ] );
		a = a.concat( doExpr(params[0], {}) );
		a = a.concat( [ Tok(","), White(e,"postComma") ] );
		a = a.concat( doExpr(params[1], {}) );
		a = a.concat( [ White(e,"preRightBracket"), Tok(")") ] );
		return a;
	}
	
	private function makeDotArr( dotPath : String ) {
		var a = dotPath.split( "." );
		var out = new Array<Content>();
		for( i in 0...a.length ) {
			out.push( Tok(a[i]) );
			out.push( Tok(".") );
		}
		out.pop();
		return out;
	}
	
}
