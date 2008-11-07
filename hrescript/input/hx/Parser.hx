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
package hrescript.input.hx;

import hrescript.input.hx.Expr;

enum Token {
	TEof;
	TConst( c : Const );
	TId( s : String );
	TOp( s : String );
	TPOpen;
	TPClose;
	TBrOpen;
	TBrClose;
	TDot;
	TComma;
	TSemicolon;
	TBkOpen;
	TBkClose;
	TQuestion;
	TDoubleDot;
}

typedef TComment = {
	var pre : Array<Int>; //whitespace char nums preceeding
	var text : String;
	var multiline : Bool;
	var post : Array<Int>; //whitespace char nums following
	var key : String;
}

class Parser {

	// config / variables
	public var line : Int;
	public var opChars : String;
	public var identChars : String;
	public var opPriority : Array<String>;
	public var unopsPrefix : Array<String>;
	public var unopsSuffix : Array<String>;

	// implementation
	var char : Null<Int>;
	var ops : Array<Bool>;
	var idents : Array<Bool>;
	var tokens : haxe.FastList<Token>;
	
	var commentMap : Hash<TComment>;
	var t : Int; // accurate indication of tokens parsed
	var subCommentCount : Int;
	var currentComment : TComment;
	var currentWhite : Array<Int>;

	public function new() {
		line = 1;
		opChars = "+*/-=!><&|^%~";
		identChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
		opPriority = [
			"...",
			"=",
			"||","&&",
			"==","!=",">","<",">=","<=",
			"|","&","^",
			"<<",">>",">>>",
			"+","-",
			"*","/",
			"%"
		];
		unopsPrefix = ["!","++","--","-","~"];
		unopsSuffix = ["++","--"];
	}

	public function parseString( s : String ) {
		line = 1;
		return parse( new haxe.io.StringInput(s) );
	}

	public function parse( s : haxe.io.Input ) {
		char = null;
		ops = new Array();
		idents = new Array();
		tokens = new haxe.FastList<Token>();
		commentMap = new Hash();
		t = subCommentCount = 0;
		for( i in 0...opChars.length )
			ops[opChars.charCodeAt(i)] = true;
		for( i in 0...identChars.length )
			idents[identChars.charCodeAt(i)] = true;
		currentWhite = [];
		var a = new Array();
		while( true ) {
			var tk = token(s);
			if( tk == TEof ) break;
			tokens.add(tk);
			a.push(parseFullExpr(s));
		}
		//trace("parse: a =  "+ Std.string(a));
		return a;
	}
	
	public function getComments() :Hash<TComment> {
		return commentMap;
	}

	function unexpected( tk ) : Dynamic {
		throw Error.EUnexpected(tokenString(tk));
		return null;
	}
	
	function unexpected_msg( s : String ) {
		throw Error.EUnexpected(s);
		return null;
	}
	
	function isBlock(e) {
		return switch( e ) {
		case EBlock(_): true;
		case EFunction(_,e,_): isBlock(e);
		case EInterface(_,_,e): isBlock(e);
		case EVar(_,_,_,e): e != null && isBlock(e);
		case EIf(_,e1,e2,_): if( e2 != null ) isBlock(e2) else isBlock(e1);
		case EBinop(_,_,e): isBlock(e);
		case EUnop(_,prefix,e): !prefix && isBlock(e);
		case EWhile(_,e): isBlock(e);
		case EFor(_,_,e): isBlock(e);
		case EReturn(e): e != null && isBlock(e);
		case EClass(_,_,_,e): isBlock(e);
		case ETyped(e,_): isBlock(e);
		case EModifier(e,_): isBlock(e);
		case EUntyped(e): isBlock(e);
		default: false;
		}
	}

	function parseFullExpr(s) {
		var e = parseExpr(s);
		var tk = token(s);
		if( tk != TSemicolon && tk != TEof ) { //we turn off semi-colon enforcement for the moment
			//if( isBlock(e) )
				tokens.add(tk);
			//else
				//unexpected(tk);
		}
		return e;
	}
	
	/**
		parses anonymous objects like { a : 10, b : 12 }, turning each 'a : 10' into
		an EBinop assignment.. it also enforces a semi-colon at the end (which is different
		from normal blocks.
	**/
	function readObject( s : haxe.io.Input ) {
		var a = new Array();
		var tk = token(s);
		tokens.add(tk);
		while( tk != TBrClose ) {
			var e1 = parseExpr(s);
			tk = token(s);
			var e2 = parseExpr(s);
			a.push( EBinop("=",e1,e2) );
			tk = token(s);
		}
		return EAObj(a);
	}

	function parseExpr( s : haxe.io.Input ) {
		var tk = token(s);
		//trace("parseExpr: tk = " +tk);
		switch( tk ) {
		case TId(id):
			var e = parseStructure(s,id);
			if( e == null )
				e = EIdent(id);
			return parseExprNext(s,e);
		case TConst(c):
			return parseExprNext(s,EConst(c));
		case TPOpen:
			var e = parseExpr(s);
			tk = token(s);
			if( tk != TPClose ) unexpected(tk);
			return parseExprNext(s,EParent(e));
		case TBrOpen:
			var a = new Array();
			while( true ) {
				tk = token(s);
				if( tk == TBrClose )
					break;
				var t2 = token(s);
				tokens.add(t2);
				tokens.add(tk);
				if( t2 == TDoubleDot )
					return readObject(s);
				a.push(parseFullExpr(s));
			}
			return EBlock(a);
		case TOp(op):
			var found;
			for( x in unopsPrefix )
				if( x == op )
					return makeUnop(op,parseExpr(s));
			return unexpected(tk);
		case TBkOpen:
			return parseExprNext(s,EArrayDecl(parseExprList(s,TBkClose)));
		default:
			return unexpected(tk);
		}
	}

	function priority(op) {
		for( i in 0...opPriority.length )
			if( opPriority[i] == op )
				return i;
		return -1;
	}

	function makeUnop( op, e ) {
		return switch( e ) {
		case EBinop(bop,e1,e2): EBinop(bop,makeUnop(op,e1),e2);
		default: EUnop(op,true,e);
		}
	}

	function makeBinop( op, e1, e ) {
		return switch( e ) {
		case EBinop(op2,e2,e3):
			if( priority(op) > priority(op2) )
				EBinop(op2,makeBinop(op,e1,e2),e3);
			else
				EBinop(op,e1,e);
		default: EBinop(op,e1,e);
		}
	}
	
	function getDotPath( s : haxe.io.Input ) {
		var out = new Array<String>();
		var t = token(s);
		var dot = true;
		while( true ) {
			switch(t) {
			case TId(id): 
				if( dot ) dot = false;
				else break;
			case TDot: dot = true;
			default: break;
			}
			out.push( tokenString(t) );
			t = token(s);
		}
		tokens.add(t);
		return ( out.length == 0 ) ? null : out.join("");
	}
	
	function getVectorPath( s : haxe.io.Input, ?isNested : Bool ) {
		if( isNested == null )
			isNested = false;
		var path = getDotPath( s );
		if( path == null )
			return null;
		var tk = token(s);
		var typeParams = new Array();
		var constraintParams = new Array();
		var cBrackets = false;
		if( Type.enumEq(tk,TOp("<")) ) {
			while( ! Type.enumEq(tk,TOp(">")) ) {
				tk = token(s);
				if( tk == TBrOpen )
					typeParams.push( getDuckType( s ) );
				else {
					tokens.add(tk);
					typeParams.push( getVectorPath(s, true) );
				}
				tk = token(s); 
			}
		} else {
			switch( tk ) {
			case TDoubleDot:
				tk = token(s);
				if( tk == TPOpen ) {
					cBrackets = true;
					while( tk != TPClose ) {
						constraintParams.push( getVectorPath(s) );
						tk = token(s);
						if( tk != TComma )
							tokens.add(tk);
					}
					tk = token(s);
					return EVector( path, typeParams, constraintParams, cBrackets );
				} else {
					tokens.add(tk);
					return EVector( path, typeParams, [getVectorPath(s)], cBrackets );
				}
			case TComma:
				if( ! isNested )
					tokens.add(tk);
				return EVector( path, typeParams, constraintParams, cBrackets );
			case TPClose:
				tokens.add(tk);
				return EVector( path, typeParams, constraintParams, cBrackets );
			default: 
				switch(tk) {
				case TOp(id):
					if( id == "->" )
						return EFuncPoint( EVector( path, typeParams, constraintParams, cBrackets ), getVectorPath(s) );
					else if( id.indexOf(">") != -1 ) {
						if( id.length > 1 )
							t += id.length-1;	//we need to update token count as '>>>' (in A<B<C<D>>>) etc
												//is later used in a single '>' token context
						for( c in 0...id.length )
							tokens.add(TOp(">"));
					} else
						tokens.add(tk);
				case TPClose:
					tk = token(s);
					return EVector( path, typeParams, constraintParams, cBrackets );
				default: // ids without <> are allowed to 'slip through' here
					tokens.add(tk);
				}
			}
		}
		tk = token(s);
		if( Type.enumEq(tk,TOp("->")) )
			return EFuncPoint( EVector( path, typeParams, constraintParams, cBrackets ), getVectorPath(s) );
		else {
			tokens.add(tk);
			return EVector( path, typeParams, constraintParams, cBrackets );
		}
	}
	
	function getDuckType( s : haxe.io.Input ) {
		var t = token(s);
		if( Type.enumEq(t,TOp(">")) ) {
			var ext = getVectorPath( s );
			t = token(s); // swallow comma
			return EDuckType( ext, parseExprTypedList( s, TBrClose ) );
		} else {
			tokens.add(t);
			return EDuckType( null, parseExprTypedList( s, TBrClose ) );
		}
	}

	function getDoubleDotType( s : haxe.io.Input, tk : Token ) {
		if( Type.enumEq(tk,TDoubleDot) ) {
			var tk = token(s);
			if( tk == TBrOpen )
				return getDuckType( s );
			else {
				tokens.add(tk);
				return getVectorPath( s );
			}
		}
		return null;
	}
	
	function parseStructure( s, id ) {
		//trace("parseStructure: id = "+ id);
		return switch( id ) {
		case "public": EModifier( parseExpr(s), "public" );
		case "private": EModifier( parseExpr(s), "private" );
		case "static": EModifier( parseExpr(s), "static" );
		case "inline": EModifier( parseExpr(s), "inline" );
		case "dynamic": EModifier( parseExpr(s), "dynamic" );
		case "override": EModifier( parseExpr(s), "override" );
		case "untyped": EUntyped( parseExpr(s) );
		case "typedef":
			var n = getVectorPath(s);
			var tk = token(s); // swallow '='
			tk = token(s);
			var v = null;
			var ext = null;
			var a = [];
			if( tk == TBrOpen ) {
				tk = token(s);
				if( Type.enumEq(tk,TOp(">")) ) {
					ext = getVectorPath(s);
					tk = token(s); // swallow comma
					tk = token(s);
				}
				while( tk != TBrClose ) {
					tokens.add(tk);
					var dec = parseExpr(s); //TODO: some var validation
					a.push( dec );
					tk = token(s); // swallow semi
					tk = token(s);
				}
			} else {
				tokens.add(tk);
				v = getVectorPath(s);
			}
			tk = token(s);
			tokens.add(tk);
			ETypeDef( n, v, ext, a, (tk == TSemicolon) );
		case "enum":
			var n = getVectorPath(s);
			var tk = token(s);
			var constrs = new Array();
			while( tk != TBrClose ) {
				var name = getVectorPath(s);
				tk = token(s);
				if( tk == TBrClose )
					break;
				var a = new Array();
				if( tk == TPOpen ) {
					a = parseExprTypedList( s, TPClose );
					tk = token(s);
				}
				constrs.push( EEnumConstr(name,a) );
			}
			EEnum( n, EBlock( constrs ) );
		case "class":
			var n = getVectorPath(s);
			var tk = token(s);
			var ext = null;
			if( Type.enumEq(tk,TId("extends")) ) {
				ext = getVectorPath(s);
				tk = token(s);
			}
			if( tk == TComma )
				tk = token(s);
			var i = new Array();
			while( Type.enumEq(tk,TId("implements")) ) {
				do {
					i.push( getVectorPath(s) );
					tk = token(s);
					if( tk == TComma )
						tk = token(s);
				} while( tk != TBrOpen );
				tokens.add(tk);
			}
			if( i.length == 0 && ext != null )
				tokens.add(tk);
			if( tokens.first() != TBrOpen && tk == TBrOpen )
				tokens.add(tk);
			var e = parseExpr(s);
			EClass( n, ext, i, e);
		case "interface":
			var n = getVectorPath(s);
			var tk = token(s);
			var i = new Array();
			while( Type.enumEq(tk,TId("implements")) ) {
				do {
					i.push( getVectorPath(s) );
					tk = token(s);
					if( tk == TComma )
						tk = token(s);
				} while( tk != TBrOpen );
			}
			tokens.add(tk);
			EInterface( n, i, parseExpr(s) );
		case "import": EImport( getDotPath(s) );
		case "package": EPackage( getDotPath(s) );
		case "if":
			var cond = parseExpr(s);
			var e1 = parseExpr(s);
			var e2 = null;
			var semic = false;
			var tk = token(s);
			if( tk == TSemicolon ) {
				semic = true;
				tk = token(s);
			}
			if( Type.enumEq(tk,TId("else")) )
				e2 = parseExpr(s);
			else {
				tokens.add(tk);
				if( semic ) tokens.add(TSemicolon);
			}
			EIf( cond, e1, e2, false );
		case "#if":
			var cond = parseExpr(s);
			var ifArr = new Array();
			var allArr = new Array<Expr>();
			var eArr = new Array();
			var exprArr = new Array();
			var e = null;
			var tk = token(s);
			while( true ) {
				if( Type.enumEq(tk,TId("#end")) ) {
					allArr.push( 
						if( allArr.length == 0 ) ECondIf( cond, EBlock( exprArr ) )
						else if( cond == null ) ECondElse( EBlock( exprArr ) )
						else ECondElseif( cond, EBlock( exprArr ) )
					);	
					break;
				}
				if( Type.enumEq(tk,TId("#else")) ) {
					allArr.push( 
						if( allArr.length == 0 ) ECondIf( cond, EBlock( exprArr ) )
						else ECondElse( EBlock( exprArr ) )
					);
					cond = null;
					exprArr = new Array();
					tk = token(s);
				}
				if( Type.enumEq(tk,TId("#elseif")) ) {
					allArr.push( 
						if( allArr.length == 0 ) ECondIf( cond, EBlock( exprArr ) )
						else ECondElseif( cond, EBlock( exprArr ) )
					);	
					cond = parseExpr(s);
					exprArr = new Array();
					tk = token(s);
				}
				tokens.add(tk);
				e = parseExpr(s);
				exprArr.push( e );
				tk = token(s); // swallow ';'
				if( tk == TSemicolon )
					tk = token(s);
			}	
			ECond( allArr );	
		case "switch":
			var e1 = parseExpr(s);
			var tk = token(s);
			if( tk != TBrOpen )
				unexpected(tk);
			tk = token(s);
			var matchArr = new Array();
			var a = new Array();
			var d = null;
			while( true ) {
				if( Type.enumEq(tk,TId("case")) ) {
					matchArr = [];
					matchArr.push( parseExpr(s) );
					tk = token(s);
					while( tk == TComma ) {
						matchArr.push( parseExpr(s) );
						tk = token(s);
					}
					if( tk != TDoubleDot )
						unexpected(tk);
					var a1 = new Array();
					while( true ) {
						tk = token(s);
						if( Type.enumEq(tk,TId("default")) || Type.enumEq(tk,TId("case")) || tk == TBrClose )
							break;
						tokens.add(tk);
						a1.push(parseFullExpr(s));
					}
					a.push( ECase( matchArr, a1 ) );
				} else if( Type.enumEq(tk,TId("default")) ) {
					tk = token(s);
					if( tk != TDoubleDot )
						unexpected(tk);
					var a1 = new Array();
					while( true ) {
						tk = token(s);
						if( Type.enumEq(tk,TId("default")) || Type.enumEq(tk,TId("case")) || tk == TBrClose )
							break;
						tokens.add(tk);
						a1.push(parseFullExpr(s));
					}
					a.push( EDefault( a1 ) );
				} else
					break;
			}
			ESwitch( e1, EBlock(a) );
		case "var":
			var tk = token(s);
			var v = null;
			var e = null;
			var get = null;
			var set = null;
			var ident = null;
			var a = new Array();
			while( true ) {
				ident = null;
				e = null;
				switch(tk) {
				case TId(id): ident = id;
				default: unexpected(tk);
				}
				tk = token(s);
				//test for g/setters here. todo..improve
				if( tk == TPOpen ) {
					get = tokenString(token(s));
					tk = token(s); // swallow comma
					set = tokenString(token(s));
					tk = token(s); // swallow TPClose
					tk = token(s);
				}
				v = getDoubleDotType(s,tk);
				if( v != null )
					tk = token(s);
				if( Type.enumEq(tk,TOp("=")) ) {
					e = parseExpr(s);
					tk = token(s);
					if( tk == TSemicolon )
						tokens.add(tk);
				} else
					if( tk != TComma )
						tokens.add(tk);
				
				if( tk == TComma ) {
					a.push( ETyped( EVar(ident,get,set,e), v ) );
					tk = token(s);
				} else
					break;
			}
			if( a.length > 0 ) {
				a.push( ETyped( EVar(ident,get,set,e), v ) );
				EVarCollection( a );
			} else
				ETyped( EVar(ident,get,set,e), v );
		case "while":
			var econd = parseExpr(s);
			EWhile( econd, parseExpr(s) );
		case "do":
			var tk;
			var e = parseExpr(s);
			if( ! isBlock(e) )
				tk = token(s);
			tk = token(s);
			if( ! Type.enumEq(tk,TId("while")) )
				unexpected(tk);
			EDoWhile( e, parseExpr(s) );
		case "for":
			var tk = token(s);
			if( tk != TPOpen ) unexpected(tk);
			tk = token(s);
			var vname = null;
			switch( tk ) {
			case TId(id): vname = id;
			default: unexpected(tk);
			}
			tk = token(s);
			if( !Type.enumEq(tk,TId("in")) ) unexpected(tk);
			var eiter = parseExpr(s);
			tk = token(s);
			if( tk != TPClose ) unexpected(tk);
			EFor(vname,eiter,parseExpr(s));
		case "cast" :
			var tk = token(s);
			if( tk == TPOpen ) {
				var e = parseExpr(s);
				tk = token(s); // swallow ','
				var type = getVectorPath(s);
				tk = token(s); // swallow ')'
				ECast( e, type );
			} else {
				tokens.add(tk);
				ECast( parseExpr(s), null );
			}
		case "break": EBreak;
		case "continue": EContinue;
		case "else": unexpected(TId(id));
		case "function":
			var tk = token(s);
			tokens.add(tk);
			var name = getVectorPath(s);
			tk = token(s);
			//tok on TPOpen here
			var args = parseExprTypedList( s, TPClose );
			tk = token(s);
			var fType = getDoubleDotType(s, tk);
			if( fType != null )
				tk = token(s);
			tokens.add(tk);
			var e = ( tk == TBrOpen ) ? parseExpr(s) : null;
			ETyped( EFunction(args,e,name), fType );
		case "return":
			var tk = token(s);
			tokens.add(tk);
			EReturn( if( tk == TSemicolon ) null else parseExpr(s) );
		case "new":
			var a = new Array();
			var v = getVectorPath(s);
			var tk = token(s);
			ENew( v, parseExprList(s,TPClose) );
		case "throw":
			EThrow( parseExpr(s) );
		case "try":
			var e = parseExpr(s);
			var tk = token(s);
			if( ! isBlock(e) && ! Type.enumEq(tk,TId("catch")) )
				tk = token(s);
			var catchArr = new Array();
			while( Type.enumEq(tk,TId("catch")) ) {
				tk = token(s); // swallow '('
				var args = parseExprTypedList(s,TPClose);
				if( args.length != 1 )
					unexpected_msg( "wrong number of catch params : " + args );
				catchArr.push( ECatch( args[0], parseExpr(s) ) );
				tk = token(s);
			}
			tokens.add(tk);
			ETry( e, catchArr );
		default:
			null;
		}
	}
	
	private function unopSuffixCheck( e : Expr ) {
		if( isBlock(e) )
			return true;
		return switch( e ) {
			case EParent(_): true;
			default: false;
		}
	}

	function parseExprNext( s : haxe.io.Input, e1 : Expr ) {
		var tk = token(s);
		switch( tk ) {
		case TOp(op):
			for( x in unopsSuffix )
				if( x == op ) {
					if( unopSuffixCheck(e1) ) { // covers: function() {} ++i; if(true) --i; etc
						tokens.add(tk);
						return e1;
					}
					return parseExprNext(s, EUnop(op,false,e1)); // covers: if( i++ > 0 )
				}
			return makeBinop(op,e1,parseExpr(s));
		case TId(id):
			tokens.add(tk);
			return e1;
		case TDot:
			tk = token(s);
			var field = null;
			switch(tk) {
			case TId(id): field = id;
			default: unexpected(tk);
			}
			return parseExprNext(s,EField(e1,field));
		case TPOpen:
			return parseExprNext(s,ECall(e1,parseExprList(s,TPClose)));
		case TBkOpen:
			var e2 = parseExpr(s);
			tk = token(s);
			if( tk != TBkClose ) unexpected(tk);
			return parseExprNext(s,EArray(e1,e2));
		case TQuestion:
			var e2 = parseExpr(s);
			tk = token(s);
			if( tk != TDoubleDot ) unexpected(tk);
			var e3 = parseExpr(s);
			return EIf(e1,e2,e3,true);
		default:
			tokens.add(tk);
			return e1;
		}
	}
	
	function parseExprTypedList( s : haxe.io.Input, etk ) {
		var args = new Array<Expr>();
		var tk = token(s);
		if( tk == etk )
			return args;
		var ident = null;
		var idType = null;
		var val = null;
		var optional = false;
		while( true ) {
			if( tk == TQuestion ) {
				optional = true;
				tk = token(s);
			}
			switch( tk ) {
			case TId(id): ident = id;
			default: unexpected(tk);
			}
			tk = token(s);
			idType = getDoubleDotType(s, tk);
			if( idType != null )
				tk = token(s);
			if( Type.enumEq(tk,TOp("=")) ) {
				val = parseExpr(s);
				tk = token(s);
			}
			if( Type.enumEq(tk,etk) ) {
				if( ident != null ) {
					var e = if( val != null )
						ETyped( EBinop( "=", EIdent(ident), val ), idType );
					else
						ETyped( EIdent(ident), idType );
					if( optional )
						e = EOptional( e );
					args.push( e );
				}
				break;
			}
			switch( tk ) {
			case TComma:
				var e = if( val != null )
					ETyped( EBinop( "=", EIdent(ident), val ), idType );
				else
					ETyped( EIdent(ident), idType );
				if( optional )
					e = EOptional( e );
				args.push( e );
			default: unexpected(tk);
			}
			ident = null;
			idType = null;
			val = null;
			optional = false;
			tk = token(s);
		}
		return args;
	}

	function parseExprList( s : haxe.io.Input, etk ) {
		var args = new Array();
		var tk = token(s);
		if( tk == etk )
			return args;
		tokens.add(tk);
		while( true ) {
			args.push(parseExpr(s));
			tk = token(s);
			switch( tk ) {
			case TComma:
			default:
				if( tk == etk ) break;
				unexpected(tk);
			}
		}
		return args;
	}

	function readChar( s : haxe.io.Input ) {
		return try s.readByte() catch( e : Dynamic ) 0;
	}

	function readString( s : haxe.io.Input, until ) {
		var c;
		var b = new StringBuf();
		var esc = false;
		var old = line;
		while( true ) {
			try {
				c = s.readByte();
			} catch( e : Dynamic ) {
				line = old;
				throw Error.EUnterminatedString;
			}
			if( esc ) {
				esc = false;
				switch( c ) {
				case 110: b.addChar(92); b.addChar(110); //b.addChar(10); // \n
				case 114: b.addChar(92); b.addChar(114); //b.addChar(13); // \r
				case 116: b.addChar(92); b.addChar(116); //b.addChar(9); // \t
				case 39: b.addChar(92); b.addChar(39); // \'
				case 34: b.addChar(92); b.addChar(34); // \"
				case 92: b.addChar(92); b.addChar(92); // \\
				case 117: b.addChar(92); b.addChar(117); // \u
				case 120: b.addChar(92); b.addChar(120); // \x
				default: throw Error.EInvalidChar(c);
				}
			} else if( c == 92 )
				esc = true;
			else if( c == until )
				break;
			else {
				if( c == 10 ) line++;
				b.addChar(c);
			}
		}
		return b.toString();
	}
	
	function returnToken( tok : Token, ?inc : Int ) {
		currentWhite = [];
		if( inc == null ) inc = 1;
		if( tok != TSemicolon ) t = t + inc; 
		return tok;
	}
	
	function updateComment( char : Int ) {
		if( currentComment != null )
			currentComment.post.push( char );
		else
			currentWhite.push(char);
	}

	function token( s : haxe.io.Input ) {
		if( !tokens.isEmpty() )
			return tokens.pop();
		var char;
		if( this.char == null )
			char = readChar(s);
		else {
			char = this.char;
			this.char = null;
		}
		while( true ) {
			if( char == 32 || char == 9 || char == 13 || char == 10 ) {
				updateComment( char );
			} else {
				if( currentComment != null ) {
					commentMap.set( currentComment.key, currentComment );
					currentComment = null;
					currentWhite = [];
				}
			}
			switch( char ) {
			case 0: return returnToken( TEof );
			case 32,9,13: // space, tab, CR
			case 10: line++; // LF
			case 48,49,50,51,52,53,54,55,56,57: // 0...9
				var n = char - 48;
				var exp = 0;
				var z = (char == 48) ? "0" : "";
				while( true ) {
					char = readChar(s);
					exp *= 10;
					switch( char ) {
					case 48,49,50,51,52,53,54,55,56,57:
						n = n * 10 + (char - 48);
					case 46:
						if( exp > 0 ) {
							// in case of '...'
							if( exp == 10 && readChar(s) == 46 ) {
								tokens.add(TOp("..."));
								if( n == 0 ) z = "";
								return returnToken( TConst( CInt( z + Std.string(n) ) ), 2 );
							}
							throw Error.EInvalidChar(char);
						}
						exp = 1;
					case 120: // x
						if( n > 0 || exp > 0 )
							throw Error.EInvalidChar(char);
						// read hexa
						var hStr = "0x";
						while( true ) {
							char = readChar(s);
							switch( char ) {
							case 48,49,50,51,52,53,54,55,56,57: // 0-9
								n = (n << 4) + (char - 48); hStr += String.fromCharCode(char);
							case 65,66,67,68,69,70: // A-F
								n = (n << 4) + (char - 55); hStr += String.fromCharCode(char);
							case 97,98,99,100,101,102: // a-f
								n = (n << 4) + (char - 87); hStr += String.fromCharCode(char);
							default:
								this.char = char;
								return returnToken( TConst( CInt( hStr ) ) );
							}
						}
					default:
						this.char = char;
						if( n == 0 ) z = "";
						return returnToken( TConst( (exp > 0) ? CFloat( Std.string((n * 10 / exp)) ) : CInt( z + Std.string(n) ) ) );
					}
				}
			case 59: return returnToken( TSemicolon );
			case 40: return returnToken( TPOpen );
			case 41: return returnToken( TPClose );
			case 44: return returnToken( TComma );
			case 46:
				char = readChar(s);
				switch( char ) {
				case 48,49,50,51,52,53,54,55,56,57:
					var n = char - 48;
					var exp = 1;
					while( true ) {
						char = readChar(s);
						exp *= 10;
						switch( char ) {
						case 48,49,50,51,52,53,54,55,56,57:
							n = n * 10 + (char - 48);
						default:
							this.char = char;
							return returnToken( TConst( CFloat( Std.string(n/exp) ) ) );
						}
					}
				case 46:
					char = readChar(s);
					if( char != 46 )
						throw Error.EInvalidChar(char);
					return returnToken( TOp("...") );
				default:
					this.char = char;
					return returnToken( TDot );
				}
			case 123: return returnToken( TBrOpen );
			case 125: return returnToken( TBrClose );
			case 91: return returnToken( TBkOpen );
			case 93: return returnToken( TBkClose );
			case 39: return returnToken( TConst( CString("'"+readString(s,39)+"'") ) ); // '
			case 34: return returnToken( TConst( CString('"'+readString(s,34)+'"') ) ); // "
			case 63: return returnToken( TQuestion );
			case 58: return returnToken( TDoubleDot );
			default:
				if( ops[char] ) {
					var op = String.fromCharCode(char);
					var isEreg = false;
					var prev = null;
					var fwdslash = false;
					while( true ) {
						char = readChar(s);
						if( op == "~/") isEreg = true;
						if( isEreg ) {
							if( char == 47 && prev != 92 ) // 47 = '/' .. 92 = '\'
								fwdslash = true; 
							if( char == 59 && fwdslash ) { // 59 = ';'
								op = StringTools.rtrim(op); //TODO: remove \n\r\t also
								tokens.add( returnToken( TSemicolon ) );
								return returnToken( TConst(CEReg(op)) );
							}
						} else {
							if( !ops[char] ) {
								if( op.charCodeAt(0) == 47 )
									return tokenComment(s,op,char);
								this.char = char;
								return returnToken( TOp(op) );
							}
						}
						prev = char;
						op += String.fromCharCode(char);
					}
				}
				if( idents[char] || char == 35 ) { // 35 added for conditional compilation
					var id = String.fromCharCode(char);
					while( true ) {
						char = readChar(s);
						if( !idents[char] ) {
							this.char = char;
							return returnToken( TId(id) );
						}
						id += String.fromCharCode(char);
					}
				}
				throw Error.EInvalidChar(char);
			}
			char = readChar(s);
		}
		return null;
	}

	function tokenComment( s : haxe.io.Input, op : String, char : Int ) {
		var c = op.charCodeAt(1);
		var comment = new StringBuf();
		comment.add(op);
		if( c == 47 ) { // comment
			try {
				while( char != 10 && char != 13 ) {
					comment.add(String.fromCharCode(char));
					char = s.readByte();
				}
				this.char = char;
			} catch( e : Dynamic ) {
			}
			var key = Std.string(t);
			if( commentMap.exists(Std.string(t)) )
				key += "_"+Std.string(++subCommentCount);
			else
				subCommentCount = 0;	
			currentComment = {
				pre : currentWhite.slice(0,currentWhite.length),
				text : comment.toString(),
				multiline : false,
				post : [],
				key : key
			};
			commentMap.set(key, currentComment);
			currentWhite = [];
			return token(s);
		}
		if( c == 42 ) { /* comment */
			var old = line;
			try {
				while( true ) {
					while( char != 42 ) {
						if( char == 10 ) line++;
						comment.add(String.fromCharCode(char));
						char = s.readByte();
					}
					comment.add(String.fromCharCode(char));
					char = s.readByte();
					if( char == 47 ) {
						comment.add(String.fromCharCode(char));
						break;
					}
				}
			} catch( e : Dynamic ) {
				line = old;
				throw Error.EUnterminatedComment;
			}
			var key = Std.string(t);
			if( commentMap.exists(Std.string(t)) )
				key += "_"+Std.string(++subCommentCount);
			else
				subCommentCount = 0;	
			currentComment = {
				pre : currentWhite.slice(0,currentWhite.length),
				text : comment.toString(),
				multiline : true,
				post : [],
				key : key
			};
			commentMap.set(key, currentComment);
			currentWhite = [];
			return token(s);
		}
		this.char = char; // covers: 'var a = 5/2;' .. where there is no space between '/' and next token
		return returnToken( TOp(op) );
	}

	function constString( c ) {
		return switch(c) {
		case CInt(v): Std.string(v);
		case CFloat(f): Std.string(f);
		case CString(s): s; // TODO : escape + quote
		case CEReg(s) : s;
		}
	}

	function tokenString( t ) {
		return switch( t ) {
		case TEof: "<eof>";
		case TConst(c): constString(c);
		case TId(s): s;
		case TOp(s): s;
		case TPOpen: "(";
		case TPClose: ")";
		case TBrOpen: "{";
		case TBrClose: "}";
		case TDot: ".";
		case TComma: ",";
		case TSemicolon: ";";
		case TBkOpen: "[";
		case TBkClose: "]";
		case TQuestion: "?";
		case TDoubleDot: ":";
		}
	}

}
