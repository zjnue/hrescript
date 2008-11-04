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
package input.as2;
import input.as2.Expr;

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
	
	var commentMap : Hash<String>;
	var t : Int; // accurate indication of tokens parsed
	var subCommentCount : Int;

	public function new() {
		line = 1;
		opChars = "+*/-=!><&|^%~";
		identChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$";
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
		var a = new Array();
		while( true ) {
			var tk = token(s);
			if( tk == TEof ) break;
			tokens.add(tk);
			a.push(parseFullExpr(s));
		}
		//trace("parse: a =  "+ Std.string(a));
		//for( c in commentMap.keys() )
			//trace("\t tok = " + c + " comment = " +commentMap.get(c));
		return if( a.length == 1 ) a[0] else EBlock(a);
	}
	
	public function getComments() : Hash<String> {
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
		case EVar(_,e): e != null && isBlock(e);
		case EIf(_,e1,e2,_): if( e2 != null ) isBlock(e2) else isBlock(e1);
		case EBinop(_,_,e): isBlock(e);
		case EUnop(_,prefix,e): !prefix && isBlock(e);
		case EWhile(_,e): isBlock(e);
		case EFor(_,_,_,e): isBlock(e);
		case EForIn(_,_,e): isBlock(e);
		case EReturn(e): e != null && isBlock(e);
		case EClass(_,_,_,e): isBlock(e);
		case EWith(_,e): isBlock(e);
		case ESwitch(_,e): isBlock(e);
		case ETyped(e,_): isBlock(e);
		case EModifier(e,_): isBlock(e);
		default: false;
		}
	}

	function parseFullExpr(s) {
		var e = parseExpr(s);
		var tk = token(s);
		//trace("parseFullExpr: e = "+ e + " tk = "+ tk);
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
		var str = "";
		var t = token(s);
		var dot = true;
		while( true ) {
			switch(t) {
			case TId(id): 
				if( dot ) {
					str += tokenString(t); t = token(s);
					dot = false;
				} else
					break;
			case TDot: str += tokenString(t); t = token(s); dot = true;
			default: break;
			}
		}
		tokens.add(t);
		return str;
	}
	
	function getTypeStr( s : haxe.io.Input, tk : Token ) {
		if( Type.enumEq(tk,TDoubleDot) )
			return getDotPath( s );
		return null;
	}
	
	function parseStructure( s, id ) {
		trace("parseStructure: id = "+ id);
		return switch( id ) {
			
		case "public": EModifier( parseExpr(s), "public" );
		case "private": EModifier( parseExpr(s), "private" );
		case "static": EModifier( parseExpr(s), "static" );
		case "typeof": ETypeof( parseExpr(s) );
		case "with":
			var cond = parseExpr(s);
			var e = parseExpr(s);
			EWith( cond, e );
		case "class":
			var n = getDotPath(s);
			var tk = token(s);
			var ext = null;
			if( Type.enumEq(tk,TId("extends")) )
				ext = getDotPath(s);
			var i = new Array();
			if( ext != null )
				tk = token(s);
			if( Type.enumEq(tk,TId("implements")) ) {
				while( tk != TBrOpen ) {
					i.push( getDotPath(s) );
					tk = token(s);
				}
				tokens.add(tk);
			}
			if( i.length > 0 ) {
				tk = token(s);
				tokens.add(tk);
			} else if( tk == TBrOpen )
				tokens.add(tk);
			var e = parseExpr(s);
			EClass( n, ext, i, e);
		case "interface":
			var n = getDotPath(s);
			var tk = token(s);
			var i = new Array();
			if( Type.enumEq(tk,TId("extends")) ) {
				while( tk != TBrOpen ) {
					i.push( getDotPath(s) );
					tk = token(s);
				}
				tokens.add(tk);
			}
			if( i.length > 0 ) {
				tk = token(s);
				tokens.add(tk);
			} else if( tk == TBrOpen )
				tokens.add(tk);
			var e = parseExpr(s);
			EInterface( n, i, e);
		case "import": EImport( getDotPath(s) );
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
			EIf(cond,e1,e2,false);
		case "switch":
			var e1 = parseExpr(s);
			var tk = token(s);
			if( tk != TBrOpen )
				unexpected(tk);

			tk = token(s);
			var a = new Array();
			var d = null;
			while( true ) {
				if( Type.enumEq(tk,TId("case")) ) {
					var e1 = parseExpr(s);
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
					a.push( ECase( e1, EBlock( a1 ) ) );
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
					a.push( EDefault( EBlock( a1 ) ) );
				} else
					break;
			}
			ESwitch( e1, EBlock(a) );
		case "var":
			var tk = token(s);
			var ts = null;
			var e = null;
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
				ts = getTypeStr(s,tk);
				if( ts != null )
					tk = token(s);
				if( Type.enumEq(tk,TOp("=")) ) {
					e = parseExpr(s);
					tk = token(s);
					if( tk == TSemicolon )
						tokens.add(tk);
				}
				else
					if( tk != TComma )
						tokens.add(tk);
				
				if( tk == TComma ) {
					a.push( ETyped( EVar(ident,e), ts ) );
					tk = token(s);
				} else
					break;
			}
			if( a.length > 0 ) {
				a.push( ETyped( EVar(ident,e), ts ) );
				EVarCollection( a );
			} else
				ETyped( EVar(ident,e), ts );
		case "while":
			var econd = parseExpr(s);
			var e = parseExpr(s);
			EWhile( econd, e );
		case "do":
			var e = parseExpr(s);
			var tk = token(s);
			if( Type.enumConstructor(e) != "EBlock" )
				tk = token(s);
			if( ! Type.enumEq(tk,TId("while")) )
				unexpected(tk);
			tk = token(s);
			var econd = parseExpr(s);
			tk = token(s);
			EDoWhile(e,econd);
		case "for":
			var tk = token(s);
			if( tk != TPOpen ) unexpected(tk);
			tk = token(s);
			tokens.add(tk);
			var e1 = null;
			if( tk != TSemicolon )
				e1 = parseExpr(s);
			tk = token(s);
			if( Type.enumEq(tk,TId("in")) ) {
				var e2 = parseExpr(s);
				tk = token(s);
				if( tk != TPClose ) unexpected(tk);
				EForIn(e1,e2,parseExpr(s));
			} else {
				var a = new Array();
				a[0] = new Array();
				if( e1 != null )
					a[0].push( e1 );
				var indx = 0;
				while( true ) {
					if( tk == TPClose )
						break;
					if( tk == TSemicolon )
						a[++indx] = new Array();
					tk = token(s);
					if( tk == TSemicolon )
						continue;
					if( tk == TPClose )
						break;
					else {
						tokens.add(tk);
						var ee = parseExpr(s);
						a[indx].push( ee );
						tk = token(s);
					}	
				}
				var e = parseExpr(s);
				EFor(a[0],a[1],a[2],e);
			}
		case "break": EBreak;
		case "continue": EContinue;
		case "else": unexpected(TId(id));
		case "function":
			var tk = token(s);
			var name = null;
			var getset = null;
			switch( tk ) {
			case TId(id): name = id; tk = token(s);
			default:
			}
			if( tk != TPOpen ) {
				if( name == "get" || name == "set" ) {
					getset = name;
					switch( tk ) {
					case TId(id): name = id; tk = token(s);
					default:
					}
					if( tk != TPOpen )
						unexpected(tk);
				} else
					unexpected(tk);
			}
			var args = new Array<Expr>();
			tk = token(s);
			if( tk != TPClose ) {
				var ident = null;
				var idType = null;
				while( true ) {
					switch( tk ) {
					case TId(id): ident = id;
					default: unexpected(tk);
					}
					tk = token(s);
					idType = getTypeStr(s,tk);
					if( idType != null )
						tk = token(s);
					switch( tk ) {
					case TComma:
						args.push( ETyped(EIdent(ident),idType) );
					case TPClose: 
						if( ident != null )
							args.push( ETyped(EIdent(ident),idType) );
						break;
					default: unexpected(tk);
					}
					ident = idType = null;
					tk = token(s);
				}	
			}
			tk = token(s);
			var fType = getTypeStr(s,tk);
			if( fType != null )
				tk = token(s);
			tokens.add(tk);
			var e = ( tk == TBrOpen ) ? parseExpr(s) : null;
			if( getset != null )
				ETyped( EGetSet( EFunction(args,e,name), getset ), fType );
			else
				ETyped( EFunction(args,e,name), fType );
		case "return":
			var tk = token(s);
			tokens.add(tk);
			EReturn(if( tk == TSemicolon ) null else parseExpr(s));
		case "new": ENew( parseExpr(s) );
		case "throw": EThrow( parseExpr(s) );
		case "try":
			var e = parseExpr(s);
			var tk = token(s);
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
		case "delete": // delete a, b; possible ...
			var a = new Array();
			var tk = token(s);
			tokens.add(tk);
			while( true ) {
				a.push( parseExpr(s) );
				tk = token(s);
				if( tk == TSemicolon )
					break;
			} 
			EDelete( a );
		default:
			null;
		}
	}

	function parseExprNext( s : haxe.io.Input, e1 : Expr ) {
		var tk = token(s);
		//trace("parseExprNext: tk = " +tk);
		switch( tk ) {
		case TOp(op):
			for( x in unopsSuffix )
				if( x == op ) {
					if( isBlock(e1) ) { // covers: function() {} ++i;
						tokens.add(tk);
						return e1;
					}
					return parseExprNext(s, EUnop(op,false,e1)); // covers: if( i++ > 0 )
				}
			return makeBinop(op,e1,parseExpr(s));
		case TId(id):
			if( id == "instanceof" )
				return EBinop(id,e1,parseExpr(s));
			else {
				tokens.add(tk);
				return e1;
			}
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
		while( true ) {
			switch( tk ) {
			case TId(id): ident = id;
			default: unexpected(tk);
			}
			tk = token(s);
			idType = getTypeStr(s, tk);
			if( idType != null )
				tk = token(s);
			if( Type.enumEq(tk,etk) ) {
				if( ident != null )
					args.push( ETyped(EIdent(ident),idType) );
				break;
			}
			switch( tk ) {
			case TComma:
				args.push( ETyped(EIdent(ident),idType) );
			default: unexpected(tk);
			}
			ident = null;
			idType = null;
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
				case 114:  b.addChar(92); b.addChar(114); //b.addChar(13); // \r
				case 116:  b.addChar(92); b.addChar(116); //b.addChar(9); // \t
				case 39:  b.addChar(92); b.addChar(39); //b.addChar(39); // \'
				case 34:  b.addChar(92); b.addChar(34); //b.addChar(34); // \"
				case 92:  b.addChar(92); b.addChar(92); //b.addChar(92); // \\
				case 117:  b.addChar(92); b.addChar(117); //b.addChar(117); // \u
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
	
	function returnToken( tok : Token ) {
		if( tok != TSemicolon )
			t++; 
		return tok;
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
								return returnToken( TConst( CInt( z + Std.string(n) ) ) );
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
			//case 39: return returnToken( TConst( CString(readString(s,39)) ) );
			//case 34: return returnToken( TConst( CString(readString(s,34)) ) );
			//tmp fix
			case 39: return returnToken( TConst( CString("'"+readString(s,39)+"'") ) ); // '
			case 34: return returnToken( TConst( CString('"'+readString(s,34)+'"') ) ); // "
			case 63: return returnToken( TQuestion );
			case 58: return returnToken( TDoubleDot );
			default:
				if( ops[char] ) {
					var op = String.fromCharCode(char);
					while( true ) {
						char = readChar(s);
						if( !ops[char] ) {
							if( op.charCodeAt(0) == 47 )
								return tokenComment(s,op,char);
							this.char = char;
							return returnToken( TOp(op) );
						}
						op += String.fromCharCode(char);
					}
				}
				if( idents[char] ) {
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
				//tmp if
				if( char == 239 || char == 187 || char == 191 )
					trace("warning, chr " + char + " encountered");
				else
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
			if( commentMap.exists(Std.string(t)) )
				commentMap.set(Std.string(t)+"_"+Std.string(++subCommentCount), comment.toString());
			else {
				commentMap.set(Std.string(t), comment.toString());
				subCommentCount = 0;
			}
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
			if( commentMap.exists(Std.string(t)) )
				commentMap.set(Std.string(t)+"_"+Std.string(++subCommentCount), comment.toString());
			else {
				commentMap.set(Std.string(t), comment.toString());
				subCommentCount = 0;
			}
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

