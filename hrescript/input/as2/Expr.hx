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
package hrescript.input.as2;

enum Const {
	CInt( s : String );
	CFloat( s : String );
	CString( s : String );
}

enum Expr {
	EConst( c : Const );
	EIdent( v : String );
	EVar( n : String, ?e : Expr );
	EParent( e : Expr );
	EBlock( e : Array<Expr> );
	EField( e : Expr, f : String );
	EBinop( op : String, e1 : Expr, e2 : Expr );
	EUnop( op : String, prefix : Bool, e : Expr );
	ECall( e : Expr, params : Array<Expr> );
	EIf( cond : Expr, e1 : Expr, ?e2 : Expr, tertiary : Bool );
	EWhile( cond : Expr, e : Expr );
	EFor( ea1 : Array<Expr>, ea2 : Array<Expr>, ea3 : Array<Expr>, e : Expr );
	EForIn( e1 : Expr, e2 : Expr, e : Expr );
	EBreak;
	EContinue;
	EFunction( params : Array<Expr>, e : Expr, ?name : String );
	EReturn( ?e : Expr );
	EArray( e : Expr, index : Expr );
	EArrayDecl( e : Array<Expr> );
	ENew( e : Expr );
	EThrow( e : Expr );
	ETry( e : Expr, catchArr : Array<Expr> );
	ECatch( err : Expr, e : Expr );
	ETyped( e : Expr, ?v : String );
	EModifier( e : Expr, f : String );
	EImport( v : String );
	EAObj( a : Array<Expr> );	// anonymous obj like { a : 10, b : 12 }
								// made to: EAObj( [EBinop(=,e1,e2), ...])
	EClass( n : String, ext : String, i : Array<String>, e : Expr );
	EGetSet( e : Expr, s : String );
	ESwitch( e1 : Expr, e : Expr );
	ECase( e1 : Expr, e : Expr );
	EDefault( e : Expr );
	EDoWhile( e : Expr, cond : Expr );
	EVarCollection( a : Array<Expr> );
	EInterface( n : String, i : Array<String>, e : Expr );
	EDelete( eArr : Array<Expr> );
	EWith( cond : Expr, e : Expr );
	ETypeof( e : Expr );
}

enum Error {
	EInvalidChar( c : Int );
	EUnexpected( s : String );
	EUnterminatedString;
	EUnterminatedComment;
	EUnknownVariable( v : String );
	EInvalidIterator( v : String );
	EInvalidOp( op : String );
	EInvalidAccess( f : String );
}
