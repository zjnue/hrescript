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
package output.hx;

import output.hx.Writer;
import input.hx.Expr;
import input.hx.Parser;

class WriteUtil {
	
	static var inst : WriteUtil;
	
	var indent : Int;
	var indentStr : String;
	var newlineStr : String;
	var semiStr : String;
	var skipNewLine : Bool;
	var skipIndent : Bool;
	var swallowEmptyBracketWhite : Bool;
	var swallowEmptyBlockBracketWhite : Bool;
	
	private function new() {
		indent = 0;
		indentStr = "\t";
		newlineStr = "\n";
		semiStr = ";";
		skipNewLine = false;
		skipIndent = false;
		swallowEmptyBracketWhite = true;
		swallowEmptyBlockBracketWhite = true;
	}
	
	public static function getInstance() : WriteUtil {
		if( inst == null )
			inst = new WriteUtil();
		return inst;
	}
	
	public function explode( a : Array<Content>, comments : Hash<TComment> ) {
		var aCount = 0;
		var tCount = 0;
		var out = new StringBuf();
		var commentAfterSemi = false;
		out.add( comment( tCount, aCount, a, comments, indent ) );
		var skipWhiteChar = null;
		for( i in a ) {
			switch(i) {
				case Tok(s):
					skipWhiteChar = null;
					skipNewLine = false;
					skipIndent = false;
					out.add( s );
					tCount++;
					var nextNonWhite = getNextNonWhite( a, aCount+1 );
					if( nextNonWhite == Semi )
						commentAfterSemi = true;
					else {
						commentAfterSemi = false;
						if( swallowEmptyBlockBracketWhite )
							if( s == "[" && Type.enumEq(nextNonWhite,Tok("]")) ) skipWhiteChar = "]";
						if( swallowEmptyBracketWhite )
							if( s == "(" && Type.enumEq(nextNonWhite,Tok(")")) ) skipWhiteChar = ")";
						var c = comment( tCount, aCount, a, comments, indent );
						if( s == "{" && c == "" && Type.enumEq(nextNonWhite,Tok("}")) ) {
							skipWhiteChar = "}";
							skipIndent = true;
						}
						out.add( c );
					}
				case Indent(count):
					indent = count;
					if( ! skipIndent )
						for( j in 0...count )
							out.add( indentStr );
				case White(e,prop): 
					if( skipWhiteChar == null )
						out.add( getWhite(e,prop) );
				case Semi:
					out.add( semiStr );
					if( commentAfterSemi )
						out.add( comment( tCount, aCount, a, comments, indent ) );
				case Newline:
					if( ! skipNewLine )
						out.add( newlineStr );
			}
			aCount++;
		}
		return out.toString();
	}
	
	private function getNextNonWhite( a : Array<Content>, aCount : Int ) {
		for( i in aCount...a.length ) {
			switch(a[i]) {
				case Indent(_), White(_,_), Newline:
				default: return a[i];
			}
		}
		return null;
	}
	
	private function getNextIndentBeforeNonWhite( a : Array<Content>, aCount : Int ) {
		for( i in aCount...a.length ) {
			switch(a[i]) {
				case Indent(ind): return ind;
				case White(_,_), Newline:
				default: return null;
			}
		}
		return null;
	}
	
	private function getLastIndentBeforeNonWhite( a : Array<Content>, aCount : Int ) {
		var out = null;
		for( i in aCount...a.length ) {
			switch(a[i]) {
				case Indent(ind): out = ind;
				case White(_,_), Newline:
				default: break;
			}
		}
		return out;
	}
	
	private function numFollowingBreaks( a : Array<Content>, aCount : Int ) {
		var num = 0;
		for( i in aCount...a.length ) {
			switch(a[i]) {
				case Indent(_), White(_,_):
				case Newline: num++;
				default: return num;
			}
		}
		return num;
	}
	
	private function comment( tCount : Int, aCount : Int, arr : Array<Content>, comments : Hash<TComment>, indent : Int, ?inln : Bool ) {
		if( inln == null ) inln = false;
		var a = new Array();
		var majorCommentIndx : Int = tCount;
		var minorCommentIndx : Int = 0;
		var key : String = Std.string( majorCommentIndx );
		while( comments.exists( key ) ) {
			var tc = comments.get( key );
			var preBreaks = Std.int( Math.min( 2, getNumLinebreaks(tc.pre) ) );
			var postBreaks = Std.int( Math.min( 2, getNumLinebreaks(tc.post) ) );
			var nextInd = getNextIndentBeforeNonWhite( arr, aCount+1 );
			var ind = indent;
			if( nextInd != null && nextInd > ind )
				ind = nextInd;
			var lastInd = getLastIndentBeforeNonWhite( arr, aCount+1 );
			var last = ( lastInd != null ) ? lastInd : indent;
			var nextKey = Std.string( majorCommentIndx ) + "_" + Std.string( minorCommentIndx + 1 );
			var followingIsComment = comments.exists( nextKey );
			if( tc.multiline ) {
				if( minorCommentIndx == 0 )
					a.push( if( preBreaks == 0 ) " " else getBreakStr(preBreaks) + getIndent( ind ) );
				a.push( tc.text.split(String.fromCharCode(13)).join("") );
				if( postBreaks == 0 )
					a.push( "" );
				else {
					a.push( getBreakStr( postBreaks ) + getIndent( if( followingIsComment ) ind else last ) );
					skipNewLine = true;
					skipIndent = true;
				}
			} else {
				if( minorCommentIndx == 0 )
					a.push( if( preBreaks == 0 ) " " else getBreakStr(preBreaks) + getIndent( ind ) );
				a.push( tc.text.split(String.fromCharCode(13)).join("") );
				a.push( getBreakStr( postBreaks ) + getIndent( if( followingIsComment ) ind else last ) );
				skipNewLine = true;
				skipIndent = true;
			}
			key = Std.string( majorCommentIndx ) + "_" + Std.string( ++minorCommentIndx );
		}
		return a.join("");
	}
	
	private function getNumLinebreaks( charArr : Array<Int> ) {
		var num = 0;
		for( c in charArr )
			if( c == 10 )
				num++;
		return num;
	}
	
	private function getIndent( indent : Int ) {
		var out = "";
		for( i in 0...indent )
			out += indentStr;
		return out;
	}
	
	private function getBreakStr( num : Int ) {
		var out = "";
		for( i in 0...num )
			out += "\n";
		return out;
	}
	
	//// TODO : move elsewhere; use CSS
	private function getWhite( e : Expr, prop : String ) {
		switch( e ) {
			/*
			case EParent(_): if( prop == "preLeftBracket" ) return "";
			case ECall(_,_): if( prop == "preLeftBracket" ) return "";
			case EFor(_,_,_): if( prop == "preLeftBracket" ) return "";
			case EFunction(_,_,_): if( prop == "preLeftBracket" ) return "";
			case ENew(_,_): if( prop == "preLeftBracket" ) return "";
			case ECast(_,_): if( prop == "preLeftBracket" ) return "";
			*/
			case EFunction(_,b,_): if( b == null && prop == "preLeftBrace" ) return "";
			case EArrayDecl(_):
				switch(prop) {
					case "postLeftBlockBracket", "preRightBlockBracket": return " ";
					default:
				}
			default:
		}
		switch( prop ) {
			case "preLeftBlockBracket", "postLeftBlockBracket", "preRightBlockBracket": return "";
			case "preLeftAngleBracket", "postLeftAngleBracket", "preRightAngleBracket": return "";
			case "preLeftBracket": return "";
			default:
		}
		return " ";
	}
	
	
	
}
