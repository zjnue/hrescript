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
package output.as2;

import input.as2.Expr;
import input.as2.Parser;

class Writer{
	private var comments :Hash<String>;
	private var outputArray :Array<String>;
	
	private var tabArray :Array<String>;

	private var semiColon :Hash<String>;
	private var currentExprn :Expr;
	
	public function new () {
		outputArray = new Array<String> ();
		semiColon = new Hash<String>();
	}

	// Entry point. Pass the exprn array and comments array
	public function write ( expressions :Array<Expr>, comments :Hash<String> ) :String {
		switch ( expressions[0] ) {
			case EBlock ( v ) :
				expressions = v;
			default :
		}
		doExpr( expressions, comments );
		//Code formatting is done.
		//Insert comments
		doComments( comments );
		return (outputArray.join(""));
	}
	
	private function doExpr ( expressions :Array<Expr>, comments :Hash<String> ) {
		//Handle each exprn. A complete class is a single exprn.
		for(i in 0...expressions.length) {
			tabArray = new Array<String> ();
			var e = expressions[i];
			outputArray = outputArray.concat( parseExpressions ( e ) );
			outputArray.push(  getNewLine () );
		}
	}
	
	
	private function doComments ( comments :Hash<String> ) {
		var commentsTabArray :Array<String>;
		var tabStatus :Bool = false;
		var sortedKeys :Array<String> = new Array<String> ();
		// zeroBasedKeys array contains all the comments that appear before actual code (Indexed as 0,0_1,0_21 etc). i.e, beginning of the file.
		var zeroBasedKeys :Array<String> = new Array<String> ();
		//var tabInsertedAtIndex :Int = 0;
		// Push all the keys except 0 into sortedKeys array. 0 comment will be placed in zeroBasedKeys and appear at the beginning, if they exist
		// Zerobased comments are a bit different where they dont need any tabs/indentation and logic to check if they exist between stmnts or if they have to appear in new line.
		// So deal them seperately.
		for(key in comments.keys()){
			if(Std.parseInt (key) != 0){
				sortedKeys.push(key);
			}else{
				zeroBasedKeys.push(key);
			}
		}
		// Sort the arrays so that they have keys arrangement as below
		// [49_1, 49, 46, 37_2, 37_1, 37, 34]
		// sortKeysFunction method only sorts in ascending order...Just reverse the array to get it in above format.
		// Reversing is a must coz consider scenario with comments indexed 5_5, 5_4....5.
		// all these get inserted at index 5. The last comment (5_5) is the one that goes first then 5_4 so on..
		// So if we dont insert in descending order, they get inserted in reverse order like 5,5_1....5_5 where as Parser gives us 5 for the first comment and 5_1 for 2nd comment so on
		sortedKeys.sort( sortKeysFunction );
		sortedKeys.reverse();
		zeroBasedKeys.sort( sortKeysFunction );
		zeroBasedKeys.reverse();
		// Place comments automatically at the beginning of a file if they exist in 0th or 0_* index.
		// These comments appear at the beginning of the file.
		if(zeroBasedKeys.length > 0){
			for(t in 0...zeroBasedKeys.length ) {
				/* TO DO: For some reason, multiline comments get extra \n for \ns. Or May be WINDOWS uses \r\n.
				 * Jst remove \n and join it. It should not remove any newlines (charcode - 13) given in a comment but just the one inserted while parsing.
				*/
				//outputArray.insert( 0, comments.get( zeroBasedKeys[t] ) );
				outputArray.insert( 0, comments.get( zeroBasedKeys[t] ).split( String.fromCharCode (13) ).join("") );
				outputArray.insert( 1, getNewLine () );
			}
		}
		//Repeat thru all the comments.
		for ( key_index in 0...sortedKeys.length ) {
			var key = sortedKeys[key_index];
			commentsTabArray = new Array<String> ();
			var token_count:Int = 0;
			// Repeat thru the outputArray and find out the index where to insert the comment.
			// Comment key is not the index number in outputArray but a token number. So count the tokens and then find the position.  
			for(j in 0...outputArray.length){
				var eachToken :String = outputArray[j];
				//Exclude  getSpace () / getNewLine () / getTab ()/ comments  etc. coz they are not counted as tokens while parsing Comments.
				if(eachToken !=  getSpace ()  && eachToken !=  getNewLine ()  && eachToken !=  getTab () && eachToken !=  getSemicolon () && eachToken.substr(0,2) != "/*" && eachToken.substr(0,2) != "//"  ){
					token_count++;
				}
				//Checks if 'Val' is ' getTab () '. If so, clear the Tab Array and start filling it. That way, we will know the latest number of TABs in 'commentsTabArray'
				// After this, commentsTabArray will have exact number of TABS that were there for previous statement.
				if(eachToken ==  getTab () ){
					if(!tabStatus){
						commentsTabArray = new Array<String>();
					}
					tabStatus = true;
					commentsTabArray.push( getTab () );
				}else{
					tabStatus = false;
				}
				//After excluding  getSpace () / getNewLine () / getTab ()  etc, would get number of tokens. If that is equal to the 'key', that index is what the token number where the comment needs to be inserted, proceed.
				if ( token_count == Std.parseInt ( key ) ) {
						// Insert one more  getTab () if the next stmnt is a BLOCK.
						// TODO : Anonymous Objects will also have a TAB inserted if they have comments within them.
						if ( outputArray[j] == "{" /*&& tabInsertedAtIndex != j*/ ) {
							commentsTabArray.push( getTab () );
							//tabInsertedAtIndex = j;
						}
						// Find if the next token is a semicolon. If so, skip the index where we insert comments.
						// If the next token is a semicolon, assign 2 places after (1 for current index and 1 for semicolon and insert next)
						// If the next token is not a semicolon, assign 1 coz right after the current token, we can insert tabs/newline/comment
						var skipSemicolonIndex :Int = ( outputArray[j+1] == getSemicolon () ) ? 2 : 1;
						// If next line is also a NEWLINE ( there will be new line after every method ) which means a comment after a method, skip that newline too so that
						// comment appears after newline and right before a new method start.
						if(outputArray[j + skipSemicolonIndex] == getNewLine() && outputArray[j + skipSemicolonIndex+1] == getNewLine() ){
							skipSemicolonIndex = skipSemicolonIndex + 1;
						}  
						/* If comment is a multiline one and appears after } ; { OR single line comment, insert new line first, then insert a tabs and then the actual comment.
						 * else, in other words, if a comment is a multiline one but does not appear after { } ;, just insert the comment without newline/tabs 
						 * coz this type of comment is actually appearning between stmnts like below.
						 * function doSomething () { //someComments
						*/
						// else part hanldes var a:Int /* String */ = 20 scenarios - multiline but within a stmnt
						if ( ( comments.get(key).substr(0,2) == "/*" && ( outputArray[j] == "}" || outputArray[j+1] == getSemicolon() || outputArray[j] == "{" ) ) || comments.get(key).substr(0,2) == "//" ){
							//Comment with // or /* which appears after } ; { always appear in next line so Add a NewLine
							outputArray.insert( j + skipSemicolonIndex,  getNewLine () );
							//Before inserting the comments, insert correct number of TABs so that even comments have right Indentation.
							for (t in 0...commentsTabArray.length) {
								outputArray.insert( j + ( skipSemicolonIndex + 1 ) + t, commentsTabArray[t] );
							}
							// Insert the comment after inserting required No. of TABs.
							
							/* TO DO: For some reason, multiline comments get extra \n for \ns. Or May be WINDOWS uses \r\n.
							 * Jst remove \n and join it. It should not remove any newlines (charcode - 13) given in a comment but just the one inserted while parsing. 
							*/ 
							// outputArray.insert( j + ( skipSemicolonIndex + 1 ) + commentsTabArray.length, comments.get(key) );
							outputArray.insert( j + ( skipSemicolonIndex + 1 ) + commentsTabArray.length, comments.get(key).split( String.fromCharCode (13) ).join("") );
							/* Insert new line and Tabs after a singleline comment (//) only.
							 * This avoids any statements follow single-line comments blindly as below. 
							 * The opening brace should not follow the single line comment. 
							 * public function Tokenizer ( xpath :String )//comment 
								{
								}
							*/
							if ( comments.get(key).substr(0,2) == "//" ) {
								var nextIndex :Int = ( j+ ( skipSemicolonIndex + 1 ) + commentsTabArray.length ) + 1;
								if ( outputArray[nextIndex] != getNewLine() ) {
									outputArray.insert( nextIndex,  getNewLine () );
									for( t in 0...commentsTabArray.length ) {
										outputArray.insert( nextIndex + t + 1, commentsTabArray[t] );
									}
								}
							}
						} else {
							/* if a comment is a multiline one but does not appear after { } ;, just insert the comment without newline/tabs 
							 * coz this type of comment is actually appearning between stmnts like below.
							 */// var a:Int /* String */ = 20; 
							 
							/* TO DO: For some reason, multiline comments get extra \n for \ns. Or May be WINDOWS uses \r\n.
							 * Jst remove \n and join it. It should not remove any newlines (charcode - 13) given in a comment but just the one inserted while parsing.
							*/ 
							// outputArray.insert(j+skipSemicolonIndex, comments.get(key));
							outputArray.insert(j+skipSemicolonIndex, comments.get(key).split( String.fromCharCode (13) ).join(""));
						}
					break;
				}
			}
		}
	}

	// Called for every single exprn.
	private function parseExpressions ( e :Expr, ?context:Dynamic ) {
		if ( e == null )
			return [];
		switch ( e ) {
				case EImport ( v ) : 
					return SImport ( context, e );
				case EIf( cond, e1, e2, t ):
					return SIf(context,e);
				case EParent ( e1 ) :
					return SParent ( context, e );
				case EBinop ( op , e1 , e2 ) :
					return SBinop ( context, e );
				case EConst ( c ) :
					return SConst ( context, e );
				case EIdent ( v ) :
					return SIdent ( context, e );
				case EBlock ( v ) :
					return SBlock ( context, e );
				case ECall ( v, params ) :
					return SCall ( context, e );
				case EField ( e1, f ) :
					return SField ( context, e );
				case EFor ( e1, e2, e3, eb ) :
					return SFor ( context, e );
				case EUnop ( op, prefix, eb ) :
					return SUnop ( context, e );
				case ETyped ( ex, v ) :
					return STyped ( context, e );
				case EVar ( n, ex ) :
					return SVar ( context, e );
				case EWhile ( cond, e1 ) :
					return SWhile ( context, e );
				case EFunction ( params, e1, name ) :
					return SFunction ( context, e );
				case EReturn ( e1 ) :
					return SReturn ( context, e );
				case EArray ( e1, index ) :
					return SArray ( context,e );
				case EArrayDecl ( e1 ) :
					return SArrayDecl ( context, e );
				case EModifier( e1, f ) :
					return SModifier ( context, e );
				case EAObj ( a ) :
					return SAObj ( context, e );
				case EDoWhile ( e1, cond ):
					return SDoWhile ( context, e );
				case ESwitch ( e1, e2 ) :
					return SSwitch ( context, e );
				case ECase ( e1, e2 ) :
					return SCase ( context,e );
				case EDefault ( e1 ) :
					return SDefault ( context, e );
				case EForIn ( e1, e2, ex ) :
					return SForIn ( context, e );
				case EBreak :
					return SBreak ( context, e );
				case EContinue:
					return SContinue (context, e );
				case ENew( e1 ) :
					return SNew ( context, e );
				case EThrow( ex ) :
					return SThrow ( context, e );
				case ETry ( e1 , catchArr ) :
					return STry ( context, e );
				case ECatch ( err, e1 ) :
					return SCatch ( context, e );
				case EInterface ( n, i, ex ) :
					return SInterface ( context, e );
				case EVarCollection ( a ) :	
					return SVarCollection ( context, e );
				case EClass ( n, ext, i, ex ) :	
					return SClass ( context, e );
				case EGetSet ( e1, s ) :
					return SGetSet ( context, e );
				case EDelete ( e1 ) :
					return SDelete ( context, e );
				case EWith ( cond, e1 ) :
					return SWith ( context, e );
				case ETypeof ( e1 ) :
					return STypeof ( context, e );
				default :
					trace("default");
					return [];
		}
	}
	
	// Called when constants (10, "cat", 2.31) are used.
	private function parseConstants ( c :Const ) {
		switch ( c ) {
			case CInt ( s ) : return s;
			case CFloat ( s ) : return s;
			case CString ( s ) : return s;
		}
		return null;
	}

	// Called when constants (10, "cat", 2.31) are used.
	private function SConst ( contextObj : Dynamic, e : Expr  ) : Array<String>  {
		var arr = new Array ();
		switch ( e ) {
			case EConst ( c ) :
				arr.push( parseConstants ( c ) );
			default:
				trace("Const Error");
		}
		return arr;
	}
	
	// typeof (obj)
	private function STypeof ( contextObj : Dynamic, e : Expr ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ETypeof ( e1 ) :
				arr.push("typeof");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e1));
			default:
				trace("typeof Error");
		}
		return arr;
	}
	
	// with (obj) { }
	private function SWith ( contextObj : Dynamic, e : Expr ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EWith ( cond, e1 ) :
				arr.push("with");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(cond));
				arr.push( getSpace () );
				arr = insertTabsForSingleStatementStructures( arr, e1 );
			default:
				trace("With Error");
		}
		return arr;
	}
	
	// function set salary (){ }
	private function SGetSet ( contextObj : Dynamic, e : Expr  ): Array<String>  {
		var arr = new Array();
		switch ( e ) {
			case EGetSet ( e1 , s ):
					contextObj.from = "getSet";
					contextObj.getSet = s;
				arr = arr.concat(parseExpressions(e1,contextObj));

			default:
				trace("Const Error");
		}
		return arr;
	}
	
	// class A extends ABase implements I1,I2 { }
	private function SClass ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EClass( n, ext, i, ex ):
				arr.push( getNewLine () );
				arr.push("class");
				arr.push( getSpace () );
				arr = tokenizeDotStrings(n,arr);
				arr.push( getSpace () );
				if(ext != null){
					arr.push("extends");
					arr.push( getSpace () );
					arr = tokenizeDotStrings(Std.string(ext),arr);
					arr.push( getSpace () );
				}
				if(i.length != 0){
					arr.push("implements");
					arr.push( getSpace () );
					for(k in 0...i.length){
						var exp = i[k];
						arr.push(exp);
						if(k>=0 && k<i.length-1){
							arr.push( getComma () );
						}
					}
					arr.push( getSpace () );
				}
				arr = arr.concat(parseExpressions(ex));
			default:
				trace("Class Error");
		}
		return arr;
	}
	
	// interface I1 extends IBase1, IBase2 { }
	private function SInterface ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EInterface(n, i, ex):
				arr.push( getNewLine () );	
				arr.push("interface");
				arr.push( getSpace () );
				arr = tokenizeDotStrings(Std.string(n),arr);
				if(i.length>0){
					arr.push( getSpace () );
					arr.push("extends");
					arr.push( getSpace () );
					for(k in 0...i.length){
						var p = i[k];
						arr = tokenizeDotStrings(Std.string(p),arr);
						arr.push( getSpace () );
						if(k>=0 && k<i.length-1){
							arr.push( getComma () );
						}
					}
				}
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(ex));
			default:
				trace("interface Error");
		}
		return arr;
	}	

	// new Object ()
	private function SNew ( contextObj : Dynamic, e : Expr ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ENew(e1):
				arr.push("new");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e1));
			default:
				trace("new Error");
		}
		return arr;
	}

	// continue
	private function SContinue ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EContinue:
				arr.push("continue");
			default:
				trace("continue Error");
		}
		return arr;
	}
	
	// for (var prop in obj) { } 
	private function SForIn ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EForIn( e1, e2, ex ):
				arr.push("for");
				arr.push( getSpace () );
				arr.push("(");
				arr = arr.concat(parseExpressions(e1));
				arr.push( getSpace () );
				arr.push("in");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e2));
				arr.push( getSpace () );
				arr.push(")");
				arr.push( getSpace () );
				arr = insertTabsForSingleStatementStructures(arr,ex);
			default:
				trace("for in Error");
		}
		return arr;
	}
	
	// default : 
	 private function SDefault ( contextObj : Dynamic, e : Expr  ): Array<String> {
		  var arr = new Array<String>();
		  switch( e ) {
			case EDefault( e1 ):
				arr.push("default");
				arr.push( getSpace () );
				arr.push(":");
				switch( e1 ) {
						case EBlock( v ):
						//statments in a 'default' are returned as if in a block with EBlock[. But EBlock inserts '{' automatically in the begining. 
						//When developer has '{' for a 'default', by default Block functionality inserts '{{' which is not a good idea. So avoid sending first block.
						//Remove first block by default and send the next so if Block exists, it inserts { if not insert statements manually in a loop as below.
							if(v.length>0){
								switch( v[0] ) {
									case EBlock( cs ):
										arr = arr.concat(parseExpressions(v[0]));
									default:
										// though block and default/case write statementes in the same way. Tabbing varies. 
										// TO DO: Either Block has to use doStatements() or vice-versa for more centralized code.
										arr = doStatements(arr,v);
								}
							}
						default:
							trace("Case Block Error");
				}
				
			default:
				trace("Default Error");
		  }
		  return arr;
	}
	
	// break
	private function SBreak ( contextObj : Dynamic, e : Expr  ): Array<String> {
		  var arr = new Array<String>();
		  switch( e ) {
			case EBreak:
				arr.push("break");
			default:
				trace("Break Error");
		  }
		  return arr;
	}
	
	// case 21 : 
 	private function SCase ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ECase( e1, e2 ):
				arr.push("case");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e1));
				arr.push( getSpace () );
				arr.push(":");
				switch( e2 ) {
					case EBlock( v ):
						//statments in a 'case' are returned as if in a block with EBlock[. But EBlock inserts '{' automatically in the begining. 
						//When developer has '{' for a 'case', default functionality inserts '{{' which is not a good idea. So avoid sending first block.
						//Remove first block by default and send the next so if Block exists, it inserts { if not insert statements manually in a loop as below.
						if(v.length>0){
							switch( v[0] ) {
								case EBlock( cs ):
									arr.push( getSpace () );
									arr = arr.concat(parseExpressions(v[0]));
								default:
									// though block and default/case write statementes in the same way. Tabbing varies. 
									// TO DO: Either Block has to use doStatements() or vice-versa for more centralized code.
									arr = doStatements(arr,v);
							}
						}
					default:
						trace("Case Block Error");
				}
			default:
				trace("Case Error");
		}
		return arr;
	}
	
	// switch (age) { case 24:  }
	private function SSwitch ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ESwitch( e1, e2 ):
				arr.push("switch");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e1));
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e2));
			default:
				trace("Switch Error");
		}
		return arr;
	}
	
	// do { } while (a<b);
	private function SDoWhile ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EDoWhile( e1, cond ):
				arr.push("do");
				arr.push( getSpace () );
				arr = insertTabsForSingleStatementStructures(arr,e1);
				if(!isBlock( e1 )){
					arr.push( getSemicolon () );
					arr.push( getNewLine () );
					arr = arr.concat(tabArray); 
				}else{
					arr.push( getSpace () );
				}
				arr.push("while");
				arr.push( getSpace () );
				arr.push("(");
				arr = arr.concat(parseExpressions(cond));
				arr.push(")");
			default:
				trace("DoWhile Error");
		}
		return arr;
	}
	
	// delete a,b;
	private function SDelete ( contextObj : Dynamic, e : Expr  ): Array<String>  {
		var arr = new Array();
		switch( e ) {
			case EDelete(e1):
					arr.push("delete");
					arr.push( getSpace () );
					for(k in 0...e1.length){
						var exp = e1[k];
						arr = arr.concat(parseExpressions(exp));
						if(k>=0 && k<e1.length-1){
							arr.push( getComma () );
							arr.push( getSpace () );
						}
					}
			default:
				trace("Delete Error");
		}
		return arr;
	}
	
	// catch (e:Error) { }
	// TO DO: SCATCH is not returned properly from Parser.
	private function SCatch ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ECatch( err, e1  ):
				arr.push("catch");
				arr.push( getSpace () );
				arr.push("(");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(err));
				arr.push( getSpace () );
				arr.push(")");
				if(!isBlock(e1)){
					trace("TO DO: NO BLOCK FOUND IN CATCH");
				}else{
					arr = arr.concat(parseExpressions(e1));
				}
				// TO DO: if there are multiple stmnts with no curly braces it is still accepted but this method does not recieve stmnts as an array but a single exprsn.
				// Currently it just works with BLOCK of stmnts.
				
			default:
				trace("Catch Error");
		}
		return arr;
	}
	
	// try { }
	//TO DO: Try is accepted with no catch blocks which is wrong.
	private function STry ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ETry(ex, ctchArr):
				arr.push("try");
				arr.push( getSpace () );
				if(!isBlock(ex)){
					trace("TO DO: NO BLOCK FOUND IN TRY");
				}else{
					arr = arr.concat(parseExpressions(ex));
				}
				for(k in 0...ctchArr.length){
						var c = ctchArr[k];
						arr.push( getSpace () );
						arr = arr.concat(parseExpressions(c));
				}
				
			default:
				trace("try Error");
		}
		return arr;
	}
	
	// throw new Error ();
	private function SThrow ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EThrow(ex):
				arr.push("throw");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(ex));
			default:
				trace("continue Error");
		}
		return arr;
	}
	
	// private var 
	private function SModifier ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EModifier( e1, f ):
				arr.push(f);
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e1));
			default:
				trace("Modifier Error");
		}
		return arr;
	}
	
	// [1,2,3];
	private function SArrayDecl ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EArrayDecl( e1 ):
				arr.push("[");
				for(k in 0...e1.length){
					var elem = e1[k];
					arr = arr.concat(parseExpressions(elem));
					if(k>=0 && k<e1.length-1){
						arr.push( getComma () );
					}
				}
				arr.push("]");
			default:
				trace("ArrayDecl Error");
		}
		return arr;
	}
	
	// a[10]
	private function SArray ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EArray( ex, index):
				arr = arr.concat(parseExpressions(ex));	
				arr.push("[");
				arr = arr.concat(parseExpressions(index));	
				arr.push("]");
			default:
				trace("Array Error");
		}
		return arr;
	}
	
	// while (a<b) { }
	private function SWhile ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EWhile( cond, e1 ):
				arr.push("while");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(cond));
				arr.push( getSpace () );
				arr = insertTabsForSingleStatementStructures(arr,e1);
			default:
				trace("While Error");
		}
		return arr;
	}
	
	// return 10;
	private function SReturn ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EReturn( ex ):
				arr.push("return");
				if(ex!=null){
					arr.push( getSpace () );
					// A return can return an anonymous object. So just assign "anonymous" here. If it returns anonymous object, "Obj" gets appended by that function.
					// TO DO: It can be removed if we have context object living in this method. 
					assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"anonymous");
					//If block does not have any statements, it means it is an empty anonymous Object.
					// Check and assign "obj" if it is so.
					isEmptyBlockAnObj(ex);
				}
				arr = arr.concat(parseExpressions(ex));
				
			default:
				trace("Return Error");
		}
		return arr;
	}
	
	// function getSal (){ }
	// = function (){ };
	private function SFunction ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EFunction( params, ex, name):
				// If this exprn needs a semicolon and not already assigned value, assign "fun" for anonymous function.
				// TO DO: Instead of this, using ContextObj we may achieve inserting SEMICOLON for anonymous obj/function. 
				if(!isLatestBlockAnExceptionalCaseForSemicolon (getLatestBlock())){
					assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"fun");
				}
				arr.push("function");
				arr.push( getSpace () );
				if(contextObj!=null){
					if(contextObj.from=="getSet"){
						arr.push(contextObj.getSet);
						arr.push( getSpace () );
					}
				}
				//Inline/Anonymous functions will not have names. So check before pushing function name
				if(name!=null){
					arr.push(name);
					arr.push( getSpace () );
				}
				arr.push("(");
				for(k in 0...params.length){
					arr.push( getSpace () );
					var p = params[k];
					arr = arr.concat(parseExpressions(p));
					if(k>=0 && k<params.length-1){
						arr.push( getComma () );
					}
				}
				if(params.length > 0){
					arr.push( getSpace () );
				}
				arr.push(")");
				if(contextObj!=null){
					if(contextObj.type!=null){
						arr.push( getSpace () );
						arr.push(":");
						arr = tokenizeDotStrings(contextObj.type,arr);
					}
					//for interfaces, ex will be null, i.e, methods would not contain any block. so do not insert a space coz before semicolon there must be no space.
					if(ex != null ){
						arr.push( getSpace () );
					}
				}
				//for interfaces, ex will be null, i.e, methods would not contain any block. so insert a semicolon after return type.
				if(ex == null ){
					arr.push( getSemicolon () );
				}
				arr = arr.concat(parseExpressions(ex));
				//we need a new line after functions but anonymous function
				if( name != null ) {
					arr.push( getNewLine () );
				}
			default:
				trace("Function Error");
		}
		return arr;
	}
	
	// {a:10,b:"Str"}
	private function SAObj ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EAObj( e1 ):
				arr.push("{");
				// If this exprn needs a semicolon and not already assigned value, assign "obj" for anonymous object.
				// TO DO: Instead of this, using ContextObj we may achieve inserting SEMICOLON for anonymous obj/function. 
				if(!isLatestBlockAnExceptionalCaseForSemicolon (getLatestBlock())){
						assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"obj");
				}
				for(k in 0...e1.length){
					var p = e1[k];
					var c:Dynamic = {};
					c.from = "object";
					arr = arr.concat(parseExpressions(p,c));
					if(k>=0 && k<e1.length-1){
						arr.push( getComma () );
						arr.push( getSpace () );
					}
				}
				arr.push("}");
			default:
				trace("AObj Error");
		}
		return arr;
	}
	
	// var a : Number = 10;
	private function SVar ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EVar( n , ex ):
				if(contextObj!=null){
					//If called from varCollection, do not insert 'var' coz it has to be put only once and is already done by SVarCollection.
					if(contextObj.from != "varCollection"){
						arr.push("var");
						// var can declare an anonymous object/function. So just assign "anonymous" here. If it declares anonymous object/function, those values get appended by respetive functions like EAOBJ.
						assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"anonymous");
						arr.push( getSpace () );
					}
				}
				arr.push(n);
				if(contextObj!=null){
					if(contextObj.type!=null){
						arr.push( getSpace () );
						arr.push(":");
						arr = tokenizeDotStrings(Std.string(contextObj.type),arr);
					}
				}
				if(ex!=null){
					arr.push( getSpace () );
					arr.push("=");
					arr.push( getSpace () );
					arr = arr.concat(parseExpressions(ex,contextObj));
					// In case of empty anonymous objects, Parser returns [EBLOCK] not [EAOBJ]. So check if it is so.
					isEmptyBlockAnObj(ex);
				}
			default:
				trace("Var Error");
		}

		return arr;
	}
	
	// var a : Number, b : Object = {a:10};
	private function SVarCollection ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EVarCollection(v):
				arr.push("var");
				arr.push( getSpace () );
				// var can declare a collection of anonymous object/functions. So just assign "anonymous" here. If it declares anonymous object/functions, those values get appended by respetive functions like EAOBJ etc.
				assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"anonymous");
				for(k in 0...v.length){
					var exp = v[k];
					var cntxt:Dynamic = {};
					cntxt.from = "varCollection";
					arr = arr.concat(parseExpressions(exp,cntxt));
					if(k>=0 && k<v.length-1){
						arr.push( getComma () );
						arr.push( getSpace () );
					}
				}
			default:
				trace("VarCollection Error");
		}
		return arr;
	}
	
	// { somestmnts; somestmnts; };
	private function SBlock ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EBlock( v ):
				arr.push("{");
				arr.push( getNewLine () );
				tabArray.push( getTab () );
				if(v.length!=0){
					arr = arr.concat(tabArray);
				}
				for(k in 0...v.length){
					var exp = v[k];
					// Store the latest block. This block is going to be like a property for semiColon object.
					// Coz all the code is recursive, we will loose the exprn where we have come from.
					// So store it as a proerty for semiColon object and assign values to it wherever required.
					// Access it whenever needed and see if the value is anything that needs a SEMICOLON. 
					setLatestBlock(exp);
					// As soon as a pro is created, assign a null value.
					assignLatestBlockValue (getLatestBlock(), "");
					arr = arr.concat(parseExpressions(exp));
					/*
						Insert semicolon after every statement within a block.
						A block within another block is also treated as a statement and this inserts a semicolon.
						So avoid it if a stmnt ends with } or :
						This even avoids inserting ; after anonymous Objects/Functions which is wrong. So handle it in ELSE part.
						In ELSE, Check to see if this stmnt is an exceptional case for semicolon. If so, insert a SEMICOLON.
					*/
					if(arr[arr.length-1]!=getNewLine() && arr[arr.length-1]!="}"  && arr[arr.length-1]!=":" && arr[arr.length-1]!=getSemicolon()){
						arr.push( getSemicolon () );
					}else{
						 insertSemiColonInExceptionalCases (exp, arr);
					}
					arr.push( getNewLine () );
					if(k==v.length-1){
						tabArray.pop();
					}
					arr = arr.concat(tabArray);
				}
				if(v.length==0){
					tabArray.pop();
					arr = arr.concat(tabArray);
				}
				arr.push("}");
				// IF part -  
				// ELSE part - Check to see if this stmnt is an exceptional case for semicolon. If so, insert a SEMICOLON.
				if(v.length <= 0){
					// As an empty anonymous object is returned as an EBLOCK, insert a SEMICOLON after it ends.
					// To make sure we insert semi	
					if(arr[arr.length-1]!="}"  && arr[arr.length-1]!=":" && arr[arr.length-1]!=getSemicolon()){
						arr.push( getSemicolon () );
					}else{ 
						insertSemiColonInExceptionalCases (e, arr);
					}
				}
				
			default:
				trace("Block Error");
		}
		return arr;
	}
	
	// a = b + 5;	
	private function SBinop ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EBinop( op , e1, e2 ):
				arr = arr.concat(parseExpressions(e1));
				arr.push( getSpace () );
				//If Binop when called from Object context, recieves = in place of :. AS2.0 syntax needs to re-place = with :
				if(contextObj != null && contextObj.from=="object")
					arr.push(":");
				else{
					arr.push(op);
					if(op=="="){
						// Binop can have an anonymous object/function. So just assign "anonymous" here. If it declares anonymous object/functions, those values get appended by respetive functions like EAOBJ etc.
						assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"anonymous");
					}
				}
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(e2));
				//If block does not have any statements, it means it is an empty anonymous Object.
				isEmptyBlockAnObj(e2);
			default:
				trace("Binop Error");
		}
		return arr;
	}
	
	// a:Number
	private function STyped ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ETyped( ex, v ):
					if(v != null){
						var cntxt:Dynamic = {};
						cntxt.type = v;
						if(contextObj!=null){
							if(contextObj.from == "varCollection"){
								cntxt.from = "varCollection";
							}
						}
						arr = arr.concat(parseExpressions(ex,cntxt));
						switch( ex ) {
							case EIdent( v1 ):
								arr.push( getSpace () );
								arr.push(":");
								arr = tokenizeDotStrings(Std.string(v),arr);
							default :
								// trace("Typed Indent Error");
						}
					}else{
						// If type is not present this block gets executed.
						// If this bock is being called by VarCollection, we should not insert var (done in SVar) in the variable sequence so send a flag saying it is from Varcollection.
						// If this bock is being called by Var, we should insert var which will be done by SVar when 'from' is not varCollection.
						var cntxt:Dynamic = {};
						if(contextObj!=null){
							if(contextObj.from == "varCollection"){
								cntxt.from = "varCollection";
							}
						}
						arr = arr.concat(parseExpressions(ex,cntxt));
					}
			default:
				trace("Typed Error");
		}
		return arr;
	}
	
	// ++c;
	private function SUnop ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EUnop( op, prefix, eb):
				if(!prefix){
					arr = arr.concat(parseExpressions(eb));
					arr.push(Std.string(op));
				}else{
					arr.push(Std.string(op));
					arr = arr.concat(parseExpressions(eb));
				}
			default:
				trace("Unop Error");
		}
		return arr;
	}
	
	//for (var i=0; i<array.length; i++) { } 
	private function SFor ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EFor( e1, e2, e3, eb ):
				arr.push("for");
				arr.push( getSpace () );
				arr.push("(");
				for(k in 0...e1.length){
					var p = e1[k];
					arr = arr.concat(parseExpressions(p));
					if(k>=0 && k<e1.length-1){
						arr.push( getComma () );
					}
				}
				arr.push( getSemicolon () );
				arr.push( getSpace () );
				for(k in 0...e2.length){
					var p = e2[k];
					arr = arr.concat(parseExpressions(p));
					if(k>=0 && k<e2.length-1){
						arr.push( getComma () );
					}
				}
				arr.push( getSemicolon () );
				arr.push( getSpace () );
				for(k in 0...e3.length){
					var p = e3[k];
					arr = arr.concat(parseExpressions(p));
					if(k>=0 && k<e3.length-1){
						arr.push( getComma () );
					}
				}
				arr.push( getSpace () );
				arr.push(")");
				arr.push( getSpace () );
				arr = insertTabsForSingleStatementStructures(arr,eb);
			default:
				trace("For Error");
		}
		return arr;
	}
	
	// employee.doWork
	private function SField ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EField( e1, f ):
				arr = arr.concat(parseExpressions(e1));
				arr.push(".");
				arr = tokenizeDotStrings(f,arr);
			default:
				trace("Ident Error");
		}
		return arr;
	}
	
	// (age:Number, name:String)
	private function SCall ( contextObj : Dynamic, e : Expr  ) :Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case ECall( v, params ):
				arr = arr.concat(parseExpressions(v));
				arr.push("(");
				for(k in 0...params.length){
					arr.push( getSpace () );
					var p = params[k];
					arr = arr.concat(parseExpressions(p));
					if(k>=0 && k<params.length-1){
						arr.push( getComma () );
					}
				}
				if(params.length>0)
					arr.push( getSpace () );
				arr.push(")");
			default:
				trace("Call Error");
		}
		return arr;
	}
	
	// import company.employees.PL;
	private function SImport( contextObj : Dynamic, e : Expr ) :Array<String> {
		var arr = new Array<String>();
		arr.push("import");
		arr.push( getSpace () );
		switch( e ) {
			case EImport( v ) :
				arr = tokenizeDotStrings(v,arr);
			default :
				trace("Import Error");
		}
		arr.push( getSemicolon () );
		return arr;
	}
	
	// obj : a.b.C (Second part only)
	private function SIdent ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EIdent( v ):
				arr = tokenizeDotStrings(v,arr);
			default:
				trace("Ident Error");
		}
		return arr;
	}

	// ( ) for ifs and so.
	private function SParent ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EParent( ex ):
				arr.push("(");
				arr.push( getSpace () );
				arr = arr.concat(parseExpressions(ex));
				arr.push( getSpace () );
				arr.push(")");
			default:
				trace("Parent Error");
		}
		return arr;
	}
	
	// if (age > 21) trace("eligible for voting");
	private function SIf ( contextObj : Dynamic, e : Expr  ): Array<String> {
		var arr = new Array<String>();
		switch( e ) {
			case EIf( cond , e1 , e2, t ): 
					if(!t){
						arr.push("if");
						arr.push( getSpace () );
						arr = arr.concat(parseExpressions(cond));
						arr.push( getSpace () );
						if(!isBlock( e1 )){
							arr.push( getNewLine () );
							tabArray.push( getTab () );
							arr = arr.concat(tabArray);
						}
						arr = arr.concat(parseExpressions(e1));
						if(!isBlock( e1 )){
							tabArray.pop();
						}
						if(e2!=null){
							if(!isBlock( e1 )){
								//If there exists only one statement within if, insert ; after that coz it never reaches EBLOCK which does ; insertion.
								arr.push( getSemicolon () );
								arr.push( getNewLine () );
								arr = arr.concat(tabArray);
							}else{
								arr.push( getSpace () );
							}
							arr.push("else");
							switch( e2 ) {
								case EBlock( v ):
									arr.push( getSpace () );
								case EIf( cond1 , e11 , e21, t1 ):
									arr.push( getSpace () );
								default:
									arr.push( getNewLine () );
									tabArray.push( getTab () );
									arr = arr.concat(tabArray);
							}
							arr = arr.concat(parseExpressions(e2));
							switch( e2 ) {
								case EBlock( v ):								
								case EIf( cond1 , e11 , e21, t1 ):								
								default:								
									tabArray.pop();
							}				
						}
					}else{
						arr = arr.concat(parseExpressions(cond));
						arr.push( getSpace () );
						arr.push("?");
						arr.push( getSpace () );
						arr = arr.concat(parseExpressions(e1));
						arr.push( getSpace () );
						arr.push(":");
						arr.push( getSpace () );
						arr = arr.concat(parseExpressions(e2));
					}

			default:
				trace("If Error");
		}
		return arr;
	}
	
	/********************************************************************************************************************
	* 
	* Below are util/housekeeping functions.
	* Changing them needs good understanding of where they are refered and how they are used.
	* 
	********************************************************************************************************************/
	
	// Store the latest block. This block is going to be like a property for semiColon object.
	// Coz all the code is recursive, we will loose the exprn where we have come from.
	// So store it as a proerty for semiColon object and assign values to it wherever required.
	// Access it whenever needed and see if the value is anything that needs a SEMICOLON. 
					
	private function getLatestBlock(){
		return currentExprn;
	}
	
	// Returns the latest block.
	private function setLatestBlock(e : Expr){
		currentExprn = e;
	}
	
	// Though a string is directly passed i.e, company.employees.HR token counting should include individual elements. 
	// So split them based on . operator and then push them individually so each is counted.
	private function tokenizeDotStrings(v:String,arr:Array<String>){
		var _v = v.toString().split(".");
		for(i in 0..._v.length){
			arr.push(_v[i]);
			if(i>=0 && i<_v.length-1){
				arr.push(".");
			}
		}
		return arr;
		
	}
	
	// Verifies if an exprn is a BLOCK
	private function isBlock(e : Expr):Bool{
		switch( e ) {
			case EBlock( v ):
				return true;
			default:
				return false;
		}
	}
	
	//Used by For, For in
	//If statements within 'for' is a 'block', '{}' automatically get inserted and tabbing will be taken care.
	//When not a 'block', do it manually
	private function insertTabsForSingleStatementStructures(arr:Array<String>,ex:Expr){
		if(!isBlock( ex )){
			arr.push( getNewLine () );
			tabArray.push( getTab () );
			arr = arr.concat(tabArray);
		}
		arr = arr.concat(parseExpressions(ex));
		if(!isBlock( ex )){
			tabArray.pop();
		}
		return arr;
	}
	private function isEmptyBlockAnObj(e:Expr){
		// If block does not have any statements, it means it is an empty anonymous Object.
		// So assign "obj" here.
			if(isBlock(e)){
				switch( e ) {
					case EBlock( v ):
						if(v.length<=0){
							if(!isLatestBlockAnExceptionalCaseForSemicolon (getLatestBlock())){
								assignLatestBlockValue (getLatestBlock(), semiColon.get(Std.string(getLatestBlock()))+"obj");
							}
						}
					default:
				}
			}
	}
	
	private function doStatements(arr:Array<String>,v:Array<Expr>){
		//For CASE/DEFAULT statements, SBLOCK would not get executed though those stmnts are similar but witout braces.
		//Do the same as of SBLOCK but without braces.
		arr.push( getNewLine () );
		tabArray.push( getTab () );
		arr = arr.concat(tabArray); 
		for(k in 0...v.length){
			var exp = v[k];
			// Store the latest block. This block is going to be like a property for semiColon object.
			// Coz all the code is recursive, we will loose the exprn where we have come from.
			// So store it as a proerty for semiColon object and assign values to it wherever required.
			// Access it whenever needed and see if the value is anything that needs a SEMICOLON. 
			setLatestBlock(exp);
			// As soon as a pro is created, assign a null value.
			assignLatestBlockValue (getLatestBlock(), "");
			//semiColon.set(Std.string(getLatestBlock()),"");
			arr = arr.concat(parseExpressions(exp));
			//Do not insert only for the last statement ; coz for a stmnt SBLOCK puts a ;
			if(arr[arr.length-1]!="}"  && arr[arr.length-1]!=":"){
				if(k != v.length-1){
					arr.push( getSemicolon () );
				}
			}else {
				// Check to see if this stmnt is an exceptional case for semicolon. If so, insert a SEMICOLON.
				insertSemiColonInExceptionalCases (exp, arr); 
			}
			if(k>=0 && k<v.length-1){
				arr.push( getNewLine () );
				arr = arr.concat(tabArray);
			}
			if(k == v.length-1){
				tabArray.pop();
			}
		}
		return arr;
	}
		
	// Function used by anonymous Object/functions only.
	// It creates the sent exprn as a propert for semiColon object & sets a value. Semicolon Obj is modified only here.
	// For e.f, anonymous, obj etc.
	// TO DO: Method may be removed if we use contextObj.
	private function assignLatestBlockValue (e:Expr, val:String) {
		var __latestBlck:String = Std.string(e);
		semiColon.set(__latestBlck, val);
	}
	
	// Call this method if want to put a SEMICOLON in exceptional cases like Anonymous obj/fucntions.
	// This function automatically clears the semiColon's respective property as soon as it puts a SEMICOLON
	// TO DO: Method may be removed if we use contextObj.
	private function insertSemiColonInExceptionalCases (whichStmnt:Expr, arr:Array<String>) {
		if(isLatestBlockAnExceptionalCaseForSemicolon (whichStmnt) ){
			arr.push( getSemicolon () );
			assignLatestBlockValue (whichStmnt, "");
		}
	}
	
	// Function that returns true if a passed exprn needs to have semiColon.
	// For e.g, if you pass an exprn of Anonymous Obj, coz we have set that property in semiColon object to a meaningful value (e.g, "anonymousobject"), 
	// it checks that value and then says this stmnt needs a SEMICOLON.
	// TO DO: Method may be removed if we use contextObj.
	private function isLatestBlockAnExceptionalCaseForSemicolon (whichStmnt:Expr) :Bool {
		if(semiColon.get(Std.string(whichStmnt)) != "anonymousfun" && semiColon.get(Std.string(whichStmnt)) != "anonymousobj"){
			return false;
		}else{
			return true;
		}
	}
	
	// SortS the arrays so that they have keys arrangement as below
	// [34, 37, 37_1, 37_2,46, 49,49_1]
	private function sortKeysFunction ( x :String, y :String ) {
		var a1 = Std.parseInt (x.split("_")[0]);
		var b1 = Std.parseInt (y.split("_")[0]);
		var a2 = Std.parseInt (x.split("_")[1]);
		var b2 = Std.parseInt (y.split("_")[1]);
		if(a1 > b1 ){
				return 1;
		}else if( a1 < b1){
				return -1;
		}		
		if(x.indexOf("_") !=-1 && y.indexOf("_") !=-1 ){
			if(a2 > b2 ){
				return 1;
			}else if(a2 < b2 ){
				return -1;
			}else {
				return 0;
			}
		}
		if(x.indexOf("_") !=-1 && y.indexOf("_") ==-1 ){
			return 1;
		}
		if(x.indexOf("_") ==-1 && y.indexOf("_") !=-1 ){
			return -1;
		}
		if(x.indexOf("_") ==-1 && y.indexOf("_") ==-1 ){
			return 0;
		}
		return 0;
	}
	
	private function getNewLine ( ) : String {
		return "\n";
	}
	
	private function getTab ( ) : String {
		return "\t";
	}
	
	private function getSpace ( ) : String {
		return " ";
	}
	
	private function getSemicolon ( ) : String {
		return ";";
	}
	
	private function getComma ( ) : String {
		return ",";
	}
}
//************************************************************************************************************************************
