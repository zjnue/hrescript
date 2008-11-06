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
package;

import hrescript.input.hx.Parser;
import hrescript.input.hx.Expr;
import hrescript.output.hx.Writer;
import neko.FileSystem;

class WriteHx { 
	
	private var mode:Bool;
	private static var excl:Hash<String>;
	private var filesNotFormatted:Hash<String>;
	
	static function main() {
		try {
			new WriteHx();
		} catch( e:Dynamic ) {
			trace("error : " + Std.string(e));
		}
	}
	
	public function new() {
		//Change mode in below stmnt;
		//True - for Folder process
		//False - for single file (code_as2.txt)
		mode = false;
		//--------------------------------
		filesNotFormatted = new Hash<String>();
		excl = new Hash<String>();
		excl.set(".svn",".svn");
		excl.set(".as",".as");
		if(getMode()){
			parseDir("C:/path/to/hx/lib/you/want/to/test");
			trace("Files not formatted are : "+filesNotFormatted);
		}else{
			loadFile();
		}
	}
	
	private function getMode():Bool{
		return mode;
	}
	
	function loadFile ( ?filePath :String = "code_hx.txt" ) {
		trace("filePath = "+filePath);
		var content = neko.io.File.getContent(filePath);
		var formattedCode:String;
		try {
			formattedCode = parseScript(content);
		}catch( e:Dynamic ) {
			trace("error : " + Std.string(e));
			filesNotFormatted.set(filePath,Std.string(e));
			return;
		}
		if(getMode()){
			writeToFile(filePath,formattedCode);
		}else{
			trace("\n"+formattedCode);
		}
	}
	
	function writeToFile(path,data){
		var f = neko.io.File.write(path, false);
		f.writeString(data);
		f.close();
	} 
	
	function parseScript( s : String ) {
		var parser = new Parser();
		var si = new haxe.io.StringInput( s );
		var program = parser.parse( si );
		//trace("program = "+program);
		var writer = new Writer( );
		return writer.write( program, parser.getComments() );
	}
	
	function parseDir( path : String ) {
		var items :Array<String> = neko.FileSystem.readDirectory(path);
		var numFiles :Int = 0;
		for( item in items ) {
			
			var itemPath :String = path + "/" + item;
			var isDirectory = FileSystem.isDirectory(itemPath);
			if(excl.exists(item))
				continue;
			if(isDirectory){
				parseDir(itemPath);
			}else{
				if(item.split(".")[1] == "hx"){
					loadFile(itemPath);
				}
			}
		}
		
	}
}