package lyc.compiler;

import java_cup.runtime.Symbol;
import lyc.compiler.ParserSym;
import lyc.compiler.model.*;
import static lyc.compiler.constants.Constants.*;

%%

%public
%class Lexer
%unicode
%cup
%line
%column
%throws CompilerException
%eofval{
  return symbol(ParserSym.EOF);
%eofval}


%{
  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }

  public void writeSimbolTable(){
      try {
                   String ruta = "./SimbolTable.txt";
                   File file = new File(ruta);

                   // Validate if exists
                   if (!file.exists()) {
                       file.createNewFile();
                   }

                   FileWriter fw = new FileWriter(file);
                   BufferedWriter bw = new BufferedWriter(fw);
                   bw.write("Name "+ "\t  "+ "Type "+ "\t  "+ "Value " + " \t   "+ "Length "+ "\n");
                   bw.newLine();
                   for(int i=0;i<elemento.size();i++)
                     {
                        //bw.write(elemento.get(i).getNombre() + "\t " + elemento.get(i).getTipo() + "\t " + elemento.get(i).getValor() + "\t " + elemento.get(i).getLongitud() +"\n");
                        bw.newLine();
                     }

                   bw.close();
            }
               catch (Exception e) {
                   e.printStackTrace();
               }
           }
%}


LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
Identation =  [ \t\f]

/* aritmethic operators */
Plus = "+"
Mult = "*"
Sub = "-"
Div = "/"
Assign = "="

/* logic operators */
Equal = "=="
NotEqual = "!="
LessThan = "<"
LessEqual = "<="
GreaterThan = ">"
GreaterEqual = ">="
Not = "!"
And = "&&"
Or  = "||"

/* flow control */
If  =  "if"
Else = "else"
While = "while"

/* declaracion */
Var = "var"
As  = "as2"
Coma = ","

/* tipos de dato */
Int    =  "int"
Real    = "real"
String_T = "string"

/* fin de instruccion */
EndStmt = ";"
NewLine = "\n"

OpenBracket = "("
CloseBracket = ")"
OpenKey = "{"
CloseKey = "}"
OpenCorchete = "["
CloseCorchete = "]"

Letter = [a-zA-Z]
Digit = [0-9]

WhiteSpace = {LineTerminator} | {Identation}
Identifier = {Letter} ({Letter}|{Digit})*
IntegerConstant = {Digit}+
RealConstant = ({Digit}+ "." {Digit}*)|({Digit}* "." {Digit}+)
Comment = "{Div}{Mult} (Letter | Digit | WhiteSpace)* {Mult}{Div}"

%%


/* keywords */

<YYINITIAL> {

  /* identifiers */
  {Identifier}                             { return symbol(ParserSym.IDENTIFIER, yytext()); }

  /* Constants */
  {IntegerConstant}                        { return symbol(ParserSym.INTEGER_CONSTANT, yytext()); }
  {RealConstant}                            { return symbol(ParserSym.REAL_CONSTANT, yytext()); }

  /* operators */
  {Plus}                                    { return symbol(ParserSym.PLUS); }
  {Sub}                                     { return symbol(ParserSym.SUB); }
  {Mult}                                    { return symbol(ParserSym.MULT); }
  {Div}                                     { return symbol(ParserSym.DIV); }

/* tipos de dato */
  {Int}                                    { return symbol(ParserSym.INT); }
  {Real}                                   { return symbol(ParserSym.REAL); }
  {String_T}                               { return symbol(ParserSym.STRING); }

  /* flow control */
  {If}                                    { return symbol(ParserSym.IF); }
  {Else}                                    { return symbol(ParserSym.ELSE); }
  {While}                                    { return symbol(ParserSym.WHILE); }

  /* declaracion */
  {Var}                                    { return symbol(ParserSym.VAR); }
  {As}                                    { return symbol(ParserSym.AS); }
  {Coma}                                    { return symbol(ParserSym.COMA); }

  {Assign}                                   { return symbol(ParserSym.ASSIG); }
  {OpenBracket}                             { return symbol(ParserSym.OPEN_BRACKET); }
  {CloseBracket}                            { return symbol(ParserSym.CLOSE_BRACKET); }
  {OpenKey}                                 { return symbol(ParserSym.OPEN_KEY); }
  {CloseKey}                                { return symbol(ParserSym.CLOSE_KEY); }
  {OpenCorchete}                            { return symbol(ParserSym.OPEN_CORCHETE); }
  {CloseCorchete}                           { return symbol(ParserSym.CLOSE_CORCHETE); }

  /* fin de instruccion */
  {EndStmt}                            { return symbol(ParserSym.END_STMT); }
  {NewLine}                            { return symbol(ParserSym.NEW_LINE); }

  /* logic operators */
  {Equal}                            { return symbol(ParserSym.EQUAL); }
  {NotEqual}                            { return symbol(ParserSym.NOT_EQUAL); }
  {LessThan}                            { return symbol(ParserSym.LESS_THAN); }
  {LessEqual}                            { return symbol(ParserSym.LESS_EQUAL); }
  {GreaterThan}                            { return symbol(ParserSym.GREATER_THAN); }
  {GreaterEqual}                            { return symbol(ParserSym.GREATER_EQUAL); }
  {Not}                            { return symbol(ParserSym.NOT); }
  {And}                            { return symbol(ParserSym.AND); }
  {Or}                            { return symbol(ParserSym.OR); }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}


/* error fallback */
[^]                              { throw new UnknownCharacterException(yytext()); }
