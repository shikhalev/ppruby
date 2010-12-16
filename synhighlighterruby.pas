{ Syntax highlighter for SynEdit. Highlighting the Ruby programming language

  Based on http://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/syntax.html

  Copyright (C) 2010 Ido Kanner idokan at@at gmail dot.dot com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit SynHighlighterRuby;

{$MODE OBJFPC}{$H+}

interface

uses
    {$IFDEF SYN_LAZARUS}
    LCLIntf, LCLType,
    {$ENDIF}
    Graphics, SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes,
    SysUtils, Classes;

type
  // What is the type of thingy we have encounterd ?
  TtkTokenKind = (
// # a comment
                  tkComment,
(*
=BEGIN
 Some Text that document the code
=END
*)
                  tkDocument,
(*
"String"
%Q{\nThis is a double-quoted string\n}
%{\nThis is a double-quoted string\n}
%/\nThis is a double-quoted string\n/
*)
                  tkDoubleQuoteString,
(*
'String'
%q{This is a single-quoted string}
*)
                  tkSingleQuoteString,
(*
print <<EOF
The price is #{$Price}.
EOF

print <<"EOF";			# same as above
The price is #{$Price}.
EOF

print <<`EOC`			# execute commands
echo hi there
echo lo there
EOC

print <<"foo", <<"bar"	# you can stack them
I said foo.
foo
I said bar.
bar

myfunc(<<"THIS", 23, <<'THAT')
Here's a line
or two.
THIS
and here's another.
THAT

eval <<-EOS			# delimiters can be indented
def foo
 print "foo\n"
end
EOS
*)
                  tkHereDocString,
// "\n"
                  tkStringExpression,
// "#{var}"
                  tkStringExpressionSubsitution,
// def, class BEGIN, END, nil do etc...
                  tkReservedWord,
// hi(), 1+1, true, false, (5*3)-1 etc...
                  tkExpression,
// `date` , %x{ date } ,  %x/date/
                  tkCommandOutput,
(*
/^Ruby the OOPL/
/Ruby/i
/my name is #{myname}/o
%r|^/usr/local/.*|
*)
                  tkRegex,
(*
123 integer
-123 integer(signed)
1_234 integer(underscore within decimal numbers ignored)
123.45 floating point number
1.2e-3  floating point number
0xffff hexadecimal integer
0b01011 binary integer
0377 octal integer
?a ASCII code for character `a'(97)
?\C-a Control-a(1)
?\M-a Meta-a(225)
?\M-\C-a Meta-Control-a(129)
:symbol Integer corresponding identifiers, variable names, and operators.
*)
                  tkNumerickLiteral,
// var = something
                  tkLocalVar,
// $var
                  tkGlobalVar,
// @instance_var
                  tkInstanceVar,
// @@class_var
                  tkClassVar,
(*
CONSTANT = something
class ClassName
module ModuleName
*)
                  tkConstant,
// ::
                  tkNameSpace,
// Class.method
                  tkMethod,
// :symbol
                  tkSymbol,
                  tkUnknown
                   );

  TRubyCodeFoldBlockType = (
    cfbtBeginEnd,          // BEGIN END
    cfbtMultilineComments, // =BEGIN =END
    cfbtHereDocString,     // <<HEREDOC ... HEREDOC
    cfbtDefEnd,            // def something ... end
    cfbtClassEnd,          // class Something ... end
    cfbtDoEnd,             // Iteration block
    cfbtIfUnlessEnd,       // if/unless .... end
    cfbtWhileUntilEnd,     // while/until .. end
    cfbtModuleEnd,         // module ... end
    cfbtCaseEnd,           // case ... end
    cfbtException,         // begin ... rescue ... ensure .. else .. end
    cfbtNone               // Internal type / not configurable
  );

  TRubyCodeFoldBlockTypes = set of TRubyCodeFoldBlockType;

const
  CountRubyCodeFoldBlockOffset : Pointer =
     Pointer(PtrInt(Integer(high(TRubyCodeFoldBlockType))+1));

  cfbtAll: TRubyCodeFoldBlockTypes =
    [low(TRubyCodeFoldBlockType)..high(TRubyCodeFoldBlockType)];

  KEYWORDS_MAX  = 38;
  KEYWORDS_LIST : array[1..KEYWORDS_MAX] of string =
     (
       'BEGIN', 'class',  'ensure', 'nil',   'self',   'when',   'END',
       'def',   'false',  'not',    'super', 'while',  'alias',  'defined',
       'for',   'or',     'then',   'yield', 'and',    'do',     'if',
       'redo',  'true',   'begin',  'else',  'in',     'rescue', 'undef',
       'break', 'elsif',  'module', 'retry', 'unless', 'case',   'end',
       'next',  'return', 'until'
     );



type
  { TSynRubySyn }

  TSynRubySyn = class(TSynCustomFoldHighlighter)
  private
  protected
    function GetKeywordIdentifiers: TStringList; virtual;
    function GetEol : boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;                     override;
  published
  end;

implementation

{ TSynRubySyn }

function TSynRubySyn.GetKeywordIdentifiers : TStringList;
var i : integer;
begin
  Result := TStringList.Create;

  for i := Low(KEYWORDS_LIST) to High(KEYWORDS_LIST) do
  begin
    Result.AddObject(KEYWORDS_LIST[i],
     TObject(ord(tkReservedWord)));
  end;
end;

function TSynRubySyn.GetEol : boolean;
begin
  Result := fTokenID = tkNull;
end;

constructor TSynRubySyn.Create ( AOwner : TComponent ) ;
begin
  inherited Create ( AOwner ) ;
end;

destructor TSynRubySyn.Destroy;
begin
  inherited Destroy;
end;

end.

