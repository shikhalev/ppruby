(*
	unit ppRuby18
	license : GNU GPL
*)

{$h+}
{$mode objfpc}
{$packenum 4}
{$packrecords C}
{$writeableconst off}

unit ppRuby18;

interface

uses
  Variants,
  Ruby18;

function Value2Variant (value : VALUE) : variant;
function Variant2Value (value : variant) : VALUE;

implementation

function Value2Variant (value : VALUE) : variant;
 begin
 end;

function Variant2Value (value : variant) : VALUE;
 begin
 end;

end.