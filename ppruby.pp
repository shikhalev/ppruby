(*
   Package : RubyFCL
   File : ppruby.pp
   Desc : core unit of ruby binding
   License : GNU GPL
*)

unit ppRuby;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynLibs;

type
  PVALUE = ^VALUE;
  VALUE = packed record
    data : PtrUInt;
  end;

  PID = ^ID;
  ID = packed record
    data : PtrUInt;
  end;

type
  TRubyVersion = (
    rvNone,
    rvRuby18, rvRuby19
    (* , future versions ... *)
  );

  TRubyMethod0   = function (slf : VALUE) : VALUE; cdecl;
  TRubyMethod1   = function (slf : VALUE; a : VALUE) : VALUE; cdecl;
  TRubyMethod2   = function (slf : VALUE; a, b : VALUE) : VALUE; cdecl;
  TRubyMethod3   = function (slf : VALUE; a, b, c : VALUE) : VALUE; cdecl;
  TRubyMethod4   = function (slf : VALUE; a, b, c, d : VALUE) : VALUE; cdecl;
  TRubyMethod5   = function (slf : VALUE; a, b, c, d, e : VALUE) : VALUE; cdecl;
  TRubyMethod6   = function (slf : VALUE; a, b, c, d, e, f : VALUE) : VALUE; cdecl;
  TRubyMethod7   = function (slf : VALUE; a, b, c, d, e, f, g : VALUE) : VALUE; cdecl;
  TRubyMethod8   = function (slf : VALUE; a, b, c, d, e, f, g, h : VALUE) : VALUE; cdecl;
  TRubyMethod9   = function (slf : VALUE; a, b, c, d, e, f, g, h, i : VALUE) : VALUE; cdecl;
  TRubyMethod10  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j : VALUE) : VALUE; cdecl;
  TRubyMethod11  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k : VALUE) : VALUE; cdecl;
  TRubyMethod12  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l : VALUE) : VALUE; cdecl;
  TRubyMethod13  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m : VALUE) : VALUE; cdecl;
  TRubyMethod14  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n : VALUE) : VALUE; cdecl;
  TRubyMethod15  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o : VALUE) : VALUE; cdecl;
  TRubyMethod16  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p : VALUE) : VALUE; cdecl;
  TRubyMethod17  = function (slf : VALUE; a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q : VALUE) : VALUE; cdecl;
  TRubyMethodArr = function (argc : LongInt; argv : PVALUE; slf : VALUE) : VALUE; cdecl;

procedure DefineMethod (cls : VALUE; method : TRubyMethod0);
procedure DefineMethod (cls : VALUE; method : TRubyMethod1);
procedure DefineMethod (cls : VALUE; method : TRubyMethod2);
procedure DefineMethod (cls : VALUE; method : TRubyMethod3);
procedure DefineMethod (cls : VALUE; method : TRubyMethod4);
procedure DefineMethod (cls : VALUE; method : TRubyMethod5);
procedure DefineMethod (cls : VALUE; method : TRubyMethod6);
procedure DefineMethod (cls : VALUE; method : TRubyMethod7);
procedure DefineMethod (cls : VALUE; method : TRubyMethod8);
procedure DefineMethod (cls : VALUE; method : TRubyMethod9);
procedure DefineMethod (cls : VALUE; method : TRubyMethod10);
procedure DefineMethod (cls : VALUE; method : TRubyMethod11);
procedure DefineMethod (cls : VALUE; method : TRubyMethod12);
procedure DefineMethod (cls : VALUE; method : TRubyMethod13);
procedure DefineMethod (cls : VALUE; method : TRubyMethod14);
procedure DefineMethod (cls : VALUE; method : TRubyMethod15);
procedure DefineMethod (cls : VALUE; method : TRubyMethod16);
procedure DefineMethod (cls : VALUE; method : TRubyMethod17);
procedure DefineMethod (cls : VALUE; method : TRubyMethodArr);

function Version : TRubyVersion; inline;

function Load (ver : TRubyVersion) : Boolean;
function Load : TRubyVersion;
procedure Unload;

function getActive : Boolean;
procedure setActive (val : Boolean);
property Active : Boolean read getActive write setActive;

implementation

var
  verRuby : TRubyVersion = rvNone;

procedure init_18_19; forward;
const
{$IFDEF UNIX}
  LIB18 = 'libruby18.so';
  LIB19 = 'libruby19.so';
{$ELSE !UNIX}
 {$IFDEF WINDOWS}
  LIB18 = 'mingw-ruby18.dll';
  LIB19 = 'mingw-ruby19.dll';
 {$ELSE !WINDOWS}
  LIB18 = 'ruby18'; // ... I don't know...
  LIB19 = 'ruby19';
 {$ENDIF WINDOWS}
{$ENDIF UNIX}

const
  LIBRARIES : array [succ(rvNone)..high(TRubyVersion)] of record
    name : ansistring;
    init : procedure;
  end = (
    (name : LIB18; init : @init_18_19),
    (name : LIB19; init : @init_18_19)
  );

procedure init_18_19;
 begin

 end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod0);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod1);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod2);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod3);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod4);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod5);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod6);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod7);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod8);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod9);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod10);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod11);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod12);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod13);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod14);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod15);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod16);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethod17);
begin

end;

procedure DefineMethod(cls: VALUE; method: TRubyMethodArr);
begin

end;

function Version : TRubyVersion; inline;
 begin
  Result := verRuby;
 end;

function Load(ver: TRubyVersion): Boolean;
begin

end;

function Load: TRubyVersion;
begin

end;

procedure Unload;
begin

end;

function getActive: Boolean;
 begin
  Result := (Version <> rvNone);
 end;

procedure setActive (val : Boolean);
 begin
  if val <> Version
     then if val
             then Load
             else Unload
 end;

end.

