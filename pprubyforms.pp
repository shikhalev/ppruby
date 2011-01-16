unit ppRubyForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppRuby, ppRubyClasses, Forms;

operator explicit (v : VALUE) : TWindowState;
operator explicit (v : TWindowState) : VALUE;

implementation

var
  cacheWindowStates : array [TWindowState] of VALUE;

procedure InitWindowStates;
 begin
  cacheWindowStates[wsNormal]    := VALUE(ID('wsNormal'));
  cacheWindowStates[wsMinimized] := VALUE(ID('wsMinimized'));
  cacheWindowStates[wsMaximized] := VALUE(ID('wsMaximized'));
 end;

function gf_application : VALUE; cdecl;
 begin
  Result := VALUE(Application);
 end;

function gf_screen : VALUE; cdecl;
 begin
  Result := VALUE(Screen);
 end;

procedure InitFunctions;
 begin
  DefineFunction('application', @gf_application);
  DefineFunction('screen', @gf_screen);
 end;

operator explicit (v : VALUE) : TWindowState;
 begin
  case ValType(v) of
       rtFixNum :
         exit(TWindowState(PtrInt(v)));
       rtSymbol :
         for Result := Low(TWindowState) to High(TWindowState) do
             if cacheWindowStates[Result] = v
                then exit;
  end;
  raise ERubyConversion.CreateFmt(msgCanNotConvertTo, [ansistring(Inspect(v)), 'TWindowState']);
 end;

operator explicit (v : TWindowState) : VALUE;
 begin
  Result := cacheWindowStates[v];
 end;

initialization
 ppRuby.AddLoadHook(@InitWindowStates);
 ppRuby.AddLoadHook(@InitFunctions);
end.
