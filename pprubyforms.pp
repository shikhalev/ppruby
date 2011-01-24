unit ppRubyForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppRuby, ppRubyClasses, Forms;

implementation

// TODO: TCustomForm, TApplication... and all others

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

initialization
 ppRuby.AddLoadHook(@InitFunctions);
end.
