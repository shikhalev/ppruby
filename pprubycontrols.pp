unit ppRubyControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ppRuby, ppRubyClasses;

implementation

function m_tcontrol_hide (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Hide;
  Result := instance;
 end;

function m_tcontrol_show (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Show;
  Result := instance;
 end;

function m_tcontrol_refresh (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Refresh;
  Result := instance;
 end;

procedure TControlClassHook (cControl : VALUE);
 begin
  DefineMethod(cControl, 'hide', @m_tcontrol_hide);
  DefineMethod(cControl, 'show', @m_tcontrol_show);
  DefineMethod(cControl, 'refresh', @m_tcontrol_refresh);
 end;

initialization
 ppRuby.AddClassHook(TControl, @TControlClassHook);
end.

