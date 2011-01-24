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

function m_tcontrol_parent (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).Parent);
 end;

function m_tcontrol_parent_set (instance : VALUE; parent : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Parent := TObject(parent) as TWinControl;
  Result := parent;
 end;

procedure TControlClassHook (cControl : VALUE);
 begin
  DefineMethod(cControl, 'hide', @m_tcontrol_hide);
  DefineMethod(cControl, 'show', @m_tcontrol_show);
  DefineMethod(cControl, 'refresh', @m_tcontrol_refresh);
  DefineMethod(cControl, 'parent', @m_tcontrol_parent);
  DefineMethod(cControl, 'parent=', @m_tcontrol_parent_set);
 end;

function m_twincontrol_canfocus (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TWinControl).CanFocus);
 end;

function m_twincontrol_focused (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TWinControl).Focused);
 end;

function m_twincontrol_setfocus (instance : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TWinControl).SetFocus;
  Result := instance;
 end;

procedure TWinControlClassHook (cWinControl : VALUE);
 begin
  DefineMethod(cWinControl, 'canfocus?', @m_twincontrol_canfocus);
  DefineMethod(cWinControl, 'focused?', @m_twincontrol_focused);
  DefineMethod(cWinControl, 'setfocus', @m_twincontrol_setfocus);
 end;

initialization
 ppRuby.AddClassHook(TControl, @TControlClassHook);
end.

