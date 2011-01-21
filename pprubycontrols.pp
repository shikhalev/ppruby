unit ppRubyControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ppRuby, ppRubyClasses;

operator explicit (v : TAlign) : VALUE;
operator explicit (v : VALUE) : TAlign;

operator explicit (v : TAnchorKind) : VALUE;
operator explicit (v : VALUE) : TAnchorKind;

operator explicit (v : TAnchors) : VALUE;
operator explicit (v : VALUE) : TAnchors;

operator explicit (v : TAnchorSideReference) : VALUE;
operator explicit (v : VALUE) : TAnchorSideReference;

implementation

var
  cacheTAlignValues : array [TAlign] of VALUE;
  cacheTAnchorKindValues : array [TAnchorKind] of VALUE;
  cacheTAnchorSideReferenceValues : array [0..1, TAnchorSideReference] of VALUE;

procedure InitEnumCaches;
 begin
  cacheTAlignValues[alNone]   := VALUE(ID('alNone'));
  cacheTAlignValues[alTop]    := VALUE(ID('alTop'));
  cacheTAlignValues[alBottom] := VALUE(ID('alBottom'));
  cacheTAlignValues[alLeft]   := VALUE(ID('alLeft'));
  cacheTAlignValues[alRight]  := VALUE(ID('alRight'));
  cacheTAlignValues[alClient] := VALUE(ID('alClient'));
  cacheTAlignValues[alCustom] := VALUE(ID('alCustom'));

  cacheTAnchorKindValues[akTop]    := VALUE(ID('akTop'));
  cacheTAnchorKindValues[akLeft]   := VALUE(ID('akLeft'));
  cacheTAnchorKindValues[akRight]  := VALUE(ID('akRight'));
  cacheTAnchorKindValues[akBottom] := VALUE(ID('akBottom'));

  cacheTAnchorSideReferenceValues[0, asrTop]    := VALUE(ID('asrTop'));
  cacheTAnchorSideReferenceValues[0, asrBottom] := VALUE(ID('asrBottom'));
  cacheTAnchorSideReferenceValues[0, asrCenter] := VALUE(ID('asrCenter'));
  cacheTAnchorSideReferenceValues[1, asrLeft]   := VALUE(ID('asrLeft'));
  cacheTAnchorSideReferenceValues[1, asrRight]  := VALUE(ID('asrRight'));
  cacheTAnchorSideReferenceValues[1, asrCenter] := VALUE(ID('asrCenter'));
 end;

operator explicit (v : TAlign) : VALUE;
 begin
  Result := cacheTAlignValues[v];
 end;

operator explicit (v : VALUE) : TAlign;
 begin
  case ValType(v) of
       rtFixNum :
         exit(TAlign(PtrInt(v)));
       rtSymbol :
         for Result in TAlign do
             if cacheTAlignValues[Result] = v
                then exit;
  end;
  raise ERubyConversion.CreateFmt(msgCanNotConvertTo, [ansistring(Inspect(v)), 'TAlign']);
 end;

operator explicit (v : TAnchorKind) : VALUE;
 begin
  Result := cacheTAnchorKindValues[v];
 end;

operator explicit (v : VALUE) : TAnchorKind;
 begin
  case ValType(v) of
       rtFixNum :
         exit(TAnchorKind(PtrInt(v)));
       rtSymbol :
         for Result in TAnchorKind do
             if cacheTAnchorKindValues[Result] = v
                then exit;
  end;
  raise ERubyConversion.CreateFmt(msgCanNotConvertTo, [ansistring(Inspect(v)), 'TAnchorKind']);
 end;

operator explicit (v : TAnchors) : VALUE;
 var
   arr : array of VALUE;
   len : Integer;
   item : TAnchorKind;
 begin
  SetLength(arr, 0);
  for item in v do
      begin
       len := Length(arr);
       SetLength(arr, len + 1);
       arr[len] := VALUE(item);
      end;
  Result := MakeArray(arr);
  SetLength(arr, 0);
 end;

operator explicit (v : VALUE) : TAnchors;
 var
   item : TAnchorKind;
 begin
  Result := [];
  for item in TAnchorKind do
      if ArrayIncludes(v, VALUE(item))
         then Include(Result, item);
 end;

operator explicit (v : TAnchorSideReference) : VALUE;
 begin
  Result := cacheTAnchorSideReferenceValues[0, v];
 end;

operator explicit (v : VALUE) : TAnchorSideReference;
 var
   i : Integer;
 begin
  case ValType(v) of
       rtFixNum :
         exit(TAnchorSideReference(PtrInt(v)));
       rtSymbol :
         for i := 0 to 1 do
             for Result in TAnchorSideReference do
                 if cacheTAnchorSideReferenceValues[i, Result] = v
                    then exit;
  end;
  raise ERubyConversion.CreateFmt(msgCanNotConvertTo, [ansistring(Inspect(v)), 'TAnchorSideReference']);
 end;

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

function m_tcontrol_action (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).Action);
 end;

function m_tcontrol_action_set (instance : VALUE; action : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Action := TObject(action) as TBasicAction;
  Result := action;
 end;

function m_tcontrol_align (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).Align);
 end;

function m_tcontrol_align_set (instance : VALUE; align : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Align := TAlign(align);
  Result := align;
 end;

function m_tcontrol_anchors (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).Anchors);
 end;

function m_tcontrol_anchors_set (instance : VALUE; anchors : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).Anchors := TAnchors(anchors);
  Result := anchors;
 end;

function m_tcontrol_anchorside (instance : VALUE; kind : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).AnchorSide[TAnchorKind(kind)]);
 end;

function m_tcontrol_autosize (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).AutoSize);
 end;

function m_tcontrol_autosize_set (instance : VALUE; autosize : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).AutoSize := Boolean(autosize);
  Result := autosize;
 end;

function m_tcontrol_borderspacing (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControl).BorderSpacing);
 end;

function m_tcontrol_borderspacing_set (instance : VALUE; spacing : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControl).BorderSpacing := TObject(spacing) as TControlBorderSpacing;
  Result := spacing;
 end;

procedure TControlClassHook (cControl : VALUE);
 begin
  DefineMethod(cControl, 'hide', @m_tcontrol_hide);
  DefineMethod(cControl, 'show', @m_tcontrol_show);
  DefineMethod(cControl, 'refresh', @m_tcontrol_refresh);
  DefineMethod(cControl, 'action', @m_tcontrol_action);
  DefineMethod(cControl, 'action=', @m_tcontrol_action_set);
  DefineMethod(cControl, 'align', @m_tcontrol_align);
  DefineMethod(cControl, 'align=', @m_tcontrol_align_set);
  DefineMethod(cControl, 'anchors', @m_tcontrol_anchors);
  DefineMethod(cControl, 'anchors=', @m_tcontrol_anchors_set);
  DefineMethod(cControl, 'anchorside', @m_tcontrol_anchorside);
  DefineMethod(cControl, 'autosize', @m_tcontrol_autosize);
  DefineMethod(cControl, 'autosize=', @m_tcontrol_autosize_set);
  DefineMethod(cControl, 'borderspacing', @m_tcontrol_borderspacing);
  DefineMethod(cControl, 'borderspacing=', @m_tcontrol_borderspacing_set);
 end;

function m_tanchorside_owner (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TAnchorSide).Owner);
 end;

function m_tanchorside_kind (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TAnchorSide).Kind);
 end;

function m_tanchorside_control (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TAnchorSide).Control);
 end;

function m_tanchorside_control_set (instance : VALUE; control : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TAnchorSide).Control := TObject(control) as TControl;
  Result := control;
 end;

function m_tanchorside_side (instance : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TAnchorSide).Side);
 end;

function m_tanchorside_side_set (instance : VALUE; side : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TAnchorSide).Side := TAnchorSideReference(side);
  Result := side;
 end;

procedure TAnchorSideClassHook (cAnchorSide : VALUE);
 begin
  DefineMethod(cAnchorSide, 'owner', @m_tanchorside_owner);
  DefineMethod(cAnchorSide, 'kind', @m_tanchorside_kind);
  DefineMethod(cAnchorSide, 'control', @m_tanchorside_control);
  DefineMethod(cAnchorSide, 'control=', @m_tanchorside_control_set);
  DefineMethod(cAnchorSide, 'side', @m_tanchorside_side);
  DefineMethod(cAnchorSide, 'side=', @m_tanchorside_side_set);
 end;

function m_tcontrolborderspacing_isequal (instance : VALUE; other : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControlBorderSpacing).IsEqual(TObject(other) as TControlBorderSpacing));
 end;

function m_tcontrolborderspacing_get_space (instance : VALUE; kind : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControlBorderSpacing).Space[TAnchorKind(kind)]);
 end;

function m_tcontrolborderspacing_set_space (instance : VALUE; kind : VALUE; space : VALUE) : VALUE; cdecl;
 begin
  (TObject(instance) as TControlBorderSpacing).Space[TAnchorKind(kind)] := PtrInt(space);
  Result := space;
 end;

function m_tcontrolborderspacing_get_sidespace (instance : VALUE; kind : VALUE) : VALUE; cdecl;
 begin
  Result := VALUE((TObject(instance) as TControlBorderSpacing).GetSideSpace(TAnchorKind(kind)));
 end;

procedure TControlBorderSpacingClassHook (cControlBorderSpacing : VALUE);
 begin
  DefineMethod(cControlBorderSpacing, 'isequal?', @m_tcontrolborderspacing_isequal);
  DefineAlias(cControlBorderSpacing, '===', 'isequal?');
  DefineMethod(cControlBorderSpacing, 'get_space', @m_tcontrolborderspacing_get_space);
  DefineMethod(cControlBorderSpacing, 'set_space', @m_tcontrolborderspacing_set_space);
  DefineMethod(cControlBorderSpacing, 'get_sidespace', @m_tcontrolborderspacing_get_sidespace);
 end;

initialization
 ppRuby.AddLoadHook(@InitEnumCaches);
 ppRuby.AddClassHook(TControl, @TControlClassHook);
 ppRuby.AddClassHook(TAnchorSide, @TAnchorSideClassHook);
 ppRuby.AddClassHook(TControlBorderSpacing, @TControlBorderSpacingClassHook);
end.

