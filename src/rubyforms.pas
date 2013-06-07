unit RubyForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, Forms, Ruby;

implementation

function get_application (slf : VALUE) : VALUE; cdecl;
 var
   rb : TRuby;
 begin
  rb := TRuby.EngineByModule(slf);
  try
    result := rb.Obj2Val(Application);
  except
    on e : Exception do
       rb.Error(e);
  end;
 end;

function get_screen (slf : VALUE) : VALUE; cdecl;
 var
   rb : TRuby;
 begin
  rb := TRuby.EngineByModule(slf);
  try
    result := rb.Obj2Val(Screen);
  except
    on e : Exception do
       rb.Error(e);
  end;
 end;

procedure hookForms (ruby : TRuby);
 var
   u : VALUE;
 begin
  u := ruby.RegisterUnit('Forms');
  ruby.DefineSingletonMethod(ruby.mPascal, 'application', @get_application);
  ruby.DefineSingletonMethod(u, 'application', @get_application);
  ruby.DefineGlobalFunction('application', @get_application);
  ruby.DefineSingletonMethod(ruby.mPascal, 'screen', @get_screen);
  ruby.DefineSingletonMethod(u, 'screen', @get_screen);
  ruby.DefineGlobalFunction('screen', @get_screen);
 end;

function cap_exename (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Str2Val((p.obj as TCustomApplication).ExeName);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_helpfile (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Str2Val((p.obj as TCustomApplication).HelpFile);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_helpfile_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TCustomApplication).HelpFile := p.rb.Val2Str(value);
    result := value;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_title (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Str2Val((p.obj as TCustomApplication).Title);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_title_set (slf : VALUE; value : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    (p.obj as TCustomApplication).Title := p.rb.Val2Str(value);
    result := value;
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_console (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Bln2Val((p.obj as TCustomApplication).ConsoleApplication);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function cap_location (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Str2Val((p.obj as TCustomApplication).Location);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTCustomApplication (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineAttribute(value, 'exename',  @cap_exename,  nil);
  ruby.DefineAttribute(value, 'helpfile', @cap_helpfile, @cap_helpfile_set);
  ruby.DefineAttribute(value, 'title',    @cap_title,    @cap_title_set);
  ruby.DefineAttribute(value, 'console',  @cap_console,  nil, true);
  ruby.DefineAttribute(value, 'location', @cap_location, nil);
 end;
{$hints on}

function scr_activecontrol (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TScreen).ActiveControl);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

function scr_activeform (slf : VALUE) : VALUE; cdecl;
 var
   p : TPack;
 begin
  unpack_object(slf, p);
  try
    result := p.rb.Obj2Val((p.obj as TScreen).ActiveForm);
  except
    on e : Exception do
       p.rb.Error(e);
  end;
 end;

{$hints off}
procedure hookTScreen (ruby : TRuby; cls : TClass; value : VALUE);
 begin
  ruby.DefineAttribute(value, 'activecontrol', @scr_activecontrol, nil);
  ruby.DefineAttribute(value, 'activeform',    @scr_activeform, nil);
  {$note todo: cursor etc.}
 end;
{$hints on}

initialization
 TRuby.AddInitHook(@hookForms);
 TRuby.AddRegisterClassHook(TCustomApplication, @hookTCustomApplication);
 TRuby.AddRegisterClassHook(TScreen,            @hookTScreen);
end.

