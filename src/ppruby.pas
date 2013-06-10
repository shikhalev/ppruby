{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ppRuby;

interface

uses
  Ruby, RubyClasses, RubyMenus, RubyControls, RubyForms, RubyLink, ValCache, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit( 'RubyLink', @RubyLink.Register);
end;

initialization
  RegisterPackage( 'ppRuby', @Register);
end.
