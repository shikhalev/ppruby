{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ppRuby;

interface

uses
  Ruby, RbTools, RbObjects, RbClasses, RbForms, RbDialogs, RubyConnection, 
  ppRubyDsgn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ppRubyDsgn', @ppRubyDsgn.Register);
end;

initialization
  RegisterPackage('ppRuby', @Register);
end.
