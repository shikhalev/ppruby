{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ppruby;

interface

uses
  Ruby, RubyClasses, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage( 'ppruby', @Register);
end.
