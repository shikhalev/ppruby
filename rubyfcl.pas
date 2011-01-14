{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RubyFCL; 

interface

uses
  ppRuby, ppRubyClasses, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('RubyFCL', @Register); 
end.
