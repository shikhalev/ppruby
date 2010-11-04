{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazruby; 

interface

uses
  ppRuby18, Ruby18, RubyScript, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RubyScript', @RubyScript.Register); 
end; 

initialization
  RegisterPackage('lazruby', @Register); 
end.
