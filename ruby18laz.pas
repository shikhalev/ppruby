{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18Laz; 

interface

uses
    rb18Classes, rb18Script, rb18ScriptSource, rb18System, ruby18, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('rb18Script', @rb18Script.Register); 
  RegisterUnit('rb18ScriptSource', @rb18ScriptSource.Register); 
end; 

initialization
  RegisterPackage('Ruby18Laz', @Register); 
end.
