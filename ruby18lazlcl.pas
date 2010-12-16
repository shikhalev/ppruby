{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18LazLCL; 

interface

uses
  rb18Script, rb18ScriptSource, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('rb18Script', @rb18Script.Register); 
  RegisterUnit('rb18ScriptSource', @rb18ScriptSource.Register); 
end; 

initialization
  RegisterPackage('Ruby18LazLCL', @Register); 
end.
