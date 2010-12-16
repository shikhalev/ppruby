{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18LazLCL; 

interface

uses
  rb18Script, rb18ScriptSource, rb18LazLCLRegister, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('rb18LazLCLRegister', @rb18LazLCLRegister.Register); 
end; 

initialization
  RegisterPackage('Ruby18LazLCL', @Register); 
end.
