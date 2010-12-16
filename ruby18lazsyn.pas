{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18LazSyn; 

interface

uses
  rb18LazSynRegister, rb18SynEditSource, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('rb18LazSynRegister', @rb18LazSynRegister.Register); 
end; 

initialization
  RegisterPackage('Ruby18LazSyn', @Register); 
end.
