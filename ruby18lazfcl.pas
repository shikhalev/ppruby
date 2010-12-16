{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18LazFCL; 

interface

uses
  rb18Classes, rb18System, ruby18, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('Ruby18LazFCL', @Register); 
end.
