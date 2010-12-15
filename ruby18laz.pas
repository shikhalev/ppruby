{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Ruby18Laz; 

interface

uses
  Ruby18Script, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Ruby18Script', @Ruby18Script.Register); 
end; 

initialization
  RegisterPackage('Ruby18Laz', @Register); 
end.
