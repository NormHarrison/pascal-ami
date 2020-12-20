Program Test_Program(output);

Uses
  Unit_Asterisk_AMI;

Var
  AMI_Passport: TAMIPassport;


Begin
  with AMI_Passport do
  begin
    AMI_Addr     := '172.16.201.11';
    AMI_Port     := 5039;
    AMI_Username := 'CALL_CONTROL';
    AMI_Secret   := 'ThisIsNotAHardPassword!';
  end;

  AMILogin(AMI_Passport);

End.
