Program Test_Program(output);

Uses
  Unit_Asterisk_AMI, SysUtils;

Var
  AMI_Passport: TAMIPassport;
  AMI_sock:     longint;


Begin
  If ParamCount <> 1 then
  begin
    WriteLn(output, 'Supply channel name');
    Halt(1);
  end;

  with AMI_Passport do
  begin
    AMI_Addr     := '172.16.201.11';
    AMI_Port     :=  5039;
    AMI_Username := 'CALL_CONTROL';
    AMI_Secret   := 'ThisIsNotAHardPassword!';
  end;

  WriteLn('Channel name: ', ParamStr(1));

  AMI_sock := AMILogin(AMI_Passport);
  AMISendAction(AMI_sock, ['Action', 'Hangup', 'Channel', ParamStr(1)]);
  Sleep(500);

End.
