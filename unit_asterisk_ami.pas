Unit Unit_Asterisk_AMI;


Interface

Uses
  fgl, Sockets, SysUtils;

Type
  { Generic dynamic array of short strings }
  TStringArray = array of string;

  { Maps event field names to their values }
  TEventFieldMap = specialize TFPGMap<string, string>;

  TAMIEvent = record
    Name:      string;
    Privilege: TStringArray;
    Fields:    TEventFieldMap;
  end;

  // Try to find an explanation about defining routine types (these are not called interfaces or prototypes)
  TAMIEventCallback = procedure(var Event: TAMIEvent);

  { A record to be filled with the AMI server's details,
    used for location information and authentication }
  TAMIPassport = record
    Addr:           string;
    Port:           integer;
    Username:       string;
    Secret:         string;
    Event_callback: TAMIEventCallback;
  end;


{ A generic procedure for sending any AMI action based on the provided fields }

Procedure AMISendAction(var AMI_sock: PtrUInt; const Fields: TStringArray);


{ Returns a new socket connected to AMI at the location specified in the
  Passport record. This socket can then be passed to any future call to
  AMISendAction() for the life of the program(?) }

Function AMILogin(var Passport: TAMIPassport): longint;


{=======================================================================================}


Implementation

Uses
  CThreads;

Const
  CRLF: string = #13 + #10;

Var
  Socket_mutex: TRTLCriticalSection;



Function WaitForEvents(PAMI_sock: pointer): PtrInt;

var
  AMI_sock:  PtrUInt;

  Part:      Char;
  Raw_event: Ansistring;
  Event:     TAMIEvent;

begin
  AMI_sock := PtrUInt(PAMI_sock);

  // Start indefinite loop, thread never ends/returns
  While true do

    Raw_event := '';

    EnterCriticalSection(Socket_mutex);
    Try
      Repeat
        fpRecv(AMI_sock, @Part , 1);  // We can't over-estimate the number of bytes to receive as events are strung together and we need to separate them manually
        Raw_event := Raw_event + Part;
        { break when the last 4 characters of Raw_event are #13#10#13#10, we can only check for this once Raw_event has a length or 4 or larger }
      Until false;
    Finally
      LeaveCriticalSection(Socket_mutex);
    end;

  end;
end;



Procedure AMISendAction(var AMI_sock: PtrUInt; const Fields: TStringArray);

Var 
  Action:            ansistring = '';  // Look into using Default()
  Response:          ansistring = '';
  Line:              string = '';
  Part:              Char;
  Field_count:       integer = 0;
  Response_complete: boolean;

begin
  If Length(Fields) mod 2 <> 0 then
  begin
    WriteLn('The fields array must contain an even number of elements');
    CloseSocket(AMI_sock);
    Exit();
  end;

  repeat
    Action := Action + Fields[Field_count * 2] + ':' + ' ' + Fields[Field_count * 2 + 1] + CRLF;

    Inc(Field_count);

    If Field_count = (Length(Fields) Div 2) then
      Action := Action + CRLF;

  until Field_count = (Length(Fields) Div 2);


  EnterCriticalSection(Socket_mutex);
  Try
    fpSend(AMI_sock, Pointer(Action), Length(Action), 0);
  Finally
    LeaveCriticalSection(Socket_mutex);
  end;

  //Sleep(50);  If the socket remains open in the event listening thread, then maybe this isn't needed

  //If Fields[1] <> 'Login' then CloseSocket(AMI_sock);

{
  repeat
    Line := '';
    repeat
      fpRecv(AMI_sock, @Part, SizeOf(Part), 0);
      Line := Line + Part;
    until Pos(CRLF, Line) <> 0;

    Response := Response + Line;
    Write(Line);
  until false;
}
  //WriteLn(Response);
end;



Function AMILogin(var Passport: TAMIPassport): longint;  // We shouldn't need critical sections in the login procedure

Var
  AMI_sock:    PtrUInt;
  Bind_info:   TSockAddr;
  Remote_info: TSockAddr;

begin
  AMI_sock := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_IP);

  with Bind_info do
  begin
    sin_family := AF_INET;
    sin_addr   := StrToNetAddr('0.0.0.0');
    sin_port   := HtoNs(0);
  end;

  If fpBind(AMI_sock, @Bind_info, SizeOf(Bind_info)) = -1 then
  begin
    WriteLn('Failed to bind local socket');
    Exit(-1);
  end;

  with Passport, Remote_info do
  begin
    sin_family := AF_INET;
    sin_addr   := StrToNetAddr(Addr);
    sin_port   := HtoNs(Port);
  end;

  If fpConnect(AMI_sock, @Remote_info, SizeOf(Remote_info)) = -1 then
  begin;
    WriteLn(Format('The AMI server located at "%s:%d" could not be reached',
    [Passport.Addr, Passport.Port]));
    Exit(-1);
  end;

  AMISendAction(AMI_sock,
  ['Action',  'Login',
   'Username', Passport.Username,
   'Secret',   Passport.Secret]);

  { Start listening for events in a new thread }
  BeginThread(@WaitForEvents, pointer(AMI_sock));

  AMILogin := AMI_sock;
end;



Initialization
  InitCriticalSection(Socket_mutex);



Finalization
  { If the program is quit via external operating system
    means or raises an exception, this won't be called }
  DoneCriticalSection(Socket_mutex);


End.
