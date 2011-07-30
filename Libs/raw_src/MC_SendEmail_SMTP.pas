(*****************************************************************************************************
* 				Library  "MPLCCommunication_Addins" for MOVI-PLC (R)            
******************************************************************************************************
* Provides function blocks for MOVI-PLC (R) 
* (c) 2009 by SEW-EURODRIVE Argentina S.A.
******************************************************************************************************
*
* MC_MailSender_SMTP
* This function block allows the MOVI-PLC (R) to send E-Mails to any destination with 
* any type of files attached!.
* The implementation of the protocol is based on the RFC 2821.
*
*********************************************** HELP *************************************************
* NOTICE:
*	- To use this function block you must configure the plc's IP address with an IP
*	   under the same network range of your SMTP server and the gateway as well.
*	   To do so you must modify the file "\System\NetConfig.cfg" on the MOVI-PLC 
*	   Advanced SD card.
*	- If authentication is required, fill in with Username and Password, else leave empty 
*	- To send to multiple recipients, separate them with a semicolon ";"  without any 
	   space between them.
*	- The maximum amount of multiple recipients is 10 users approximately, and the 
*	   maximun lenght of the STRING data type is 255 characters, so you must not 
*	   exceed it!
*	- To attach a file fill the "FileToAttach" field with the name and extension of such file,
*	   else leave empty.
*
******************************************************************************************************)

FUNCTION BLOCK MC_SendEmail_SMTP
VARINPUT
	Execute: BOOL;			(* Enable at Rising Edge *)
	ServerIPAddr: STRING(15); 	(* SMTP server IP address *)
	ServerPort:UINT;			(* SMTP port on the server - Commonly port number 25 *)
	NameToShow: STRING(80);	(* Name to show in the "FROM" field on you e-mail viewer - eg:"Machine 1" *)
	Username: STRING(80);		(* Account username *)
	Password: STRING(80);		(* Account password *)
	MailFrom: STRING(80);		(* Address from which the e-mail will be sent *)
	MailTo: DWORD;		 	(* Memory address (ADR) of the E-mail destination (STRING)- eg: ADR(Recipients) *)
	Subject: STRING(80);		(* E-mail subject *)
	EmailContent: DWORD;		(* Memory address (ADR) of the E-mail body (STRING) - eg: ADR(TextToSend) *)
	FileToAttach:STRING(80);	(* Name of the attached file *)
	TimeOut: UINT;				(* TimeOut in [ms] *)
END_VAR

VAR_OUTPUT
	Done: BOOL;		(* E-Mail Sent *)
	Busy: BOOL;		(* Module is working *)
	Error: BOOL;		(* Error *)
	ErrorID: DWORD;	(* Error Code *)
	LoggedIn: BOOL;	(* Authentication OK *)
	Connected: BOOL; 	(* Server is conected *)
	Completed:INT;		(* Percent [%] of the sending process - e.g. 25 of 100% *)
END_VAR

{library private}
{flag nowatch on}
VAR
	(* Standard *)
	fbTCP_InitComm: MC_InitClient_TCP;		(* Initializes the communication with the SMTP server *)
	fbTCP_Receive: MC_ReceiveEnable_TCP;	(* Receives the commands from the SMTP server *)
	fbTCP_Send: MC_Send_TCP;				(* Sends data to server *)
	bCommON: BOOL;							(* Flag to start the communication between server and movi-plc *)
	bSendTCP: BOOL;							(* Flag to enable the sending commands *)
	bReceiveTCP: BOOL;						(* Flag to enable the reception of acknowledge *)
	nTxLen: UINT;								(* Lenght of the sending data *)
	fbStartTrigger: R_TRIG;						(* Rising edge for the module to execute *)
	fbEndTrigger: F_TRIG;						(* Falling edge to reset *)
	bStartFlag: BOOL;							(* Flag to start the sending process *)
	nProcessState: INT;							(* Step of the state machine *)

	(* HeartBeat  Mode *)
	fbHeartbeatTimer : TON;						(* Timer of the Heartbeat mode *)
	nLastProcessState:INT;						(* Last step *)

	(* File Variables *)
	fbSMTPOpenFile: SysFileOpenAsync;		(* Open file to read its content *)
	fbSMTPReadFile: SysFileReadAsync;		(* Read the content of the file in parts of 1026 bytes to send it then *)
	fbSMTPGetSizeFile: SysFileGetSizeAsync;	(* Gets the size of the file to calculate the percentual of the file sending process *)
	fbSMTPCloseFile: SysFileCloseAsync;		(* Closes read file *)
	dwFileSize: DWORD;						(* Saves the amount of read bytes of the file *)

	(* Temp variables *)
	pMailTo: POINTER TO STRING;					(* Pointer to the string of the Mail To *)
	pEmailContent: POINTER TO STRING;			(* Pointer to the Email body *)
	sStringTemp: STRING;							(* Saves strings to work with them after *)
	ayDataTemp: ARRAY [0..1459] OF BYTE;		(* Array that saves sent and received data *)
	ayBase64BufferIn:ARRAY[0..1459]OF BYTE;		(* Input buffer to convert to Base64 *)
	ayBase64BufferOut: ARRAY[0..1459] OF BYTE;	(* Buffer converted to Base64 *)
	fbConvertToBase64: fbBase64Encoding;			(* Convert a file to Base64 in chunks of 1026 bytes *)
	sHelloType: STRING;							(* Hello Type, EHLO or HELO *)
	nStrErrorID: INT;				(* Gets the first 3 characters of StringTemp to compare them against the error codes *)
	nStep: UINT;				(* Step for the division of converted array on base64 in an array of 76 characters and adds a CR\LF *)
	nPack76: UINT;				(* Amount of complete lines of 76 characters each *)
	nRestPack76: UINT;			(* Rest of the Amount *)
	nInSemicolon: UINT;			(* Index of the semicolon in the string *)
	sRecipient: STRING;		(* E-mail address of the recipient *)
	nInLastSemi: UINT;			(* Save the last position of the semicolon *)
	sNewMailTo: STRING;		(* New string with the modified recipients *)
	nPrevLen: UINT;				(* Save the previous Length of the StringTemp *)
	bNotAttachedFile: BOOL;	(* Flag to know if are file to attach *)
END_VAR
{flag off}


(*****************************************************************************************************************************************************
	Programmed:	28.01.2009	Cristian Adamo  <cristian.adamo@sew-eurodrive.com.ar>  -  SEW EURODRIVE Argentina S.A.	
	Current Version: 0.5 release		
	Change:	Ver			Date			Author				Description:
				v0.1		 	28.01.2009		Cristian Adamo		Start Project
				v0.2		 	29.01.2009		Cristian Adamo		First implementation
				v0.3 	 	04.02.2009		Cristian Adamo		Rising Edge Start, Error Handling, Subject 
				v0.4			29.04.2009		Cristian Adamo		Code Optimization
				v0.5			18.08.2009		Cristian Adamo 		Add MIME and BASE64 encoding to the file send
				v0.6			20.08.2009		Cristian Ada	mo        Fix error on BASE64 file sending
											Carlos Ravazzano
				v0.7			21.08.2009		Cristian Adamo		Fix error at sending files, resets and Code Optimization
				v0.8			24.08.2009		Cristian Adamo		Code optimization and Fixes
				v0.8 rev 1	31.08.2009		Cristian Adamo		Corrections
******************************************************************************************************************************************************)

(***********************************************************************************************)
(****************************** Framework - Edges for Execute ***************************)
(***********************************************************************************************)
fbStartTrigger(CLK := Execute);
fbEndTrigger(CLK := Execute);

(******* Rising Edge Execute *******)
IF ( fbStartTrigger.Q ) THEN
	nProcessState := 0;
	nLastProcessState := -1;
	bStartFlag := TRUE;
END_IF

(******* Falling Edge Execute *******)
IF ( fbEndTrigger.Q AND bStartFlag = FALSE ) THEN
	Done := FALSE;
	Busy := FALSE;
	Error := FALSE;
	ErrorID := 0;
	Loggedin := FALSE;
	Connected := FALSE;
	nStrErrorID := 0;
	Completed := 0;
END_IF


(*********************************************************************************************)
(******************************* Check active errors **************************************)
(*********************************************************************************************)
IF ( Error ) THEN
	nProcessState := 320;
	Done := FALSE;
	Busy := FALSE;
	Loggedin := FALSE;
	Connected := FALSE;
END_IF



 (***************************************************************************************************************************)
 (*********************************************** Framework - State Machine ******************************************)
 (***************************************************************************************************************************)
IF ( bStartFlag ) THEN

	(*********************************************************************************************)
	(*********************************** Hearbeat Mode ***************************************)
	(*********************************************************************************************)
	IF ( nLastProcessState <> nProcessState ) THEN
		fbHeartbeatTimer(IN:=FALSE , PT:= UINT_TO_TIME(TimeOut));
	ELSE
		fbHeartbeatTimer(IN:=TRUE , PT:= UINT_TO_TIME(TimeOut));
	END_IF
	nLastProcessState := nProcessState;


	(*********************************************************************************************)
	(*********************************** State Machine ****************************************)
	(*********************************************************************************************)
	CASE ( nProcessState ) OF
		0:		(* Connects with the SMTP Server and checks if are file to send*)
				bCommON := TRUE;

				IF( FileToAttach = '') THEN
					bNotAttachedFile := TRUE;
				ELSE
					bNotAttachedFile := FALSE;
				END_IF

				IF ( fbTCP_InitComm.Done ) THEN
					nProcessState := 10;
				END_IF


		10:		(* Waiting Welcome Message *)
				bReceiveTCP := TRUE;

				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '220' ) THEN
						bReceiveTCP := FALSE;
						Busy := TRUE;
						Connected := TRUE;
						nProcessState := 20;
					END_IF
				END_IF


		20:		(* Sends HELO command *)
				IF (Username = '' AND Password = '') THEN
					sHelloType := 'HELO';
				ELSE
					sHelloType := 'EHLO';
				END_IF

				sStringTemp := CONCAT( sHelloType ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 30;
				END_IF


		30:		(* Waits the acknowledge of EHLO or HELO command *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '250' ) THEN
						bReceiveTCP := FALSE;
						IF (Username = '' AND Password = '') THEN
							nProcessState := 100; (* Without Authentication *)
						ELSE
							nProcessState := 40; (* With Authentication*)
						END_IF
					END_IF
				END_IF


		40: 		(* Sends AUTH LOGIN command *)
				sStringTemp := CONCAT( 'AUTH LOGIN' ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 50;
				END_IF


		50:		(* Waits the request of Username *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 16);
					sStringTemp := MID( sStringTemp, 16, 1);

					IF ( sStringTemp = '334 VXNlcm5hbWU6' ) THEN
						bReceiveTCP := FALSE;
						sStringTemp := '';
						nProcessState := 60;
					END_IF
				END_IF


		60: 		(* Sends the Username encoded in BASE64*)
				fbConvertToBase64(
								Enable:= TRUE,
								InputBuffer:= ADR(Username),
								Size:= LEN(Username),
								OutputBuffer:= ADR(sStringTemp),
								 );

				IF (fbConvertToBase64.Done) THEN
					sStringTemp := MID(sStringTemp, DWORD_TO_INT(fbConvertToBase64.dwRead), 1);
					sStringTemp := CONCAT( sStringTemp ,'$R$N');
					fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));
					bSendTCP := TRUE;
					nTxLen := LEN(sStringTemp);

					IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
						bSendTCP := FALSE;
						bReceiveTCP := TRUE;
						nProcessState := 70;
					END_IF
				END_IF


		70: 		(* Waits the request of Password *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 16);
					sStringTemp := MID( sStringTemp, 16, 1);

					IF ( sStringTemp = '334 UGFzc3dvcmQ6' ) THEN
						bReceiveTCP := FALSE;
						sStringTemp := '';
						nProcessState := 80;
					END_IF
				END_IF


		80:	 	(* Sends the Password encoded in BASE64*)
				fbConvertToBase64(
								Enable:= TRUE,
								InputBuffer:= ADR(Password),
								Size:= LEN(Password),
								OutputBuffer:= ADR(sStringTemp),
								 );

				IF ( fbConvertToBase64.Done ) THEN
					sStringTemp := MID(sStringTemp, DWORD_TO_INT(fbConvertToBase64.dwRead), 1);
					sStringTemp := CONCAT( sStringTemp ,'$R$N');
					fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));
					bSendTCP := TRUE;
					nTxLen := LEN(sStringTemp);

					IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
						bSendTCP := FALSE;
						bReceiveTCP := TRUE;
						fbConvertToBase64( Enable:= FALSE );
						nProcessState := 90;
					END_IF
				END_IF


		90:	 	(* Waits confirmation of the Password *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '235' ) THEN
						bReceiveTCP := FALSE;
						Loggedin := TRUE;
						nProcessState := 100;
					END_IF
				END_IF


		100:	(* Sends MAIL FROM: Command *)
				sStringTemp := CONCAT( CONCAT('MAIL FROM: ', MailFrom ) ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 110;
				END_IF


		110:	(* Waits acknowledge of MAIL FROM command *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '250' ) THEN
						bReceiveTCP := FALSE;
						pMailTo := MailTo;
						sNewMailTo := pMailTo^;
						nInSemiColon := 0;
						nInLastSemi := 0;
						nProcessState := 120;
					END_IF
				END_IF


		120: 	(* Sends E-Mail to multiples recipients*)
				nInSemiColon := FIND(sNewMailTo, ';');
				IF ( nInSemiColon <> 0 ) THEN
					sNewMailTo := REPLACE( sNewMailTo,',', 1, nInSemiColon);
				END_IF

				IF ( nInSemiColon > nInLastSemi ) THEN
					IF ( nInLastSemi = 0 ) THEN
						sRecipient := MID(sNewMailTo, (nInSemiColon-nInLastSemi)-1, 1);
					ELSE
						sRecipient := MID(sNewMailTo, (nInSemiColon-nInLastSemi)-1, (nInSemiColon-nInLastSemi));
					END_IF
					nProcessState := 130; (* Sends Recipient *)
				ELSIF ( nInSemiColon = 0 AND nInLastSemi = 0 ) THEN
					sRecipient := sNewMailTo;
					nProcessState := 130; (* Sends recipient *)
				ELSIF ( nInSemiColon = 0 AND nInLastSemi > 0 ) THEN
					sRecipient := MID(sNewMailTo, LEN(sNewMailTo)-nInLastSemi , nInLastSemi+1 );
					nProcessState := 130; (* Sends recipient *)
				END_IF


		130:	(* Sends RCPT TO: Command *)
				sStringTemp := CONCAT( CONCAT('RCPT TO: ', sRecipient) ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND  fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 140;
				END_IF


		140:	(* Waits Acknowledge of RCPT TO command *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '250' ) THEN
						bReceiveTCP := FALSE;
						IF ( nInSemiColon > nInLastSemi ) THEN
							nProcessState := 120; (* Sends next Recipient *)
							nInLastSemi := nInSemiColon;
						ELSE
							nProcessState := 150; (* Continues the process *)
						END_IF
					END_IF
				END_IF


		150:	(* Sends DATA Command  *)
				sStringTemp := CONCAT('DATA ' ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 160;
				END_IF


		160:	(* Waits confirmation of DATA command *)
				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '354' ) THEN
						bReceiveTCP := FALSE;
						nProcessState := 170;
					END_IF
				END_IF


		170:	(* Sends the From tag *)
				sStringTemp := CONCAT ( CONCAT( CONCAT( 'From: ', NameToShow ), CONCAT(' <', CONCAT( MailFrom, '>')) ), '$R$N');
				sStringTemp := CONCAT ( sStringTemp ,'To: ');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := LEN(sStringTemp);

				sStringTemp := sNewMailTo;
				fMemCpy( ADR(ayDataTemp)+nPrevLen, ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := nPrevLen + LEN(sStringTemp);

				sStringTemp := CONCAT ( CONCAT('$R$NSubject: ' , Subject), '$R$N');
				fMemCpy( ADR(ayDataTemp)+UINT_TO_DWORD(nPrevLen), ADR(sStringTemp), LEN(sStringTemp) );

				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp)+nPrevLen;

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					nProcessState := 180;
				END_IF;


	(*******************************************************************************************************)
	(**************************************** MIME EXTENSION ***************************************)
	(*******************************************************************************************************)
		180:	(* MIME tags for the Email Content*)
				sStringTemp := CONCAT( 'MIME-Version: 1.0' ,'$R$N');
				sStringTemp := CONCAT( sStringTemp, CONCAT( 'Content-TYPE: multipart/mixed;' ,'$R$N'));
				sStringTemp := CONCAT( sStringTemp, CONCAT( '	boundary="StartBoundary"' ,'$R$N$R$N'));
				sStringTemp := CONCAT( sStringTemp, CONCAT( 'This is a multi-part message in MIME format.' ,'$R$N$R$N'));
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := LEN(sStringTemp);

				sStringTemp := CONCAT( '--StartBoundary' ,'$R$N');
				sStringTemp := CONCAT( sStringTemp, CONCAT( 'Content-Type: text/plain;' ,'$R$N'));
				sStringTemp := CONCAT( sStringTemp, CONCAT( '	format=flowed;' ,'$R$N'));
				fMemCpy( ADR(ayDataTemp)+UINT_TO_DWORD(nPrevLen), ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := nPrevLen+LEN(sStringTemp);

				sStringTemp := CONCAT( '	charset="iso-8859-1";' ,'$R$N');
				sStringTemp := CONCAT( sStringTemp, CONCAT( '	reply-type=original' ,'$R$N'));
				sStringTemp := CONCAT( sStringTemp, CONCAT( 'Content-Transfer-Encoding: 7bit' ,'$R$N$R$N'));
				fMemCpy( ADR(ayDataTemp)+UINT_TO_DWORD(nPrevLen), ADR(sStringTemp), LEN(sStringTemp) );

				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp)+nPrevLen;

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					nProcessState := 190;
				END_IF;


		190:	(* Sends the body of the e-mail*)
				pEmailContent := EmailContent;
				fMemCpy( ADR(ayDataTemp), EmailContent, LEN(pEmailContent^) );
				sStringTemp := '$R$N';
				fMemCpy( ADR(ayDataTemp)+LEN(pEmailContent^), ADR(sStringTemp), LEN(sStringTemp) );

				bSendTCP := TRUE;
				nTxLen := LEN(pEmailContent^)+LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					IF ( bNotAttachedFile ) THEN
						nProcessState := 280;
					ELSE
						nProcessState := 200;
					END_IF
				END_IF;


		(************************************* ATTACHMENT OF THE FILE **************************************)
		200:	(* MIME tags for the attached file *)
				sStringTemp := CONCAT( '--StartBoundary' ,'$R$N');
				sStringTemp := CONCAT( sStringTemp, CONCAT( 'Content-Type: application/octet-stream;' ,'$R$N'));
				sStringTemp := CONCAT( sStringTemp, '	name="');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := LEN(sStringTemp);

				sStringTemp := CONCAT( FileToAttach ,'"$R$N');
				sStringTemp := CONCAT( sStringTemp , CONCAT( 'Content-Transfer-Encoding: base64' ,'$R$N'));
				fMemCpy( ADR(ayDataTemp)+UINT_TO_DWORD(nPrevLen), ADR(sStringTemp), LEN(sStringTemp) );
				nPrevLen := nPrevLen+LEN(sStringTemp);

				sStringTemp := CONCAT( 'Content-Disposition: attachment;' ,'$R$N');
				sStringTemp := CONCAT( sStringTemp, '	filename="');
				sStringTemp := CONCAT( sStringTemp, CONCAT( FileToAttach ,'"$R$N$R$N'));
				fMemCpy( ADR(ayDataTemp)+UINT_TO_DWORD(nPrevLen), ADR(sStringTemp), LEN(sStringTemp) );

				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp)+nPrevLen;

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					nProcessState := 210;
				END_IF;

		(************************************* READ AND SEND FILE PROCESS **************************************)
		210:	(* Opens File to Read the Content *)
				fbSMTPOpenFile(
							bEnable:= TRUE,
							stFileName:= FileToAttach,
							stMode:= 'r'
							);

				(* Gets the full sizes of the file *)
				fbSMTPGetSizeFile(
								bEnable:= TRUE ,
								stFName:= FileToAttach
								);

				IF ( fbSMTPOpenFile.bEnable AND fbSMTPOpenFile.bDone AND fbSMTPGetSizeFile.bDone ) THEN
					fbSMTPGetSizeFile( bEnable:= FALSE );
					dwFileSize := 0;
					nProcessState := 220;
				END_IF;


		220: 	(* Reads file in parts of 1k *)
				fbSMTPReadFile(
								bEnable:= TRUE ,
								hFile:= fbSMTPOpenFile.hFile,
								pBuffer:= ADR(ayBase64BufferIn),
								dwSize:= 1026
								);

				IF ( fbSMTPReadFile.bEnable AND fbSMTPReadFile.bDone ) THEN
					IF ( fbSMTPReadFile.dwRead > 0 ) THEN
							nProcessState := 230;	(* continues sending file *)
					ELSE
							nProcessState := 330;	(* Closes connection *)
					END_IF;
				END_IF;


		230: 	(* Converts read values in base 64 *)
				fbConvertToBase64(
								Enable:= TRUE,
								InputBuffer:= ADR(ayBase64BufferIn),
								Size:= (fbSMTPReadFile.dwRead),
								OutputBuffer:=ADR(ayBase64BufferOut)
								);

				IF ( fbConvertToBase64.Done ) THEN
							nStep := 0;
							nProcessState := 240;
							fbConvertToBase64(Enable := FALSE);
				END_IF


		240: 	(* Generates a lines of 76 chars + CR \ LF - 13 10 *)
				nPack76 := DWORD_TO_UINT(fbConvertToBase64.dwRead) / 76;
				nRestPack76 := DWORD_TO_UINT(fbConvertToBase64.dwRead) - ( nPack76 * 76 );

				WHILE ( nStep < nPack76 ) DO
						fMemCpy(ADR(ayDataTemp)+UINT_TO_DWORD(nStep*78), ADR(ayBase64BufferOut)+UINT_TO_DWORD(nStep*76) , 76);
						ayDataTemp[(nStep*78)+76] := 13;
						ayDataTemp[(nStep*78)+77] := 10;
						nStep := nStep + 1;
				END_WHILE

				IF ( nRestPack76 <> 0 ) THEN
					fMemCpy(ADR(ayDataTemp)+UINT_TO_DWORD(nStep*78), ADR(ayBase64BufferOut)+UINT_TO_DWORD(nStep*76), nRestPack76);
				END_IF

				IF ( nStep >= nPack76 ) THEN
					nProcessState := 250;
				END_IF


		250: 	(* Sends read data through the Second Connection *)
				IF ( fbTCP_InitComm.Enable AND fbTCP_InitComm.Done ) THEN
						bSendTCP := TRUE;
						nTxLen := ((nPack76*78)+nRestPack76);
						nProcessState := 260;
				END_IF


		260:	(* Closes File transfer  *)
				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					fbSMTPReadFile(
									bEnable:= FALSE ,
									hFile:= fbSMTPOpenFile.hFile
									);
					dwFileSize :=  dwFileSize + fbSMTPReadFile.dwRead;
					Completed := DWORD_TO_INT(( dwFileSize * 100 ) / fbSMTPGetSizeFile.dwSize);

					IF ( fbSMTPReadFile.dwRead < 1026 ) THEN
						nProcessState := 270;	(* Closes connection *)
					ELSE
						nProcessState := 220;	(* Continues sending file *)
					END_IF;
				END_IF;


		270:	(* Closes Data ports - File transfer Completed*)
				fbSMTPOpenFile(
								bEnable:= FALSE,
								stFileName:= FileToAttach
								);

				fbSMTPCloseFile(
								bEnable:= TRUE,
								hFile:= fbSMTPOpenFile.hFile
								);

				IF ( fbSMTPCloseFile.bDone  ) THEN
					fbSMTPCloseFile( bEnable:= FALSE );
					nProcessState := 280;
				END_IF;


		280:	(* Sends END BOUNDARY -- tag*)
				sStringTemp := CONCAT( '$R$N$R$N--StartBoundary--' ,'$R$N.$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done ) THEN
					bSendTCP := FALSE;
					nProcessState := 290;
				END_IF


	(*******************************************************************************************************)

		290:	(* Waits acknowledge of DATA command *)
				bReceiveTCP := TRUE;

				IF ( fbTCP_Receive.Enable AND fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '250' ) THEN
						bReceiveTCP := FALSE;
						nProcessState := 300;
					END_IF;
				END_IF;


		300:	(* Sends QUIT Command  *)
				sStringTemp := CONCAT( 'QUIT' ,'$R$N');
				fMemCpy( ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp) );
				bSendTCP := TRUE;
				nTxLen := LEN(sStringTemp);

				IF ( fbTCP_Send.Execute AND fbTCP_Send.Done )THEN
					bSendTCP := FALSE;
					bReceiveTCP := TRUE;
					nProcessState := 310;
				END_IF;


		310:	(* Waits acknowledge of QUIT  command *)
				IF ( fbTCP_Receive.Enable AND  fbTCP_Receive.Done ) THEN
					fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
					sStringTemp := MID( sStringTemp, 3, 1);

					IF ( sStringTemp = '221' ) THEN
						Busy := FALSE;
						Done := TRUE;
						LoggedIn:= FALSE;
						Connected := FALSE;
						bReceiveTCP := FALSE;
						nProcessState := 320;
					END_IF;
				END_IF;


		320: 	(* Closes Connections - Reset All *)
				bSendTCP := FALSE;
				bReceiveTCP := FALSE;
				bStartFlag := FALSE;
				bCommON := FALSE;

END_CASE;


 (*************************************************************************************************************************)
 (************************************************** Error Provider Module *********************************************)
 (*************************************************************************************************************************)
	IF ( fbHeartbeatTimer.Q ) THEN
		Error := TRUE;
		ErrorID := SMTP_TIMEOUT;
	ELSIF ( fbTCP_InitComm.Error ) THEN
		Error := TRUE;
		ErrorID := fbTCP_InitComm.ErrorID;
	ELSIF ( fbTCP_Receive.Error ) THEN
		Error := TRUE;
		ErrorID := fbTCP_Receive.ErrorID;
	ELSIF ( fbTCP_Send.Error ) THEN
		Error := TRUE;
		ErrorID :=  fbTCP_Send.ErrorID;
	ELSIF ( fbSMTPOpenFile.bError ) THEN
		Error := TRUE;
		ErrorID := fbSMTPOpenFile.wErrorId;
	ELSIF ( fbSMTPReadFile.bError ) THEN
		Error := TRUE;
		ErrorID := fbSMTPReadFile.wErrorId;
	ELSIF ( fbSMTPGetSizeFile.bError ) THEN
		Error := TRUE;
		ErrorID := fbSMTPGetSizeFile.wErrorId;
	ELSIF ( fbSMTPCloseFile.bError ) THEN
		Error := TRUE;
		ErrorID := fbSMTPCloseFile.wErrorId;
	END_IF;

	nStrErrorID := STRING_TO_INT(MID(sStringTemp, 3, 1));

	CASE ( nStrErrorID ) OF
		421:
				Error := TRUE;
				ErrorID := SMTP_SERV_NOT_AVAILABLE;
		450:
				Error := TRUE;
				ErrorID := SMTP_REQ_MAIL_UNAVAILABLE;
		451:
				Error := TRUE;
				ErrorID := SMTP_REQ_ACT_ABORTED;
		452:
				Error := TRUE;
				ErrorID := SMTP_INSUF_SYS_STORAGE;
		500:
				Error := TRUE;
				ErrorID := SMTP_SYNTAX_ERROR;
		501:
				Error := TRUE;
				ErrorID := SMTP_SYNTAX_IN_ARGUMENT;
		502:
				Error := TRUE;
				ErrorID := SMTP_COMM_NOT_IMPLEMENTED;
		503:
				Error := TRUE;
				ErrorID := SMTP_BAD_SEQUENCE_COMMAND;
		504:
				Error := TRUE;
				ErrorID := SMTP_COMM_PARAM_NOT_IMPLEMENTED;
		530:
				Error := TRUE;
				ErrorID := SMTP_TTLS_NOT_STARTED;
		535:
				Error := TRUE;
				ErrorID := SMTP_AUTHENTICATION_UNSUCCESSFUL;
		550:
				Error := TRUE;
				ErrorID := SMTP_MAILBOX_UNAVAILABLE;
		551:
				Error := TRUE;
				ErrorID := SMTP_USER_NOT_LOCAL;
		552:
				Error := TRUE;
				ErrorID := SMTP_EXCEEDED_STORAGE_ALLOCATION;
		553:
				Error := TRUE;
				ErrorID := SMTP_MAILBOX_NAME_NOT_ALLOWED;
		554:
				Error := TRUE;
				ErrorID := SMTP_TRANSACTION_FAILED;
	END_CASE;

ELSE (***** Middle Of IF *****)
	(* Resets *)
	sStringTemp := '';
	bSendTCP := FALSE;
	bReceiveTCP := FALSE;
	bCommON := FALSE;

	IF ( fbSMTPOpenFile.bEnable ) THEN
		fbSMTPCloseFile( bEnable := TRUE);
	ELSE
		fbSMTPCloseFile( bEnable := FALSE);
	END_IF

	fbSMTPReadFile( bEnable := FALSE);
	fbSMTPOpenFile( bEnable := FALSE);
	fbSMTPGetSizeFile( bEnable := FALSE);
END_IF


(*********************************************************************************************)
(******************************* Communication Interface ********************************)
(*********************************************************************************************)
(* Main Connection *)
fbTCP_InitComm(
				Enable := bCommON ,
				ClientPort := 0 ,
				ServerIPAddr:= ServerIPAddr ,
				ServerPort:= ServerPort
				);

(* Receives TCP Telegrams *)
fbTCP_Receive(
				Enable := bReceiveTCP ,
				SocketRef :=fbTCP_InitComm.SocketRef ,
				Data:= ayDataTemp
				);

(* Sends TCP Telegrams *)
fbTCP_Send(
			Execute := bSendTCP,
			SocketRef := fbTCP_InitComm.SocketRef ,
			TxLen := nTxLen,
			Data:= ayDataTemp
			);

