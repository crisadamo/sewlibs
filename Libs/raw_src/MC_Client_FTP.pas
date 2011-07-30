(*******************************************************************************
*            Library "MPLCCommunication_Addins" for MOVI-PLC (R)
********************************************************************************
* Provides function blocks for MOVI-PLC (R)
* (c) 2009 by Cristian Adamo
********************************************************************************
*
* MC_Client_FTP
* This function block allows the MOVI-PLC (R) to send any type of files using
* the FTP protocol.
* The implementation of the protocol is based on the RFC 959.
*
******************************* HELP *******************************************
* NOTICE:
* - If authentication is required, fill in with Username and Password, else
*   leave empty.
* - In the "WorkingDirectory" field place the path name where the file will be
*   stored in the server, preceded by a slash "/" or "\", depending the server
*   configuration. If the path viewed from a tree node point of view has more
*   than one directory, separate those with slash either, else fill it with a
*   slash to use the root directory.
*
* TRANFERRING A FILE WITH DOP:
* - In the "WorkingDirectory" field, insert the Name of the directory where the
*   recipes are stored in the panel; the default directory is 'RECIPE'
* - We recommend you to use the PORT command when sending a file to the DOP
*
********************************************************************************
* STYLE: tab: 2 (as space) ; Font Size: 12 ; Column: 80
*******************************************************************************)

FUNCTION BLOCK MC_Client_FTP
VAR INPUT
  Execute: BOOL;                 (* Enable at Rising Edge *)
  PassiveMode: BOOL;             (* TRUE: PassiveMode , FALSE: ActiveMode *)
  LocalPort: UINT;               (* Port to send file - Only for PORT Command *)
  ServerIPAddr: STRING(15);      (* FTP Server IP Address *)
  ServerPort: UINT;              (* FTP port on the Server - Commonly port 21 *)
  Username: STRING(80);          (* Account username  *)
  Password: STRING(80);          (* Account password *)
  FileToSend: STRING;            (* Name of the file with extension xxas.skv *)
  WorkingDirectory: STRING(245); (* Directory name where the file will be stored *)
  TimeOut: UINT;                 (* TimeOut in [ms] *)
END VAR

VAR OUTPUT
  Done: BOOL;       (* File Sent *)
  Busy: BOOL;       (* Module is working *)
  Error: BOOL;      (* Error *)
  ErrorID: DWORD;   (* Error Code *)
  Loggedin: BOOL;   (* User Authenticated *)
  Connected: BOOL;  (* Connected to the server*)
  Completed: INT;   (* Percent [%] of the process - e.g. 25 of 100% *)
END VAR

{library private}
{flag nowatch on}
VAR
  (*** Standard ***)
  (* Initialize the communication with the FTP server *)
  fbTCP_ClientComm: MC_InitClient_TCP;
  (* Receives commands from the server *)
  fbTCP_Receive: MC_ReceiveEnable_TCP;
  (* Sends commands to the server *)
  fbTCP_Send: MC_Send_TCP;
  (* Flag to enable the TCP_ClientComm *)
  bClientON: BOOL;
  (* Flag to enable the sending commands *)
  bSendTCP: BOOL;
  (* Flag to enable the Reception of acknowledge *)
  bReceiveTCP: BOOL;
  (* Socket Reference for TCP_Send *)
  nConnRefTx: DINT;
  (* Lenght of the sending data *)
  nTxLen: UINT;
  (* Socket Reference for TCP_Receive *)
  nConnRefRx: DINT;
  (* FB to get IP address *)
  fbGetIpAddress: fbGetIpAddress;
  (* IP address of the PLC *)
  sLocalIPAddr: STRING(15);

  (*** Temp variables ***)
  (* Array to save the data read on the file *)
  ayDataTemp: ARRAY [0..1459] OF BYTE;
  (* Save strings to work with them after*)
  sStringTemp: STRING;
  (* Rising edge to execute the module *)
  fbStartTrigger: R_TRIG;
  (* Falling edge to reset *)
  fbEndTrigger: F_TRIG;
  (* Flag to start the sending process *)
  bStartFlag: BOOL;
  (* Step of the process - Used in the State Machine *)
  nProcessState: INT;
  (* Gets the first 3 characters of StringTemp to compare them against the
   * error codes *)
  nStrErrorID: INT;

  (*** File Variables ***)
  (* Opens file to read its content *)
  fbFTPOpenFile: SysFileOpenAsync;
  (* Reads the content of the file in parts of 1400 bytes to send them after *)
  fbFTPReadFile: SysFileReadAsync;
  (* Gets the size of the file to calculate the percentual of the file
   * sending process *)
  fbFTPGetSizeFile: SysFileGetSizeAsync;
  (* Closes the opened file *)
  fbFTPCloseFile: SysFileCloseAsync;
  (* Saves the amount of read bytes of the file *)
  dwFileSize: DWORD;

  (*** HeartBeat Mode ***)
  (* Previous state of the State Machine *)
  nLastProcessState: INT;
  (* Timer of the Heartbeat mode *)
  fbHeartbeatTimer: TON;

  (*** Port Variables ***)
  (* Initializes the communication with the FTP server to send the file content
   * through the configured port - PORT mode *)
  fbTCP_ServerPORTComm: MC_InitServer_TCP;
  (* Flag to enable the TCP_ServerPORTComm *)
  bServerON: BOOL;
  (* Saves the argument of the PORT command *)
  sPortArgs: STRING;
  (* Gets the position of the DOT in the "PortArgs" variable *)
  nPortDots: INT;
  (* Calculated port1 - CalcP1*256+CalcP2 = port - Port that establishes the
   * communication with the FTP Server *)
  nCalcP1: INT;
  (* Calculated port2 *)
  nCalcP2: INT;

  (*** Passive Variables ***)
  (* Initializes the communication with the FTP server through the Port asigned
   * by the server - PASV mode *)
  fbTCP_ClientPASVComm: MC_InitClient_TCP;
  (* Flag to enable the TCP_ClientPORTComm *)
  bClientPasvON: BOOL;
  (* Start of Address received*)
  nSOA: INT;
  (* End of Address received *)
  nEOA: INT;
  (* Index of the comma in the String to delete it then *)
  nComma: INT;
  (* Decodes the PASV command arguments from the FTP server *)
  sBackConn: STRING;
  (* Port 1 of PASV argument *)
  sBackPort1: STRING;
  (* Port 2 of PASV argument *)
  sBackPort2: STRING;
  (* Port of the FTP Server to connect;
   * DestinationPort = BackPort1*256+BackPort2 *)
  nDestinationPort: INT;
END VAR
{flag off}


(*******************************************************************************
 * Programmed: 05.03.2009 - by Cristian Adamo <cristian.adamo@gmail.com>
 * Current Version: 0.8
 * Change log:
 *    Ver         Date          Author            Description:
 *    v0.1        05.03.2009    Cristian Adamo    Generation of Program
 *    v0.2        10.03.2009    Cristian Adamo    Implementation of File read
 *    v0.3        11.03.2009    Crisitan Adamo    Add some lines
 *    v0.4        07.04.2009    Cristian Adamo    Some fixes
 *    v0.5        08.04.2009    Cristian Adamo    Add Completed Output
 *    v0.6        23.04.2009    Cristian Adamo    Optimization and more Fixes
 *    v0.7        13.08.2009    Cristian Adamo    Implementation of Passive Mode
 *    v0.8        21.08.2009    Cristian Adamo    Correct the reset mode
 *    v0.8 rev 1  31.08.2009    Cristian Adamo    Corrections
*******************************************************************************)

(******************************************************************************)
(*** Framework - Edges for Execute ********************************************)
(******************************************************************************)
fbStartTrigger(CLK := Execute);
fbEndTrigger(CLK := Execute);

(*** Rising Edge Execute ***)
IF (fbStartTrigger.Q) THEN
  nProcessState := 0;
  nLastProcessState := -1;
  bStartFlag := TRUE;
END IF;

(*** Falling Edge Execute ***)
IF (fbEndTrigger.Q) THEN
  Done := FALSE;
  Busy := FALSE;
  Error := FALSE;
  ErrorID := 0;
  Completed := 0;
  Loggedin := FALSE;
  Connected := FALSE;
  nStrErrorID := 0;
  nProcessState := 210;
END IF

(*** Check active errors ***)
IF (Error) THEN
  nProcessState := 210;
  Done := FALSE;
  Busy := FALSE;
  Loggedin := FALSE;
  Connected := FALSE;
END IF;

(******************************************************************************)
(*** Framework - State Machine ************************************************)
(******************************************************************************)
IF (bStartFlag) THEN

  (*** Hearbeat Mode ***)
  IF (nLastProcessState <> nProcessState) THEN
    fbHeartbeatTimer(IN := FALSE, PT := UINT_TO_TIME(TimeOut));
  ELSE
    fbHeartbeatTimer(IN := TRUE, PT := UINT_TO_TIME(TimeOut));
  END IF;
  nLastProcessState := nProcessState;

  (****************************************************************************)
  (*** State Machine **********************************************************)
  (****************************************************************************)
  CASE nProcessState OF
   (**
    * Connect with the server and gets the IP address of the MoviPLC
    *)
    0:
      IF (FileToSend = '') THEN
        Error := TRUE;
        ErrorID := FTP_FILE_MISSING;
      ELSE
        fbGetIpAddress(Enable := TRUE); (* Get actual IP of the MoviPLC *)

        IF (fbGetIpAddress.Done AND (fbGetIpAddress.IPaddress <> '')) THEN
          bClientON := TRUE;
          sLocalIPAddr := fbGetIpAddress.IPaddress;
        END IF;

        IF (fbTCP_ClientComm.Done) THEN
          nProcessState := 5;
          fbGetIpAddress(Enable := FALSE);
        END IF;
      END IF


   (**
    * Waits the acknowledge from the Server
    *)
    5:
      bReceiveTCP := TRUE;
      nConnRefRx := fbTCP_ClientComm.SocketRef;

      IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
        fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
        sStringTemp := MID(sStringTemp, 3, 1);

        IF (sStringTemp = '220') THEN
          bReceiveTCP := FALSE;
          Connected := TRUE;
          Busy := TRUE;
          nProcessState := 10;
        END IF;
      END IF;


   (**
    * Sends Username - USER Command
    *)
    10:
      sStringTemp := CONCAT(CONCAT('USER ', Username), '$R$N');
      fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

      bSendTCP := TRUE;
      nConnRefTx := fbTCP_ClientComm.SocketRef;
      nTxLen := LEN(sStringTemp);

      IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
        bSendTCP := FALSE;
        nProcessState := 20;
      END IF;

   (**
    * Waits the acknowledge of the USER command
    *)
    20:
      bReceiveTCP := TRUE;
      nConnRefRx := fbTCP_ClientComm.SocketRef;

      IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
        fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
        sStringTemp := MID(sStringTemp, 3, 1);

				IF (sStringTemp = '331') THEN
					bReceiveTCP := FALSE;
					nProcessState := 30;
				END IF;
			END IF;


	 (**
    * Sends Password - PASS Command
    *)
		30:
			sStringTemp := CONCAT(CONCAT('PASS ', Password), '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(sStringTemp);

      IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;
				nProcessState := 40;
			END IF;


   (**
    * Waits the acknowledge of the PASS command
    *)
		40:
			bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

			IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
				fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
				sStringTemp := MID(sStringTemp, 3, 1);

        IF (sStringTemp = '230') THEN
					bReceiveTCP := FALSE;
					Loggedin := TRUE;
					nProcessState := 50;
				END IF;
			END IF;


   (**
    * Sends the Working Directory - CWD Command
    *)
		50:
			sStringTemp := CONCAT(CONCAT('CWD ', WorkingDirectory), '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(sStringTemp);

			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;
				nProcessState := 60;
			END IF;


   (**
    * Waits the acknowledge of CWD command
    *)
		60:
      bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

			IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
        fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
				sStringTemp := MID(sStringTemp, 3, 1);

        IF (sStringTemp = '250') THEN
					IF (PassiveMode) THEN
					  (* PASSIVE MODE *)
						nProcessState := 70;
					ELSE
					  (* ACTIVE MODE *)
						nProcessState := 80;
					END IF;
          bReceiveTCP := FALSE;
				END IF;
			END IF;


   (**
    * Sends a request of Port to establish the second connection to
    * transfer the file - PASV command
    *)
		70:
			sStringTemp := CONCAT('PASV',  '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(sStringTemp);

			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;
				nProcessState := 100;
			END IF;


   (**
    * Start Server Communication - ACTIVE Mode
    *)
		80:
			bServerON := TRUE;
			nProcessState := 90;


   (**
    * Sends the port number where the server has to establish the
    * connection - PORT command
    *)
    90:
			(* Making a PORT Arguments with the FB Inputs *)
      nPortDots := FIND(sLocalIPAddr, '.');
			sPortArgs := REPLACE(sLocalIPAddr, ',', 1,nPortDots);
			nPortDots := FIND(sPortArgs, '.');
				sPortArgs := REPLACE(sPortArgs, ',', 1,nPortDots);
			nPortDots := FIND(sPortArgs, '.');
				sPortArgs := REPLACE(sPortArgs, ',', 1,nPortDots);
			nPortDots := FIND(sPortArgs, '.');
				sPortArgs := REPLACE(sPortArgs, ',', 1,nPortDots);
			nCalcP1 := LocalPort / 256;
			nCalcP2 := LocalPort - (nCalcP1 * 256);
				sPortArgs := CONCAT(sPortArgs, ',');
				sPortArgs := CONCAT(sPortArgs, INT_TO_STRING(nCalcP1));
				sPortArgs := CONCAT(sPortArgs, ',');
				sPortArgs := CONCAT(sPortArgs, INT_TO_STRING(nCalcP2));
			nPortDots := FIND(sPortArgs, ',');
				sPortArgs := DELETE(sPortArgs, 1, nPortDots);
			(* End of PORT Arguments *)

			sStringTemp := CONCAT(CONCAT('PORT ', sPortArgs),  '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(sStringTemp);

			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
      	bSendTCP := FALSE;
				nProcessState := 100;
			END IF;


    (**
     * Waits the acknowledge of the PASSIVE or ACTIVE MODE
     *)
		100:
			bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

      IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
				fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), fbTCP_Receive.RxLen);

				IF ((MID(sStringTemp, 3, 1) = '227') OR
				    (MID(sStringTemp, 3, 1) = '200')) THEN
					bReceiveTCP := FALSE;

				  (**
				   * If receive confirmation from PASV command, decodes the Port and
				   * IP address from the FTP server
				   *)
          IF (MID(sStringTemp, 3, 1) = '227') THEN
						nSOA := FIND(sStringTemp, '(');
						nEOA := FIND(sStringTemp, ')');
              sBackConn := MID(sStringTemp, (nEOA - nSOA) - 1, nSOA + 1);
						nComma := FIND(sBackConn, ',');
							sBackConn := DELETE(sBackConn, 1, nComma);
						nComma := FIND(sBackConn, ',');
							sBackConn :=DELETE(sBackConn, 1, nComma);
						nComma := FIND(sBackConn, ',');
							sBackConn :=DELETE(sBackConn, 1, nComma);
						nComma := FIND(sBackConn, ',');
							sBackConn :=DELETE(sBackConn, nComma, 1);
						nComma := FIND(sBackConn, ',');
							sBackPort1 := MID(sBackConn, nComma - 1, 1);
							sBackPort2 := MID(sBackConn, LEN(sBackConn) + nComma, nComma + 1);
						nDestinationPort := STRING_TO_INT(sBackPort1) * 256 +
						                    STRING_TO_INT(sBackPort2);
						bClientPasvON := TRUE;
					END IF;
						nProcessState := 110;
				END IF;
			END IF;


    (**
     * Opens the connection and Sends the file name - STOR command
     *)
		110:
			sStringTemp := CONCAT(CONCAT('STOR ', FileToSend), '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(sStringTemp);

			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;
				nProcessState := 120;
			END IF;


    (**
     * Waits the acknowledge of STOR Command and connection established
     *)
		120:
			bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

			IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
				fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
				sStringTemp := MID(sStringTemp, 3, 1);

				IF (sStringTemp = '150') THEN
					bReceiveTCP := FALSE;
					nProcessState := 130;
        END IF;
      END IF;


    (**
     * Opens File to Read the Content
     *)
		130:
			fbFTPOpenFile(
        bEnable := TRUE,
        stFileName := FileToSend,
				stMode := 'r'
			);

			(* Gets the full sizes of the file *)
			fbFTPGetSizeFile(
				bEnable := TRUE,
				stFName := FileToSend
			);

			IF (fbFTPOpenFile.bEnable AND
			    fbFTPOpenFile.bDone AND
			    fbFTPGetSizeFile.bDone) THEN
				fbFTPGetSizeFile(bEnable := FALSE);
				dwFileSize := 0;
				nProcessState := 140;
			END IF;


    (**
     * Reads file in parts of 1,4k
     *)
		140:
			fbFTPReadFile(
				bEnable := TRUE,
				hFile := fbFTPOpenFile.hFile,
				pBuffer := ADR(ayDataTemp),
				dwSize := 1400
			);

			IF (fbFTPReadFile.bEnable AND fbFTPReadFile.bDone) THEN
				IF (fbFTPReadFile.dwRead > 0) THEN
				  (* continues sending file *)
					nProcessState := 150;
				ELSE
				  (* Closes connection *)
					nProcessState := 170;
				END IF;
			END IF;


    (**
     * Sends read data through the Second Connection
     *)
		150:
			IF (PassiveMode) THEN
				(* Sends through the Passive Connection*)
				IF (fbTCP_ClientPASVComm.Enable AND fbTCP_ClientPASVComm.Done) THEN
					bSendTCP := TRUE;
					nConnRefTx := fbTCP_ClientPASVComm.SocketRef;
					nTxLen := DWORD_TO_UINT(fbFTPReadFile.dwRead);
					nProcessState := 160;
				END IF
			ELSE
				(* Sends through the Active Connection*)
				IF (fbTCP_ServerPORTComm.Enable AND fbTCP_ServerPORTComm.Done) THEN
					bSendTCP := TRUE;
					nConnRefTx := fbTCP_ServerPORTComm.SocketRef;
					nTxLen := DWORD_TO_UINT(fbFTPReadFile.dwRead);
					nProcessState := 160;
				END IF
			END IF


    (**
     * Closes File transfer
     *)
		160:
			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;

        fbFTPReadFile(
          bEnable := FALSE,
          hFile := fbFTPOpenFile.hFile
				);

        dwFileSize :=  dwFileSize + fbFTPReadFile.dwRead;
				Completed := DWORD_TO_INT((dwFileSize * 100) / fbFTPGetSizeFile.dwSize);

				IF (fbFTPReadFile.dwRead < 1400) THEN
          (* Closes connection *)
					nProcessState := 170;
				ELSE
          (* Sends next part of 1k *)
					nProcessState := 140;
				END IF;
			END IF;


    (**
     * Closes Data ports - File transfer Completed
     *)
		170:
			fbFTPCloseFile(
				bEnable := TRUE,
        hFile := fbFTPOpenFile.hFile
			);

			fbFTPOpenFile(
				bEnable := FALSE,
				stFileName := FileToSend
			);

			IF (fbFTPCloseFile.bDone) THEN
				fbFTPCloseFile(bEnable := FALSE);
				nProcessState := 180;
				bClientPasvON := FALSE;
				bServerON := FALSE;
			END IF;


    (**
     * Waits the acknowledge of file transfer completed
     *)
		180:
			bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

			IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
				fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
				sStringTemp := MID(sStringTemp, 3, 1);

				IF (sStringTemp = '226') THEN
					bReceiveTCP := FALSE;
					nProcessState := 190;
					Done := TRUE;
					Busy := FALSE;
				END IF;
			END IF;

	  (**
	   * Sends QUIT Command
	   *)
		190:
			sStringTemp := CONCAT('QUIT', '$R$N');
			fMemCpy(ADR(ayDataTemp), ADR(sStringTemp), LEN(sStringTemp));

			bSendTCP := TRUE;
			nConnRefTx := fbTCP_ClientComm.SocketRef;
			nTxLen := LEN(SstringTemp);

			IF (fbTCP_Send.Execute AND fbTCP_Send.Done) THEN
				bSendTCP := FALSE;
				nProcessState := 200;
      END IF;


    (**
     * Waits the acknowledge of QUIT command
     *)
		200:
			bReceiveTCP := TRUE;
			nConnRefRx := fbTCP_ClientComm.SocketRef;

			IF (fbTCP_Receive.Enable AND fbTCP_Receive.Done) THEN
				fMemCpy(ADR(sStringTemp), ADR(ayDataTemp), 3);
				sStringTemp := MID(sStringTemp, 3, 1);

        IF (sStringTemp = '221') THEN
					bReceiveTCP := FALSE;
					nProcessState := 210;
				END IF;
			END IF;


    (**
     * Closes Connections - Reset All
     *)
		210:
			bStartFlag := FALSE;
			bSendTCP := FALSE;
			bReceiveTCP := FALSE;
			bClientON := FALSE;
			bServerON := FALSE;
			bClientPasvON:= FALSE;
			Busy := FALSE;
			Connected := FALSE;
			Loggedin := FALSE;
  END CASE;


  (****************************************************************************)
  (*** Error Provider Module **************************************************)
  (****************************************************************************)
  IF (fbTCP_ClientComm.Error) THEN
    Error := TRUE;
    ErrorID := fbTCP_ClientComm.ErrorID;
  ELSE IF (fbTCP_Receive.Error) THEN
    Error := TRUE;
    ErrorID := fbTCP_Receive.ErrorID;
  ELSE IF (fbTCP_Send.Error) THEN
    Error := TRUE;
    ErrorID := fbTCP_Send.ErrorID;
  ELSE IF (fbTCP_ClientPASVComm.Error) THEN
    Error := TRUE;
    ErrorID := fbTCP_ClientPASVComm.ErrorID ;
  ELSE IF (fbTCP_ServerPORTComm.Error) THEN
    Error := TRUE;
    ErrorID := fbTCP_ServerPORTComm.ErrorID;
  ELSE IF (fbFTPOpenFile.bError) THEN
    Error := TRUE;
    ErrorID := fbFTPOpenFile.wErrorId;
  ELSE IF (fbFTPReadFile.bError) THEN
    Error := TRUE;
    ErrorID := fbFTPReadFile.wErrorId;
  ELSE IF (fbFTPCloseFile.bError) THEN
    Error := TRUE;
    ErrorID := fbFTPCloseFile.wErrorId;
  ELSE IF (fbHeartbeatTimer.Q) THEN
    Error := TRUE;
    ErrorID := FTP_TIMEOUT;
  ELSE IF (fbGetIPAddress.Error) THEN
    Error := TRUE;
    ErrorID := FTP_GET_IP;
  END IF;

  (* Catch the error number *)
  nStrErrorID := STRING_TO_INT(MID(sStringTemp, 3, 1));

  CASE (nStrErrorID) OF
    202:
      Error := TRUE;
      ErrorID := FTP_COMM_NOT_IMPLEMENTED_SUPERF;
    421:
      Error := TRUE;
      ErrorID := FTP_SERV_NOT_AVAILABLE;
    425:
      Error := TRUE;
      ErrorID := FTP_CAN_NOT_OPEN_CONNECTION;
    426:
      Error := TRUE;
      ErrorID := FTP_CONNECTION_CLOSED;
    450:
      Error := TRUE;
      ErorID := FTP_REQ_FILE_ACTION_NOT_TAKEN;
    451:
      Error := TRUE;
      ErrorID := FTP_REQ_ACTION_ABORTED;
    452:
      Error := TRUE;
      ErrorID := FTP_REQ_ACTION_NOT_TAKEN_STORAGE;
    500:
      Error := TRUE;
      ErrorID := FTP_SYNTAX_ERROR;
    501:
      Error := TRUE;
      ErrorID := FTP_SYNTAX_ERROR_IN_ARGUMENTS;
    502:
      Error := TRUE;
      ErrorID := FTP_COMM_NOT_IMPLEMENTED;
    504:
      Error := TRUE;
      ErrorID := FTP_COMM_NOT_IMPLEMENTED_ARGUMENT;
    530:
      Error := TRUE;
      ErrorID := FTP_NOT_LOGGED_IN;
    532:
      Error := TRUE;
      ErrorID := FTP_NEED_ACCOUNT_TO_STORING_FILE;
    550:
      Error := TRUE;
      ErrorID := FTP_REQUESTED_ACTION_NOT_TAKEN;
    551:
      Error := TRUE;
      ErrorID := FTP_ACTION_ABORTED;
    552:
      Error := TRUE;
      ErrorID := FTP_EXCEEDED_STORAGE_ALLOCATION;
    553:
      Error := TRUE;
      ErrorID := FTP_FILENAME_NOT_ALLOWED;
  END CASE;
ELSE  (*** Middle Of IF ***)
  (* Resets *)
  sStringTemp := '';
  bReceiveTCP := FALSE;
  bSendTCP := FALSE;
  bClientON := FALSE;
  bServerON := FALSE;
  bClientPasvON := FALSE;

  IF (fbFTPOpenFile.bEnable) THEN
    fbFTPCloseFile(bEnable := TRUE);
  ELSE
    fbFTPCloseFile(bEnable := FALSE);
  END IF

  fbFTPReadFile(bEnable:= FALSE);
  fbFTPOpenFile(bEnable:= FALSE);
END IF


(******************************************************************************)
(*** Communication Interface **************************************************)
(******************************************************************************)
(* Main Connection *)
fbTCP_ClientComm(
  Enable := bClientON,
  ClientPort := 0,
  ServerIPAddr := ServerIPAddr,
  ServerPort := ServerPort,
);

(* Secundary connection to send the file using the PORT Command - ACTIVE MODE *)
fbTCP_ServerPORTComm(
  Enable:= bServerON,
  ServerPort := LocalPort,
);

(* Secundary connection to send the file using the PASV Command - PASSIVE MODE*)
fbTCP_ClientPASVComm(
  Enable := bClientPasvON,
  ClientPort := 0,
  ServerIPAddr := ServerIPAddr,
  ServerPort := nDestinationPort,
);

(* Receives TCP Telegrams *)
fbTCP_Receive(
  Enable := bReceiveTCP,
  SocketRef := nConnRefRx,
  Data:= ayDataTemp,
);

(* Sends TCP Telegrams *)
fbTCP_Send(
  Execute := bSendTCP,
  SocketRef := nConnRefTx,
  TxLen := nTxLen,
  Data := ayDataTemp,
);
