:Class GhostRider

⍝ Headless RIDE client for QA and automation.
⍝ This class will connect to an APL process (or create a new one)
⍝ and synchronously communicate through the RIDE protocol in order to control it.
⍝ This means that when the GhostRider expects a response from the interpreter
⍝ it will block the APL thread until it gets it.

⍝ To create a new APL process and connect to it
⍝    R←⎕NEW GhostRider {env}
⍝ - optional {env} is a string giving a list of environment variables to set up for the interpreter
⍝   e.g. 'MAXWS=1G WSPATH=.'
⍝   defaults to ''

⍝ To connect to an existing process
⍝    R←⎕NEW GhostRider (port {host})
⍝ - port is the positive integer port number to connect to.
⍝ - optional {host} is a string giving the ip address to connect to
⍝   {host} defaults to '127.0.0.1' which is the local machine

⍝ Public API :
⍝
⍝ (output wins errors)←Execute expr  ⍝ execute an APL expression
⍝                               ⍝ output is the string of session output
⍝                               ⍝ wins is list of opened windows
⍝                               ⍝ errors is the list of ⎕SIGNAL-ed errors (there should be one at most)
⍝
⍝ wins←EditWindows              ⍝ List open edit windows
⍝ wins←TracerWindows            ⍝ List open tracer windows
⍝ CloseWindow win               ⍝ Close an editor or tracer window
⍝ CloseWindows                  ⍝ Close all windows (SI stack unchanged)
⍝
⍝ win←EditOpen name             ⍝ Start editing a name (may create a new window or jump to an existing one)
⍝ ok←win EditFix src {stops}    ⍝ Fix new source (and stops) in given window
⍝ name Edit src {stops}         ⍝ EditOpen + EditFix + CloseWindow
⍝ win←{types}ED names           ⍝ Cover for ⎕ED that returns the created windows
⍝ src←Reformat src              ⍝ Reformat code
⍝
⍝ The following tracer functions run code and return the same result as Execute.
⍝ Trace expr                    ⍝ Start tracing an expression
⍝ TraceRun win                  ⍝ Run current line and move to next line (step over)
⍝ TraceInto win                 ⍝ Run current line and trace into callees (step into)
⍝ TraceResume win               ⍝ Resume execution of current thread
⍝ TraceReturn win               ⍝ Run current function until return to next line of caller
⍝ Resume                        ⍝ Resume all threads
⍝
⍝ The following functions change the state of the tracer without executing code (and don't return anything)
⍝ TraceCutback win              ⍝ Cut back to caller
⍝ TraceNext win                 ⍝ Jump to next line
⍝ TracePrev win                 ⍝ Jump to previous line
⍝ win TraceJump line            ⍝ Jump to specified line
⍝ win SetStops stops            ⍝ Change stop points (edit or trace window)

⍝ Notes:
⍝ - Non-standard prompts (⎕, ⍞, ∇) are not supported
⍝ - Multi-threading not supported
⍝ - Edit/trace windows are represented as namespaces with fields for window attributes
⍝   (see the WINS field of this class)




    :Field Public Shared ReadOnly Version←'1.2.0'
    ⍝ V1.2.0 - Nic 2020
    ⍝   - API to control tracer
    ⍝   - Added GetTcpPort to avoid re-using the same port number and failing the constructor
    ⍝ V1.1.0 - Nic 2020
    ⍝   - API to control editor
    ⍝ V1.0.1 - Nic 2020
    ⍝   - Using Tool.New to initialise Conga
    ⍝   - Using APLProcess to launch interpreter
    ⍝   - Unicode edition only
    ⍝ V1.0.0 - Unknown author, unknown date


    ⎕IO←⎕ML←1

    :Field Public DEBUG←1               ⍝ set to 1 to log debug information
    :Field Public TRACE←1               ⍝ set to 1 to fully trace the RIDE protocol
    :Field Public TIMEOUT←200          ⍝ timeout in milliseconds for responses that don't require significant computation - 200 is required on windows VM

    :Field Private Shared ReadOnly LF←⎕UCS 10
    :Field Private Shared ReadOnly CR←⎕UCS 13
    :Field Public NL←CR                 ⍝ newline character for output : (CR) is APL-friendly, (LF) is system-friendly

    :Field Private Shared ReadOnly ERRNO←309        ⍝ error number signaled by this class
    :Field Private Shared ReadOnly CONGA_ERRNO←999  ⍝ error number signaled by Conga
    :Field Private BUFFER←0⍴⊂''                 ⍝ list of received chunks
    :Field Private PROCESS←⎕NULL                ⍝ APLProcess to launch RIDE (if required)
    :field Private CLIENT←⎕NULL                 ⍝ Conga connection

    :Field Private Shared DRC←⎕NULL              ⍝ Conga namespace - loaded from conga workspace
    :Field Private Shared APLProcess←⎕NULL       ⍝ APLProcess namespace - loaded from APLProcess.dyalog

    :Field Private Shared ReadOnly ERROR_OK←0 '' ''  ⍝ error←(EN EM Message)
    :Field Private Shared ReadOnly ERROR_STOP←1001 '' ''  ⍝ error returned when hitting a breakpoint
    :Field Private Shared ReadOnly NO_ERROR←0⍴⊂ERROR_OK       ⍝ no error produces this list of errors
    :Field Private Shared ReadOnly ERROR_BREAK←,⊂ERROR_STOP   ⍝ simple breakpoint produces this list of errors
    :Field Private Shared ReadOnly NO_WIN←0⍴⎕NULL   ⍝ empty list of windows (force prototype to ⎕NULL)

    :Field Private WINS←NO_WIN                  ⍝ list of editor/tracer windows currently opened
    ⍝ each win is a namespace with:
    ⍝ - id: integer identifying the window      ⍝ Edit:win OpenWindow:token
    ⍝ - tracer: boolean indicated whether this is a tracer window or an edit window      ⍝ OpenWindow:debugger WindowTypeChanged:tracer
    ⍝ - name: name being edited/traced
    ⍝ - text: content of the window (list of strings)
    ⍝ - line: current line number                 ⍝ OpenWindow:currentRow SetHighLightLine:line
    ⍝ - stop: list of line numbers that have break points  ⍝ stop



    Resignal←⎕SIGNAL∘{⍵/⊂⎕DMX.(('EN'EN)('EM'EM)('Message'Message))}
    Signal←⎕SIGNAL∘{(en em msg)←⍵ ⋄ ('EN' en)('EM' em)('Message'msg)}

    Error←{((⍕⎕THIS),' ',⍺,' failed: ',⍕⍵)⎕SIGNAL ERRNO}
    Log←{⎕←(⍕⎕THIS),' ',⍺,': ',,⍕⍵ ⋄ 1:_←⍵}
    LogWarn←{⍺←'' ⋄ 1:_←('Warning',(~0∊⍴⍺)/' ',⍺)Log ⍵}   ⍝ always warn
    LogInfo←{⍺←'' ⋄ DEBUG:_←('Info',(~0∊⍴⍺)/' ',⍺)Log ⍵ ⋄ 1:_←⍵}
    TrimReplyGetLog←{pre←'["ReplyGetLog",{"result":[' ⋄ post←']}]' ⋄ (pre,post)≡((≢pre)↑⍵),((-≢post)↑⍵):pre,'...',post ⋄ ⍵}  ⍝ this one is too large to trace
    LogTrace←{TRACE:_←⍵⊣('Trace ',⍺)Log TrimReplyGetLog ⍵ ⋄ 1:_←⍵}

    GetLength←{256⊥⎕UCS 4↑⍵}
    AddHeader←{((⎕UCS (4/256)⊤8+≢⍵),'RIDE'),⍵}
    ToUtf8←{⎕UCS 'UTF-8'⎕UCS ⍵}
    FromUtf8←{'UTF-8'⎕UCS ⎕UCS ⍵}
    Stringify←{'''',((1+⍵='''')/⍵),''''}

    IsStops←{(1≡≢⍴⍵)∧(1≡≡⍵)∧(⍬≡0⍴⍵)}
    IsSource←{(1≡≢⍴⍵)∧(2≡≡⍵)∧(∧/''∘≡¨0⍴¨⍵)}
    IsWin←{⍵∊WINS}
    IsString←{(1≡≢⍴⍵)∧(1≡≡⍵)∧(''≡0⍴⍵)}

    ∇ ok←LoadLibraries;Tool
    ⍝ Failure to load library will cause ⎕SE.SALT.Load to error
      :Access Shared
      :If ⎕NULL∊DRC APLProcess
          Tool←⎕SE.SALT.Load'Tool'
          :If DRC≡⎕NULL
              DRC←Tool.New'Conga'
              {}DRC.SetProp'' 'EventMode' 1
          :EndIf
          :If APLProcess≡⎕NULL
              APLProcess←⎕SE.SALT.Load'APLProcess'
          :EndIf
      :EndIf
    ∇

    ∇ port←GetTcpPort;addr;rc;srv
    ⍝ find a free TCP port by starting and closing a conga server (pretty heavy weight...)
      :Access Public
      (rc srv)←DRC.Srv'' '127.0.0.1' 0 'Text'
      :If rc≠0 ⋄ 'GetTcpPort'Error'Failed to start server' ⋄ :EndIf
      (rc addr)←DRC.GetProp srv'LocalAddr'
      :If rc≠0 ⋄ 'GetTcpPort'Error'Failed to get local TCP/IP address' ⋄ :EndIf
      port←4⊃addr
      CloseConga srv
    ∇

    ∇ Constructor0
      :Access Public
      :Implements Constructor
      Constructor ⍬
    ∇

    ∇ Constructor args;RIDE_INIT;_;env;host;port;r;runtime;tm1;tm2
      :Access Public
      :Implements Constructor
      LoadLibraries
      :If (0∊⍴args)∨(''≡0⍴args)  ⍝ spawn a local Ride - args is {env}
          host←'127.0.0.1' ⋄ port←GetTcpPort
          env←,⍕args ⋄ RIDE_INIT←'serve::',⍕port ⍝ only accept local connections
          runtime←0  ⍝ otherwise we'd need to keep the interpreter busy
          :If 0∊⍴('\sDYAPP='⎕S 0)env  ⍝ don't inherit some environment variables
              env,←' DYAPP='
          :EndIf
          :If 0∊⍴('\sSESSION_FILE='⎕S 0)env  ⍝ don't inherit some environment variables
              env,←' SESSION_FILE='
          :EndIf
          PROCESS←⎕NEW APLProcess(''env runtime RIDE_INIT)
          ⎕DL 0.3  ⍝ ensure process doesn't exit early
          :If PROCESS.HasExited
              'Constructor'Error'Failed to start APLProcess: RIDE_INIT=',RIDE_INIT,' env=',env
          :EndIf
      :Else  ⍝ connect to an existing Ride - args is (port {host})
          (port host)←2↑args,⊂''  ⍝ port is integer and must be specified
          :If ⍬≢⍴port ⋄ :OrIf ⍬≢0⍴port ⋄ :OrIf port≠⌊port ⋄ :OrIf 0≠11○port ⋄ :OrIf port≤0
              'Constructor'Error'Port number must be positive integer: ',⍕port
          :EndIf
          :If 0∊⍴host ⋄ host←'127.0.0.1' ⋄ :EndIf  ⍝ default to local machine
          args←''
          PROCESS←⎕NULL  ⍝ no process started
      :EndIf
      tm1←'SupportedProtocols=2' ⋄ tm2←'UsingProtocol=2'
      ⎕DF('@',host,':',⍕port){(¯1↓⍵),⍺,(¯1↑⍵)}⍕⎕THIS
      :If 0≠⊃(_ CLIENT)←2↑r←DRC.Clt''host port'Text' 100000
          'Constructor'Error'Could not connect to server ',host,':',⍕port
      :ElseIf (1⍴⊂tm1)≡Read 0  ⍝ first message is not JSON
      :AndIf Send tm1
      :AndIf (1⍴⊂tm2)≡Read 0  ⍝ second message is not JSON
      :AndIf Send tm2
      :AndIf (0⍴⊂'')≡Read 1
      :AndIf Send'["Identify",{"identity":1}]'
      :AndIf ('Identify' 'UpdateDisplayName')≡⊃¨Read 1
      :AndIf Send'["Connect",{"remoteId":2}]'
      :AndIf {(1=+/⍵≡¨⊂'SetPromptType')∧(0∊⍴⍵~'ReplyGetLog' 'SetPromptType')}⊃¨Read 1  ⍝ interpreter sends 0 or more 'ReplyGetLog'  and one 'SetPromptType'
      :AndIf Send'["GetWindowLayout",{}]'  ⍝ what's this for ????
      :AndIf (0⍴⊂'')≡Read 1  ⍝ no response to GetWindowLayout ????
      :AndIf Send'["CanSessionAcceptInput",{}]'
      :AndIf (,⊂'CanAcceptInput')≡⊃¨Read 1
      ⍝ TODO should arguably set ⎕PW to 32767  ⍝ Send '["SetPW",{"pw":79}]'
      :AndIf ('©°=⍓⍌⌾⍬',NL)(NO_WIN)(NO_ERROR)≡Execute'⎕UCS 1+⎕UCS''¨¯<⍒⍋⌽⍫'''  ⍝ try and execute APL
          LogInfo'Connection established'
      :Else
          Terminate
          'Constructor'Error'RIDE handshake failed'
      :EndIf
    ∇

    ∇ CloseConga obj
      :Trap CONGA_ERRNO   ⍝ )clear can un-initialise Conga, making DRC.Close ⎕SIGNAL 999 instead of returning error code 1006 - ERR_ROOT_NOT_FOUND - Please re-initialise
          {}DRC.Close obj
      :EndTrap
    ∇

    ∇ Terminate
      :Implements Destructor
      :If PROCESS≢⎕NULL  ⍝ we did spawn an interpreter
      :AndIf ~PROCESS.HasExited  ⍝ APLProcess destructor generally triggers before this one
          {}LogInfo'Shutting down spawned interpreter'
          :Trap CONGA_ERRNO  ⍝ Conga may throw error 1006 - ERR_ROOT_NOT_FOUND - Please re-initialise
              {}0 Send'["Exit",{"code":0}]'  ⍝ attempt to shut down cleanly - APLProcess will kill it anyways
          :EndTrap
      :EndIf
      :If CLIENT≢⎕NULL
          {}LogInfo'Closing connection'
          CloseConga CLIENT
      :EndIf
      CLIENT←⎕NULL
      PROCESS←⎕NULL
    ∇


    ∇ {ok}←{error}Send msg;r
    ⍝ Send a message to the RIDE
      :Access Public
      :If 0=⎕NC'error' ⋄ error←1 ⋄ :EndIf
      ok←0=⊃r←DRC.Send CLIENT(AddHeader ToUtf8'Send'LogTrace msg)
      :If error∧~ok
          'Send'Error⍕r
          Terminate
      :EndIf
    ∇

    ∇ messages←{timeout}Read json;buffer;done;len;ok;r;start
    ⍝ Read message queue from the RIDE
      :Access Public
      :If 0=⎕NC'timeout' ⋄ timeout←TIMEOUT ⋄ :EndIf
      :Repeat
          :If ok←0=⊃r←DRC.Wait CLIENT timeout
              :If r[3]∊'Block' 'BlockLast'   ⍝ we got some data
                  BUFFER,←⊂4⊃r
              :EndIf
              done←r[3]∊'BlockLast' 'Closed' 'Timeout'  ⍝ only a timeout is normal behaviour because RIDE connection is never closed in normal operation
              ok←~r[3]∊'BlockLast' 'Closed'  ⍝ interpreter closed connection (should not happen)
          :Else ⋄ done←1 ⍝ ok←0
          :EndIf
      :Until done
      messages←0⍴⊂''
      :If ok
          buffer←∊BUFFER
          :While (len←GetLength buffer)≤≢buffer
              :If ok∧←('RIDE'≡4↓8↑buffer)∧(8<len)
                  messages,←⊂0 ⎕JSON⍣json⊢'Receive'LogTrace FromUtf8 8↓len↑buffer
                  buffer←len↓buffer
              :EndIf
          :EndWhile
          :If ok ⋄ BUFFER←,⊂buffer
          :Else ⋄ BUFFER←0⍴⊂'' ⋄ 'Read'Error'Invalid buffer: ',buffer
          :EndIf
      :Else
          'Read'LogWarn'Connection failed: ',⍕r
          Terminate  ⍝ consider connection dead for good (avoid trying to read more)
      :EndIf
    ∇

    ∇ EmptyQueue msg;messages
    ⍝ empty message queue, expecting it to be empty
      messages←0 Read 1  ⍝ do not wait for new messages
      :If ~0∊⍴messages ⋄ msg LogInfo'Message queue not empty: ',⍕⊃¨messages ⋄ :EndIf
    ∇
    ∇ messages←ignored Ignore messages
      messages/⍨←~(⊃¨messages)∊⊆ignored
    ∇
    ∇ message←{ignored}WaitFor commands;messages
    ⍝ wait for a single command
      :If 0=⎕NC'ignored' ⋄ ignored←0⍴⊂'' ⋄ :EndIf
      ignored←(×⍴ignored){(⍺×⍴⍵)⍴⍵}⊆ignored  ⍝ '' becomes (0⍴⊂'')
      :Repeat ⋄ messages←ignored Ignore Read 1 ⋄ :Until ~0∊⍴messages ⍝ blocking wait for a non-ignored message
      :If 1≠≢messages
      :OrIf ~(⊂⊃⊃messages)∊(⊆commands)
          'WaitFor'Error'Could not get message ',(⍕commands),', got: ',⍕⊃¨messages
          message←''⎕NULL
      :Else
          message←⊃messages
      :EndIf
    ∇







    ∇ win←new GetWindow id;inx
      :If (~0∊⍴WINS) ⋄ :AndIf (≢WINS)≥(inx←WINS.id⍳id) ⋄ win←inx⊃WINS       ⍝ found
      :ElseIf new ⋄ win←⎕NS ⍬ ⋄ win.id←id ⋄ WINS,←win   ⍝ create new window
      :Else ⋄ win←NO_WIN                               ⍝ not found
      :EndIf
    ∇

    ∇ (done output wins errors)←fn ProcessMessages messages;arguments;command;em;en;msg;win
      done←0                ⍝ 1 if returned to the 6-space prompt
      output←''             ⍝ session output (string with newlines)
      wins←NO_WIN           ⍝ opened windows (list of namespaces from WINS)
      errors←NO_ERROR       ⍝ list of ⎕DMX.(EN EM Message)
      :For command arguments :In messages
          win←NO_WIN
          :Select command
          :CaseList 'CanAcceptInput' 'FocusThread' 'EchoInput' 'UpdateDisplayName'  ⍝ these are ignored
          :Case 'SetPromptType'  ⍝ 0 no prompt, 1 the usual 6-space APL prompt (a.k.a. Descalc or "desktop calculator"), 2 Quad(⎕) input, 3 line editor, 4 Quote-Quad(⍞) input, 5 any prompt type unforeseen here.
              :If ~arguments.type∊0 1
                  fn Error'Unsupported prompt type: ',(1+arguments.type)⊃'blocked' 'normal' '⎕' '∇' '⍞' 'unknown'
              :EndIf
              done←0≠arguments.type ⍝ 0 is not ready for next execution
          :Case 'AppendSessionOutput'
              output,←⊂{LF=⊃⌽⍵:NL@(≢⍵)⊢⍵ ⋄ ⍵}arguments.result
          :Case 'HadError'  ⍝ ⎕SIGNAL within APL execution
              ⍝ done←1   ⍝ not necessary - SetPromptType should still be called
              (en em msg)←Execute¨'⎕DMX.EN' '⎕DMX.EM' '⎕DMX.Message'  ⍝ would infinitely loop on error
              :If (NL∨.≠{⊃⌽⍵}¨1⊃¨en em msg) ⋄ :OrIf 0∨.<⊃∘⍴¨2⊃¨en em msg ⋄ :OrIf 0∨.<⊃∘⍴¨3⊃¨en em msg
                  fn Error'Failed to retrieve ⎕DMX information'
              :EndIf
              (en em msg)←¯1↓¨1⊃¨en em msg  ⍝ keep output only
              en←⊃2⊃⎕VFI en ⋄ en-←en=0  ⍝ failed ⎕VFI would produce (⎕SIGNAL 0) which is a no-op !
              errors,←⊂(en em msg)
          :Case 'InternalError'  ⍝ Error within RIDE message processing
              ⍝ done←1   ⍝ not necessary - SetPromptType should still be called
              errors,←⊂(en em msg)←arguments.(error error_text dmx)
              ⍝ arguments.message gives the failing RIDE command
          :CaseList 'OpenWindow' 'UpdateWindow'
              win←1 GetWindow arguments.token
              win.(tracer name text line stop)←arguments.(debugger name text currentRow stop)
          :Case 'GotoWindow'
              :If 0∊⍴win←0 GetWindow arguments.win
                  'GotoWindow'Error'Unknown window: ',⍕arguments.win
              :EndIf
          :Case 'WindowTypeChanged'
              :If 0∊⍴win←0 GetWindow arguments.win
                  'WindowTypeChanged'Error'Unknown window: ',⍕arguments.win
              :EndIf
              'Tracer flag'LogInfo⍕win.tracer←arguments.tracer
          :Case 'SetHighlightLine'
              :If 0∊⍴win←0 GetWindow arguments.win
                  'SetHighlightLine'Error'Unknown window: ',⍕arguments.win
              :EndIf
              win.line←arguments.line
          :Case 'SetLineAttributes'
              :If 0∊⍴win←0 GetWindow arguments.win
                  'SetLineAttributes'Error'Unknown window: ',⍕arguments.win
              :EndIf
              win.stop←arguments.stop
          :Case 'CloseWindow'
              :If 0∊⍴win←0 GetWindow arguments.win
                  'CloseWindow'Error'Unknown window: ',⍕arguments.win
              :EndIf
              RemoveWindows win
              win←NO_WIN  ⍝ this message doesn't "go" to a window
          :Case 'SysError'
              LogInfo'SysError: ',⍕arguments.(text stack)
              Terminate
              done←1
              fn Error'Interpreter system error'
          :Case 'Disconnect'
              LogInfo'Disconnected: ',arguments.message
              Terminate
              done←1
              fn Error'Interpreter unexpectedly disconnected'
          :Else
              fn LogWarn'Unexpected RIDE command: ',command
          :EndSelect
          wins,←win
      :EndFor
      output←⊃,/output
    ∇


    ∇ (output wins errors)←fn GatherResults(waitdone waitwin);done;messages
    ⍝ Run an APL expression, get session output and opened windows
    ⍝ edit windows may be opened only by running ⎕ED and )ED in the expression
    ⍝ one tracer window will be opened if (trace=1)
    ⍝ a tracer window may also be opened by the "show stack trace on error" setting
      done←0 ⋄ output←wins←errors←⍬
      :Repeat
          (done output wins errors),←⊂¨fn ProcessMessages messages←Read 1
          done←(waitdone≤⊃⌽done)∧(waitwin≤~0∊⍴⊃⌽wins)
      :Until done
      (output wins errors)←⊃¨,/¨(output wins errors)
      wins←∪wins  ⍝ window may get several messages e.g. UpdateWindow+SetHighlightLine
    ∇

    ∇ (output wins errors)←Execute expr;wins
    ⍝ Execute an APL expression.
      :Access Public
      :If ~IsString expr←,expr ⋄ 'Execute'Error'Expression must be a string' ⋄ :EndIf
      EmptyQueue'Execute'
      Send'["Execute",{"text":',(1 ⎕JSON expr,LF),',"trace":0}]'  ⍝ DOC error : trace is 0|1 not true|false
      (output wins errors)←'Execute'GatherResults 1 0
    ∇

    ∇ (output wins errors)←Trace expr;wins
    ⍝ Trace into an APL expression.
    ⍝ Caller must ensure that the timeout is larger than expression computation time.
      :Access Public
      :If ~IsString expr←,expr ⋄ 'Trace'Error'Expression must be a string' ⋄ :EndIf
      EmptyQueue'Trace'
      Send'["Execute",{"text":',(1 ⎕JSON expr,LF),',"trace":1}]'  ⍝ DOC error : trace is 0|1 not true|false
      (output wins errors)←'Trace'GatherResults 1 1
    ∇

    ∇ (output wins errors)←TraceRun win
    ⍝ Run current line and move to next (step over)
      :Access Public
      Send'["RunCurrentLine",{"win":',(1 ⎕JSON win.id),'}]'
      (output wins errors)←'TraceRun'GatherResults 1 0
      wins RemoveWindows←win  ⍝ only interested in spawning a new window
    ∇
    ∇ (output wins errors)←TraceInto win
    ⍝ Run current line and trace callees (step into)
      :Access Public
      Send'["StepInto",{"win":',(1 ⎕JSON win.id),'}]'
      (output wins errors)←'TraceInto'GatherResults 1 0
      wins RemoveWindows←win  ⍝ only interested in spawning a new window
    ∇
    ∇ (output wins errors)←TraceResume win
    ⍝ Resume execution of current thread
      :Access Public
      Send'["Continue",{"win":',(1 ⎕JSON win.id),'}]'
      (output wins errors)←'TraceResume'GatherResults 1 0
    ∇
    ∇ (output wins errors)←TraceReturn win
    ⍝ Run current function until return to next line of caller
      :Access Public
      Send'["ContinueTrace",{"win":',(1 ⎕JSON win.id),'}]'
      (output wins errors)←'TraceReturn'GatherResults 1 0
      wins RemoveWindows←win  ⍝ only interested in spawning a new window
    ∇
    ∇ (output wins errors)←Resume
    ⍝ Resume execution of all threads
      :Access Public
      Send'["RestartThreads",{}]'
      (output wins errors)←'Resume'GatherResults 1 0
    ∇



    ∇ {list}←{list}RemoveWindows wins;default
    ⍝ ensure prototype of empty list (changing WINS by default)
      :If default←0=⎕NC'list' ⋄ list←WINS ⋄ :EndIf
      list~←wins ⋄ :If 0∊⍴list ⋄ list←NO_WIN ⋄ :EndIf
      :If default ⋄ WINS←list ⋄ :EndIf
    ∇
    ∇ CloseWindow win;errors;message;messages;ok;output;wins
    ⍝ Close an edit or tracer window
      :Access Public
      ⍝ ["CloseWindow",{"win":123}] // RIDE -> Interpreter  and  Interpreter -> RIDE
      :If ~IsWin win ⋄ 'CloseWindow'Error'Argument must be a window' ⋄ :EndIf
      Send message←'["CloseWindow",{"win":',(1 ⎕JSON win.id),'}]'
      :If 0
          :If (,⊂message)≢Read 0 ⋄ 'CloseWindow'Error'Failed to close window ',⍕win.id ⋄ :EndIf
          RemoveWindows win
      :Else
          (output wins errors)←'CloseWindow'GatherResults 0 0  ⍝ do not expect significant processing time
          ok←(output≡'')∧(errors≡NO_ERROR)
          :If ok∧(0∊⍴wins)∧(~IsWin win) ⍝ actually closed the window
          :ElseIf ok∧(1=≢wins)∧(IsWin win) ⋄ :AndIf (⊃wins).tracer  ⍝ edit turned back into a tracer
          :Else
              'CloseWindow'Error'Failed to close window ',⍕win.id
          :EndIf
      :EndIf
    ∇
    ∇ CloseWindows;messages
    ⍝ Close all windows
      :Access Public
      Send'["CloseAllWindows",{}]'
      :If 0∊⍴WINS ⋄ messages←0⍴⊂''
      :Else ⋄ messages←{'["CloseWindow",{"win":',(1 ⎕JSON ⍵),'}]'}¨WINS.id
      :EndIf
      :If ({⍵[⍋⍵]}messages)≢({⍵[⍋⍵]}Read 0) ⋄ 'CloseAllWindows'Error'Failed to close all windows' ⋄ :EndIf
      WINS←NO_WIN
    ∇




    ∇ wins←{types}ED names;errors;expr;output;wins
    ⍝ Cover for ⎕ED to open one or more editor window
    ⍝ Remember that ⎕ED silently fails on invalid names
      :Access Public
      :If 0=⎕NC'types' ⋄ types←''
      :ElseIf ~IsSource names←,⊆,names ⋄ 'ED'Error'Right argument must be a string or list of strings'
      :ElseIf ~IsString types←,types ⋄ 'ED'Error'Left argument must be a string'
      :Else ⋄ types←⍕Stringify¨types
      :EndIf
      expr←types,'⎕ED',⍕Stringify¨names
      EmptyQueue'Execute'
      Send'["Execute",{"text":',(1 ⎕JSON expr,LF),',"trace":false}]'
      (output wins errors)←'ED'GatherResults 1 1  ⍝ no timeout - not expected to take a lot of computation time
      :If ~0∊⍴errors ⋄ 'ED'Error'Produced unexpected error: ',⍕errors
      :ElseIf ~0∊⍴output ⋄ 'ED'Error'Produced unexpected output: ',⍕output
      :EndIf
    ∇

    ∇ win←EditOpen name;errors;ok;output;wins
    ⍝ Edit a name, get a window.
    ⍝ To specify the type of entity to edit, use ED instead.
      :Access Public
      :If ~IsString name←,name ⋄ :OrIf ¯1=⎕NC name ⋄ 'EditOpen'Error'Right argument must be a valid APL name' ⋄ :EndIf  ⍝ otherwise EditOpen '' loops forever
      ⍝:If 0=⎕NC'type' ⋄ type←'∇' ⋄ :EndIf
      ⍝:If ~type∊'∇→∊-⍟○∘' ⋄ 'EditOpen'Error'Left argument must be one of: ∇: fn/op ⋄ →: string ⋄ ∊: vector of strings ⋄ -: character matrix ⋄ ⍟: namespace ⋄ ○: class ⋄ ∘: interface' ⋄ :EndIf
      ⍝type←1 128 16 2 256 512 1024['∇→∊-⍟○∘'⍳type]  ⍝ Constants for entityType: 1 defined function, 2 simple character array, 4 simple numeric array, 8 mixed simple array, 16 nested array, 32 ⎕OR object, 64 native file, 128 simple character vector, 256 APL namespace, 512 APL class, 1024 APL interface, 2048 APL session, 4096 external function.
      win←NO_WIN  ⍝ failed to open anything
      EmptyQueue'EditOpen'
      Send'["Edit",{"win":0,"text":',(1 ⎕JSON name),',"pos":1,"unsaved":{}}]'  ⍝ ⎕BUG doesn't work if unsaved not specified ? ⎕DOC : win must be 0 to create a new window
      (output wins errors)←'EditOpen'GatherResults 0 1  ⍝ no timeout - not expected to take a lot of computation time
      :If 0≠≢errors ⋄ 'EditOpen'Error'Produced unexpected error: ',⍕errors
      :ElseIf 0≠≢output ⋄ 'EditOpen'Error'Produced unexpected output: ',⍕output
      :ElseIf 1≠≢wins ⋄ 'EditOpen'Error'Failed to open 1 window'
      :ElseIf wins.name≢,⊂name ⋄ 'EditOpen'Error'Did not edit expected name'
      :Else ⋄ win←⊃wins
      :EndIf
    ∇

    ∇ {ok}←win EditFix src;arguments;command;messages;stops
    ⍝ Fix source in a given edit window
      :Access Public
      :If 3=|≡src←,⊆,src  ⍝ right argument may be (src stops)
          (src stops)←src
          :If ~IsStops stops ⋄ 'EditFix'Error'Stops must be numeric vector: ',⍕stops ⋄ :EndIf
          ⍝stops∩←0,⍳⍴src  ⍝ only legitimate line numbers
      :Else ⋄ stops←win.stop
      :EndIf
      :If ~IsSource src←,¨src ⋄ 'EditFix'Error'Source must be a string or a vector of strings: ',⍕src ⋄ :EndIf
      :If ~IsWin win ⋄ 'EditFix'Error'Left argument must be a window' ⋄ :EndIf
      EmptyQueue'EditFix'
      Send'["SaveChanges",{"win":',(1 ⎕JSON win.id),',"text":',(1 ⎕JSON src),',"stop":',(1 ⎕JSON stops),'}]'
      ⍝messages←Read ⍬   ⍝ ["ReplySaveChanges",{"win":123,"err":0}] // Interpreter -> RIDE
      (command arguments)←(1 1/'SetPromptType' 'FocusThread')WaitFor'ReplySaveChanges'
      :If command≢'ReplySaveChanges'
      :OrIf win.id≢arguments.win
          'EditFix'Error'Failed to fix window ',win.id,': ',⍕src
      :EndIf
      ok←0=arguments.err
      win.(text stop)←src stops
    ∇

    ∇ {ok}←name Edit src;win
    ⍝ Modify a name through the editor
      :Access Public
      win←EditOpen name
      ok←win EditFix src
      CloseWindow win
    ∇

    ∇ src←Reformat src;arguments;command;messages;ok;type;win;ns;trad;script
    ⍝ Reformat source as vectors of strings
      :Access Public
      :If ~IsSource src←,¨,⊆,src ⋄ 'EditFix'Error'Source must be a string or a vector of strings: ',⍕src ⋄ :EndIf
      ns←⎕NS ⍬ ⋄ trad←script←ok←0
      ⍝ BUG: Mantis 18310: on windows, the interpreter sends ReplyFormatCode with incorrect format if type is '∇' and source is that of a :Namespace, rather than send OptionsDialog like linux does. Similarly when trying to format a function with type '○'
      ⍝ This is why we detect the type first hand
      :Trap 0 ⋄ trad←IsString ⎕FX src ⋄ :EndTrap
      :Trap 0 ⋄ {}ns.⎕FIX src ⋄ script←1 ⋄ :EndTrap
      :For type :In script trad/'○∇'  ⍝ Either a function or a script
          ⍝win←EditOpen ⎕A[?100⍴26]  ⍝ 3E¯142 chance of existing name
          ⍝win←type ED ⎕A[?100⍴26]  ⍝ 3E¯142 chance of existing name
          win←type ED ⎕A[?50⍴26]  ⍝ use only 50 chars because of mantis 18309 - 1.8E¯71 chance of existing name
          :If 1≠≢win ⋄ 'Reformat'Error'Failed to edit a new name' ⋄ :EndIf
          win←⊃win ⋄ EmptyQueue'Reformat'
          Send'["FormatCode",{"win":',(1 ⎕JSON win.id),',"text":',(1 ⎕JSON src),'}]'
          messages←(1 1/'SetPromptType' 'FocusThread')Ignore Read 1
          :If (⊂'OptionsDialog')∊(⊃¨messages)   ⍝ OptionsDialog pops up when invalid reformatting
              arguments←2⊃((⊃¨messages)⍳(⊂'OptionsDialog'))⊃messages
              Send'["ReplyOptionsDialog",{"index":-1,"token":',(1 ⎕JSON arguments.token),'}]'   ⍝ Cancel modal dialog box
              messages,←(1 1/'SetPromptType' 'FocusThread')Ignore Read 1  ⍝ now the interpreter should send back the ReplyFormatCode
          :EndIf
          :If 1≠≢messages ⋄ :OrIf (1⊃⊃messages)≢'ReplyFormatCode' ⋄ :OrIf (2⊃⊃messages).win≢win.id ⋄ ok←0
          :Else ⋄ ok←1 ⋄ src←(2⊃⊃messages).text ⋄ :EndIf
          CloseWindow win
          :If ok ⋄ :Leave ⋄ :EndIf
      :EndFor
      :If ~ok ⋄ 'Reformat'Error'Failed to reformat: ',⍕src ⋄ :EndIf
    ∇







    ∇ TraceCutback win;errors;output;wins
    ⍝ Cut back to caller (no code execution)
      :Access Public
      Send'["Cutback",{"win":',(1 ⎕JSON win.id),'}]'
      :If ''(,win)NO_ERROR≢'TraceCutback'GatherResults 0 1
          'TraceCutback'Error'Failed'
      :EndIf
    ∇

    ∇ TraceNext win
    ⍝ Jump to next tracer line (no code execution)
      :Access Public
      Send'["TraceForward",{"win":',(1 ⎕JSON win.id),'}]'
      :If ''(,win)NO_ERROR≢'TraceNext'GatherResults 0 1
          'TraceNext'Error'Failed'
      :EndIf
    ∇
    ∇ TracePrev win
    ⍝ Jump to previous tracer line (no code execution)
      :Access Public
      Send'["TraceBackward",{"win":',(1 ⎕JSON win.id),'}]'
      :If ''(,win)NO_ERROR≢'TracePrev'GatherResults 0 1
          'TracePrev'Error'Failed'
      :EndIf
     
    ∇
    ∇ win TraceJump line;diff
    ⍝ Jump to arbitrary tracer line (no code execution)
      :Access Public
      :If 0<diff←line-win.line ⋄ TraceNext¨diff⍴win
      :ElseIf 0>diff ⋄ TracePrev¨(-diff)⍴win
      :EndIf
    ∇
    ∇ win SetStops stops
    ⍝ Change stop points of trace or edit window
      :Access Public
      :If ~IsStops stops←,stops ⋄ 'SetStops'Error'Right argument must be numeric vector: ',⍕stops ⋄ :EndIf
      :If ~IsWin win ⋄ 'SetStops'Error'Left argument must be a window' ⋄ :EndIf
      Send'["SetLineAttributes",{"win":',(1 ⎕JSON win.id),',"stop":',(1 ⎕JSON stops),'}]'
      :If ''NO_WIN NO_ERROR≢'SetStops'GatherResults 0 0 ⍝ do not expect significant processing time
          'SetStops'Error'Failed'
      :EndIf
      win.stop←stops
    ∇








    ∇ ok←_RunQA stop;BUG;_Reformat;dup;dup2;dupstops;dupwin;error;foo;foowin;goo;goowin;ok;output;src;src1;src2;tmp;win;win2;∆
      :Access Public
      ∆←stop{⍺←'' ⋄ ⍵≡1:⍵ ⋄ ⍺⍺:0⊣'QA'Error ⍺ ⋄ 0⊣'QA'LogWarn ⍺}
      ok←1
     
      ok∧←'Execute )CLEAR'∆('clear ws',NL)(NO_WIN)(NO_ERROR)≡Execute')CLEAR'
      ok∧←'Execute meaning of life'∆('42',NL)(NO_WIN)(NO_ERROR)≡Execute'⍎⊖⍕⊃⊂|⌊-*+○⌈×÷!⌽⍉⌹~⍴⍋⍒,⍟?⍳0'
      output←∊'DOMAIN ERROR: Divide by zero' '      ÷0' '      ∧',¨NL
      error←11 'DOMAIN ERROR' 'Divide by zero'
      ok∧←'Execute ÷0'∆(output)(NO_WIN)(,⊂error)≡Execute'÷0'
     
      src←'    res  ←   format   arg   ' ':if arg' '⎕←      ''yes''' ':endif'
      ok∧←'Reformat function'∆({(⎕NS ⍬).(⎕NR ⎕FX ⍵)}src)≡(Reformat src)
      src1←'    :Namespace   ' '∇   tradfn   ' '⎕ ← 1 2 3   ' '∇' 'VAR  ←   4 5  6  ' 'dfn  ←   {  ⍺ +  ⍵   }   ' '      :EndNamespace    '
      src2←':Namespace' '    ∇ tradfn' '      ⎕←1 2 3' '    ∇' '    VAR  ←   4 5  6' '    dfn  ←   {  ⍺ +  ⍵   }' ':EndNamespace'
      ok∧←'Reformat namespace'∆ src2≡Reformat src1
     
      dup←' res←dup arg' ' ⎕←''this is dup''' ' res←arg arg'
      foo←' res←foo arg' ' ⎕←''this is foo''' ' res←dup arg'
      goo←' res←goo arg' ' ⎕←''this is goo''' ' res←foo arg'
      win←EditOpen'dup'
      ok∧←'EditOpen on same window'∆ win≡EditOpen'dup'
      ok∧←'ED on same window'∆(,⊂win)≡ED'dup'
      ok∧←'EditFix from scratch'∆ win EditFix dup
      ok∧←'EditFix from scratch effect'∆(,(↑dup),NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕CR''dup'' '
      CloseWindow win
     
      win←EditOpen'dup'
      ok∧←'EditOpen source'∆ dup≡win.text
      ok∧←'EditFix changing name'∆ win EditFix foo
      ok∧←'EditFix changing name effect'∆(,(↑foo),NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕CR''foo'' '
      ok∧←'EditOpen on previous name'∆ win≡dupwin←EditOpen'dup'  ⍝ mantis 18291 - opens the window where foo was fixed because the interpreter thinks it's still dup
      ok∧←'EditOpen on fixed name'∆ win≢foowin←EditOpen'foo'  ⍝ mantis 18291 - opens a new window rather than go to window where foo was fixed
      CloseWindows  ⍝ close two windows - would error on failure
      CloseWindows  ⍝ close zero window - would error on failure
     
      ok∧←'Trace foo'∆('')(WINS)(NO_ERROR)≡Trace'foo ''argument'' '
      ok∧←'TraceResume foo'∆('this is foo',NL,'this is dup',NL,' argument  argument ',NL)(NO_WIN)(NO_ERROR)≡TraceResume⊃WINS  ⍝
      ok∧←'TraceResume effect'∆ WINS≡NO_WIN
     
      ok∧←'Execute ⎕FX goo'∆('goo',NL)(NO_WIN)(NO_ERROR)≡Execute'+⎕FX ',⍕Stringify¨goo   ⍝ goo → foo → dup
      ok∧←'Execute ⎕STOP goo'∆('1',NL)(NO_WIN)(NO_ERROR)≡Execute'+1 ⎕STOP ''goo'''       ⍝ set breakpoint
      ok∧←'Execute goo'∆(NL,'goo[1]',NL)(WINS)(ERROR_BREAK)≡Execute'goo ''hello'''  ⍝ pop up tracer on breakpoint
      win←⊃WINS
      ok∧←'Tracing goo[1]'∆('goo' 1 1 goo(,1))(,win)≡win.(name line tracer text stop)WINS
      ok∧←'Editing while tracing goo[1]'∆ win≡EditOpen'goo'
      ok∧←'Editing goo[1]'∆('goo' 1 0 goo(,1))(,win)≡win.(name line tracer text stop)WINS
      CloseWindow win
      ok∧←'Closing goo[1] editor back into tracer'∆('goo' 1 1 goo(,1))(,win)≡win.(name line tracer text stop)WINS
     
      ok∧←'TraceInto goo[1]'∆('this is goo',NL)(NO_WIN)(NO_ERROR)≡TraceInto win  ⍝ goo[1]
      ok∧←'Tracing goo[2]'∆('goo' 2 1 goo(,1))(,win)≡win.(name line tracer text stop)WINS
      ok∧←'TraceInto goo[2]'∆('')(NO_WIN)(NO_ERROR)≡TraceInto win  ⍝ goo[2] → foo[1]
      ok∧←'Tracing foo[1]'∆('foo' 1 1 foo ⍬)(,win)≡win.(name line tracer text stop)WINS
      TraceCutback win ⍝ → goo[2]
      ok∧←'Tracing goo[2]'∆('goo' 2 1 goo(,1))(,win)≡win.(name line tracer text stop)WINS
     
      win2←EditOpen'dup'
      ok∧←'Tracing goo[2] and editing dup'∆('goo' 2 1 goo(,1))('dup' 0 0 dup ⍬)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'Fixing dup + stops'∆ win2 EditFix(dup2←' dup' ' dup1' ' dup2')(0 1 2)
      ok∧←'Tracing goo[2] and fixed dup + stops'∆('goo' 2 1 goo(,1))('dup' 0 0 dup2(0 1 2))(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'Fixing dup source effect'∆(,(↑dup2),NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕CR''dup'' '
      ok∧←'Fixing dup stops effect'∆('0 1 2',NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕STOP''dup'' '
      ok∧←'Unfixing dup + stops'∆ win2 EditFix dup ⍬
      ok∧←'Tracing goo[2] and unfixed dup + stops'∆('goo' 2 1 goo(,1))('dup' 0 0 dup ⍬)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'Unfixing dup source effect'∆(,(↑dup),NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕CR''dup'' '
      ok∧←'Unfixing dup stops effect'∆(,NL)(NO_WIN)(NO_ERROR)≡Execute' ⎕STOP''dup'' '
      ok∧←'Execute ⎕STOP dup'∆('0 1 2',NL)(NO_WIN)(NO_ERROR)≡Execute' +0 1 2 ⎕STOP ''dup'' '
      dupstops←⍬ ⍝ Mantis 18308 : ⎕STOP does not update opened windows - dupstops should be (0 1 2)
      ok∧←'Tracing goo[2] and unfixed dup + stops'∆('goo' 2 1 goo(,1))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
     
      win SetStops 2
      ok∧←'Tracing goo[2] and changed stops'∆('goo' 2 1 goo(,2))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      TracePrev win ⋄ TracePrev win ⋄ TraceNext win
      ok∧←'Tracing back to goo[1]'∆('goo' 1 1 goo(,2))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'TraceRetrun goo[1]'∆('this is goo',NL NL,'goo[2]',NL)(NO_WIN)(ERROR_BREAK)≡TraceReturn win   ⍝ hitting stop point on goo[2]
      ok∧←'Stopping at goo[2]'∆('goo' 2 1 goo(,2))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
     
      ok∧←'Execute ⎕STOP foo'∆('1',NL)(NO_WIN)(NO_ERROR)≡Execute' +1 ⎕STOP ''foo'' '
      ok∧←'TraceRun goo[2]'∆(NL,'foo[1]',NL)(NO_WIN)(ERROR_BREAK)≡TraceRun win  ⍝ hitting stop point on foo[1]
      ok∧←'Tracing foo[1]'∆('foo' 1 1 foo(,1))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'TraceRun foo[1]'∆('this is foo',NL)(NO_WIN)(NO_ERROR)≡TraceRun win  ⍝ foo[1] → foo[2]
      ok∧←'Tracing foo[2]'∆('foo' 2 1 foo(,1))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'TraceRun foo[2]'∆(NL,'dup[1]',NL)(NO_WIN)(ERROR_BREAK)≡TraceRun win
      ok∧←'Tracing dup[1]'∆('dup' 1 1 dup(0 1 2))('dup' 0 0 dup dupstops)(win win2)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
     
      ok∧←'TraceResume dup[1]'∆('this is dup',NL NL,'dup[2]',NL)(¯1↑WINS)(ERROR_BREAK)≡TraceResume win
      win←⊃⌽WINS  ⍝ resume has closed the previous window and opens a new one
      ok∧←'Tracing dup[2]'∆('dup' 2 1 dup(0 1 2))('dup' 0 0 dup dupstops)(win2 win)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
      ok∧←'TraceResume dup[2]'∆(NL,'dup[0]',NL)(¯1↑WINS)(ERROR_BREAK)≡TraceResume win
      win←⊃⌽WINS  ⍝ resume has closed the previous window and opens a new one
      ok∧←'Tracing dup[0]'∆('dup' 0 1 dup(0 1 2))('dup' 0 0 dup dupstops)(win2 win)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS
     
      ok∧←'EditFix dup'∆ win2 EditFix dup ⍬  ⍝ reset stops
      CloseWindow win2
      ok∧←'Closed dup editor'∆('dup' 0 1 dup(0 1 2))('dup' 0 0 dup ⍬)(,win)≡win.(name line tracer text stop)win2.(name line tracer text stop)WINS  ⍝ win2 is now close but we check that its fields were updated
      ok∧←'⎕STOP dup'∆(,NL)(NO_WIN)(NO_ERROR)≡Execute'+⎕STOP ''dup'' '
     
      ok∧←'TraceReturn dup[0]'∆('')(NO_WIN)(NO_ERROR)≡TraceReturn win
      ok∧←'Tracing foo[0]'∆('foo' 0 1 foo(,1))(,win)≡win.(name line tracer text stop)WINS
     
      :If 0  ⍝ BUG: RestartThreads doesn't resume execution on linux
          tmp←Resume  ⍝ resume execution of all threads - does NOT work
      :Else
          tmp←TraceResume win  ⍝ resume execution of current thread - DOES work
      :EndIf
     
      ok∧←'Resume'∆(' hello  hello ',NL)(NO_WIN)(NO_ERROR)≡tmp
      ok∧←'Closed all windows'∆ WINS≡NO_WIN
    ∇


    ∇ ok←{where}_QA stop
    ⍝ stop is boolean flag to suspend execution on QA failure
      :Access Public Shared
      :If 0=⎕NC'where' ⋄ where←⊢ ⋄ :EndIf
      ok←(where New ⍬)._RunQA stop
    ∇

    ∇ ok←_RunBug bug;errors;output;win;wins
      :Access Public
      ok←1
      (output wins errors)←Execute'+⎕FX ''foo'' ''⎕←1 2 3'' ''⎕←4 5 6'''
      ok∧←output wins errors≡('foo',NL)(NO_WIN)(NO_ERROR)
      :If ~2∊bug
          (output wins errors)←Execute'+1 ⎕STOP ''foo'' '
          ok∧←output wins errors≡('1',NL)(NO_WIN)(NO_ERROR)
      :EndIf
      ⍝ bug #2 : trace doesn't OpenWindow if there is no ⎕STOP point
      (output wins errors)←Execute'foo'
      ok∧←output wins errors≡(NL,'foo[1]',NL)(WINS)(ERROR_BREAK)
      win←⊃wins
      ⍝ bug #1 : RestartThreads doesn't work on linux
      :If 1∊bug
          (output wins errors)←Resume  ⍝ resume execution of all threads - does NOT work
      :Else
          (output wins errors)←TraceResume win  ⍝ resume execution of current thread - DOES work
      :EndIf
      :If 2∊bug  ⍝ no stop point
          ok∧←output wins errors≡('1 2 3',NL,'4 5 6',NL)(NO_WIN)(NO_ERROR)
      :Else  ⍝ stop on foo[1]
          ok∧←output wins errors≡(NL,'foo[1]',NL)(WINS)(ERROR_BREAK)
      :EndIf
    ∇

    ∇ instance←New env
    ⍝ Spawn a new interpreter, giving an environment string e.g. 'MAXWS=1G' or ''
      :Access Public Shared
      instance←#.⎕NEW ⎕THIS env
    ∇
    ∇ instance←Connect arg
    ⍝ Connect to an listening interpreter at arg≡(port {host}) e.g. Connect (4600 '127.0.0.1')
      :Access Public Shared
      'Empty arg would spawn a new interpreter - use New instead'⎕SIGNAL(0∊⍴arg)/11
      instance←#.⎕NEW ⎕THIS arg
     
    ∇

:EndClass
