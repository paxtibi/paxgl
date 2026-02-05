unit pax.OpenAL;

{$mode objfpc}{$H+}
{$packenum 4}
{$packrecords C}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$M+}

interface

uses
  Classes, SysUtils;

type
  ALboolean = uint8;
  ALchar = ansichar;
  ALbyte = int8;
  ALubyte = uint8;
  ALshort = int16;
  ALushort = uint16;
  ALint = int32;
  ALuint = uint32;
  ALsizei = int32;
  ALenum = int32;
  ALfloat = single;
  ALdouble = double;
  ALvoid = pointer;

  PALvoid = ^ALvoid;
  PALboolean = ^ALboolean;
  PALint = ^ALint;
  PALuint = ^ALuint;
  PALfloat = ^ALfloat;
  PALdouble = ^ALdouble;

  // ============================================================================
  // Costanti principali (tutte incluse dal file fornito)
  // ============================================================================

const
  AL_INVALID = -1;

  AL_NONE = 0;
  AL_FALSE = 0;
  AL_TRUE = 1;

  AL_SOURCE_RELATIVE = $202;
  AL_CONE_INNER_ANGLE = $1001;
  AL_CONE_OUTER_ANGLE = $1002;
  AL_PITCH = $1003;
  AL_POSITION = $1004;
  AL_DIRECTION = $1005;
  AL_VELOCITY = $1006;
  AL_LOOPING = $1007;
  AL_BUFFER = $1009;
  AL_GAIN = $100A;
  AL_MIN_GAIN = $100D;
  AL_MAX_GAIN = $100E;
  AL_ORIENTATION = $100F;
  AL_CHANNEL_MASK = $3000;

  AL_SOURCE_STATE = $1010;
  AL_INITIAL = $1011;
  AL_PLAYING = $1012;
  AL_PAUSED = $1013;
  AL_STOPPED = $1014;

  AL_BUFFERS_QUEUED = $1015;
  AL_BUFFERS_PROCESSED = $1016;

  AL_SEC_OFFSET = $1024;
  AL_SAMPLE_OFFSET = $1025;
  AL_BYTE_OFFSET = $1026;

  AL_SOURCE_TYPE = $1027;
  AL_STATIC = $1028;
  AL_STREAMING = $1029;
  AL_UNDETERMINED = $1030;

  AL_FORMAT_MONO8 = $1100;
  AL_FORMAT_MONO16 = $1101;
  AL_FORMAT_STEREO8 = $1102;
  AL_FORMAT_STEREO16 = $1103;

  AL_REFERENCE_DISTANCE = $1020;
  AL_ROLLOFF_FACTOR = $1021;
  AL_CONE_OUTER_GAIN = $1022;
  AL_MAX_DISTANCE = $1023;

  AL_FREQUENCY = $2001;
  AL_BITS = $2002;
  AL_CHANNELS = $2003;
  AL_SIZE = $2004;

  AL_UNUSED = $2010;
  AL_PENDING = $2011;
  AL_PROCESSED = $2012;

  AL_NO_ERROR = 0;
  AL_INVALID_NAME = $A001;
  AL_INVALID_ENUM = $A002;
  AL_INVALID_VALUE = $A003;
  AL_INVALID_OPERATION = $A004;
  AL_OUT_OF_MEMORY = $A005;

  AL_VENDOR = $B001;
  AL_VERSION = $B002;
  AL_RENDERER = $B003;
  AL_EXTENSIONS = $B004;

  AL_DOPPLER_FACTOR = $C000;
  AL_DOPPLER_VELOCITY = $C001;
  AL_SPEED_OF_SOUND = $C003;

  AL_DISTANCE_MODEL = $D000;
  AL_INVERSE_DISTANCE = $D001;
  AL_INVERSE_DISTANCE_CLAMPED = $D002;
  AL_LINEAR_DISTANCE = $D003;
  AL_LINEAR_DISTANCE_CLAMPED = $D004;
  AL_EXPONENT_DISTANCE = $D005;
  AL_EXPONENT_DISTANCE_CLAMPED = $D006;

  // ============================================================================
  // Puntatori a funzione in PascalCase
  // ============================================================================

type
  TalEnable = procedure(Capability: ALenum); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalDisable = procedure(Capability: ALenum); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalIsEnabled = function(Capability: ALenum): ALboolean; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetString = function(Param: ALenum): pansichar; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBooleanv = procedure(Param: ALenum; Data: PALboolean); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetIntegerv = procedure(Param: ALenum; Data: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetFloatv = procedure(Param: ALenum; Data: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetDoublev = procedure(Param: ALenum; Data: PALdouble); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetBoolean = function(Param: ALenum): ALboolean; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetInteger = function(Param: ALenum): ALint; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetFloat = function(Param: ALenum): ALfloat; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetDouble = function(Param: ALenum): ALdouble; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetError = function: ALenum; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalIsExtensionPresent = function(const ExtName: pansichar): ALboolean; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetProcAddress = function(const FName: pansichar): Pointer; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetEnumValue = function(const EName: pansichar): ALenum; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  // Listener
  TalListenerf = procedure(Param: ALenum; Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalListener3f = procedure(Param: ALenum; Value1, Value2, Value3: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalListenerfv = procedure(Param: ALenum; const Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalListeneri = procedure(Param: ALenum; Value: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalListener3i = procedure(Param: ALenum; Value1, Value2, Value3: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalListeneriv = procedure(Param: ALenum; const Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetListenerf = procedure(Param: ALenum; Value: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetListener3f = procedure(Param: ALenum; Value1, Value2, Value3: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetListenerfv = procedure(Param: ALenum; Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetListeneri = procedure(Param: ALenum; Value: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetListener3i = procedure(Param: ALenum; Value1, Value2, Value3: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetListeneriv = procedure(Param: ALenum; Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  // Source
  TalGenSources = procedure(N: ALsizei; Sources: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalDeleteSources = procedure(N: ALsizei; const Sources: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalIsSource = function(Sid: ALuint): ALboolean; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalSourcef = procedure(Sid: ALuint; Param: ALenum; Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSource3f = procedure(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourcefv = procedure(Sid: ALuint; Param: ALenum; const Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourcei = procedure(Sid: ALuint; Param: ALenum; Value: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSource3i = procedure(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceiv = procedure(Sid: ALuint; Param: ALenum; const Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetSourcef = procedure(Sid: ALuint; Param: ALenum; Value: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetSource3f = procedure(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetSourcefv = procedure(Sid: ALuint; Param: ALenum; Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetSourcei = procedure(Sid: ALuint; Param: ALenum; Value: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetSource3i = procedure(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetSourceiv = procedure(Sid: ALuint; Param: ALenum; Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalSourcePlayv = procedure(Ns: ALsizei; const Sids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceStopv = procedure(Ns: ALsizei; const Sids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceRewindv = procedure(Ns: ALsizei; const Sids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourcePausev = procedure(Ns: ALsizei; const Sids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalSourcePlay = procedure(Sid: ALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceStop = procedure(Sid: ALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceRewind = procedure(Sid: ALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourcePause = procedure(Sid: ALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalSourceQueueBuffers = procedure(Sid: ALuint; NumEntries: ALsizei; const Bids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSourceUnqueueBuffers = procedure(Sid: ALuint; NumEntries: ALsizei; Bids: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  // Buffer
  TalGenBuffers = procedure(N: ALsizei; Buffers: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalDeleteBuffers = procedure(N: ALsizei; const Buffers: PALuint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalIsBuffer = function(Bid: ALuint): ALboolean; {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalBufferData = procedure(Bid: ALuint; Format: ALenum; const Data: Pointer; Size: ALsizei; Freq: ALsizei); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalBufferf = procedure(Bid: ALuint; Param: ALenum; Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalBuffer3f = procedure(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalBufferfv = procedure(Bid: ALuint; Param: ALenum; const Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalBufferi = procedure(Bid: ALuint; Param: ALenum; Value: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalBuffer3i = procedure(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalBufferiv = procedure(Bid: ALuint; Param: ALenum; const Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TalGetBufferf = procedure(Bid: ALuint; Param: ALenum; Value: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBuffer3f = procedure(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBufferfv = procedure(Bid: ALuint; Param: ALenum; Values: PALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBufferi = procedure(Bid: ALuint; Param: ALenum; Value: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBuffer3i = procedure(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalGetBufferiv = procedure(Bid: ALuint; Param: ALenum; Values: PALint); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  // Global
  TalDopplerFactor = procedure(Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalDopplerVelocity = procedure(Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalSpeedOfSound = procedure(Value: ALfloat); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TalDistanceModel = procedure(DistanceModel: ALenum); {$IFDEF WINDOWS} stdcall {$ELSE} cdecl {$ENDIF};


const
  ALC_VERSION_1_0 = 1;
  ALC_VERSION_1_1 = 1;

type
  // Tipi base OpenAL / ALC
  ALCboolean = bytebool;           // 8-bit boolean (0=false, 1=true)
  ALCchar = ansichar;
  ALCbyte = shortint;
  ALCubyte = byte;
  ALCshort = smallint;
  ALCushort = word;
  ALCint = longint;
  ALCuint = longword;
  ALCsizei = longint;
  ALCenum = longint;
  ALCfloat = single;
  ALCdouble = double;
  ALCvoid = byte;           // usato solo come placeholder per puntatori opachi

  PALCvoid = ^ALCvoid;
  PALCint = ^ALCint;
  PALCuint = ^ALCuint;
  PALCenum = ^ALCenum;
  PALCsizei = ^ALCsizei;
  PALCfloat = ^ALCfloat;
  PALCchar = pansichar;

  ALCdevice = pointer;
  ALCcontext = pointer;

  PALCdevice = ^ALCdevice;
  PALCcontext = ^ALCcontext;

const
  // Valori booleani
  ALC_FALSE = bytebool(0);
  ALC_TRUE = bytebool(1);

  // Codici di errore
  ALC_NO_ERROR = 0;
  ALC_INVALID_DEVICE = $A001;
  ALC_INVALID_CONTEXT = $A002;
  ALC_INVALID_ENUM = $A003;
  ALC_INVALID_VALUE = $A004;
  ALC_OUT_OF_MEMORY = $A005;

  // Query generali
  ALC_MAJOR_VERSION = $1000;
  ALC_MINOR_VERSION = $1001;
  ALC_ATTRIBUTES_SIZE = $1002;
  ALC_ALL_ATTRIBUTES = $1003;
  ALC_DEFAULT_DEVICE_SPECIFIER = $1004;
  ALC_DEVICE_SPECIFIER = $1005;
  ALC_EXTENSIONS = $1006;
  ALC_FREQUENCY = $1007;
  ALC_REFRESH = $1008;
  ALC_SYNC = $1009;

  // OpenAL 1.1
  ALC_MONO_SOURCES = $1010;
  ALC_STEREO_SOURCES = $1011;

  // Enumerate all devices extension
  ALC_DEFAULT_ALL_DEVICES_SPECIFIER = $1012;
  ALC_ALL_DEVICES_SPECIFIER = $1013;

  // Capture extension
  ALC_CAPTURE_DEVICE_SPECIFIER = $310;
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER = $311;
  ALC_CAPTURE_SAMPLES = $312;

type
  TALCCreateContext = function(device: PALCdevice; attrlist: PALCint): PALCcontext;
  TALCMakeContextCurrent = function(context: PALCcontext): ALCboolean;
  TALCProcessContext = procedure(context: PALCcontext);
  TALCSuspendContext = procedure(context: PALCcontext);
  TALCDestroyContext = procedure(context: PALCcontext);
  TALCGetCurrentContext = function: PALCcontext;
  TALCGetContextsDevice = function(context: PALCcontext): PALCdevice;
  TALCOpenDevice = function(devicename: PALCchar): PALCdevice;
  TALCCloseDevice = function(device: PALCdevice): ALCboolean;
  TALCGetError = function(device: PALCdevice): ALCenum;
  TALCIsExtensionPresent = function(device: PALCdevice; extname: PALCchar): ALCboolean;
  TALCGetProcAddress = function(device: PALCdevice; funcname: PALCchar): Pointer;
  TALCGetEnumValue = function(device: PALCdevice; enumname: PALCchar): ALCenum;
  TALCGetString = function(device: PALCdevice; param: ALCenum): PALCchar;
  TALCGetIntegerv = procedure(device: PALCdevice; param: ALCenum; size: ALCsizei; values: PALCint);
  TALCCaptureOpenDevice = function(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice;
  TALCCaptureCloseDevice = function(device: PALCdevice): ALCboolean;
  TALCCaptureStart = procedure(device: PALCdevice);
  TALCCaptureStop = procedure(device: PALCdevice);
  TALCCaptureSamples = procedure(device: PALCdevice; buffer: PALCvoid; samples: ALCsizei);


  IOpenALContext = interface
    ['{53E5C35E-B111-4B2A-8F2E-18DF656C0879}']
    function alcCreateContext(var device: ALCdevice; attrlist: PALCint): PALCcontext;
    function alcMakeContextCurrent(context: PALCcontext): ALCboolean;
    procedure alcProcessContext(var context: ALCcontext);
    procedure alcSuspendContext(var context: ALCcontext);
    procedure alcDestroyContext(var context: ALCcontext);
    function alcGetCurrentContext: PALCcontext;
    function alcGetContextsDevice(var context: ALCcontext): PALCdevice;

    function alcOpenDevice(devicename: PALCchar): PALCdevice;
    function alcCloseDevice(var device: ALCdevice): ALCboolean;

    function alcGetError(var device: ALCdevice): ALCenum;

    function alcIsExtensionPresent(var device: ALCdevice; extname: PALCchar): ALCboolean;
    function alcGetProcAddress(var device: ALCdevice; funcname: PALCchar): Pointer;
    function alcGetEnumValue(var device: ALCdevice; enumname: PALCchar): ALCenum;

    function alcGetString(device: PALCdevice; param: ALCenum): PALCchar;
    procedure alcGetIntegerv(var device: ALCdevice; param: ALCenum; size: ALCsizei; values: PALCint);

    function alcCaptureOpenDevice(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice;
    function alcCaptureCloseDevice(var device: ALCdevice): ALCboolean;
    procedure alcCaptureStart(var device: ALCdevice);
    procedure alcCaptureStop(var device: ALCdevice);
    procedure alcCaptureSamples(var device: ALCdevice; buffer: PALCvoid; samples: ALCsizei);
  end;


  { IOpenAL }

  IOpenAL = interface
    ['{398BA2F2-7A72-44DA-AD10-389ED28EE469}']
    procedure alEnable(Capability: ALenum);
    procedure alDisable(Capability: ALenum);
    function alIsEnabled(Capability: ALenum): ALboolean;

    function alGetString(Param: ALenum): pansichar;
    procedure alGetBooleanv(Param: ALenum; Data: PALboolean);
    procedure alGetIntegerv(Param: ALenum; Data: PALint);
    procedure alGetFloatv(Param: ALenum; Data: PALfloat);
    procedure alGetDoublev(Param: ALenum; Data: PALdouble);

    function alGetBoolean(Param: ALenum): ALboolean;
    function alGetInteger(Param: ALenum): ALint;
    function alGetFloat(Param: ALenum): ALfloat;
    function alGetDouble(Param: ALenum): ALdouble;

    function alGetError: ALenum;

    function alIsExtensionPresent(const ExtName: pansichar): ALboolean;
    function alGetProcAddress(const FName: pansichar): Pointer;
    function alGetEnumValue(const EName: pansichar): ALenum;

    // Listener
    procedure alListenerf(Param: ALenum; Value: ALfloat);
    procedure alListener3f(Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alListenerfv(Param: ALenum; const Values: PALfloat);
    procedure alListeneri(Param: ALenum; Value: ALint);
    procedure alListener3i(Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alListeneriv(Param: ALenum; const Values: PALint);

    procedure alGetListenerf(Param: ALenum; Value: PALfloat);
    procedure alGetListener3f(Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetListenerfv(Param: ALenum; Values: PALfloat);
    procedure alGetListeneri(Param: ALenum; Value: PALint);
    procedure alGetListener3i(Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetListeneriv(Param: ALenum; Values: PALint);

    // Source
    procedure alGenSources(N: ALsizei; Sources: PALuint);
    procedure alDeleteSources(N: ALsizei; const Sources: PALuint);
    function alIsSource(Sid: ALuint): ALboolean;

    procedure alSourcef(Sid: ALuint; Param: ALenum; Value: ALfloat);
    procedure alSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alSourcefv(Sid: ALuint; Param: ALenum; const Values: PALfloat);
    procedure alSourcei(Sid: ALuint; Param: ALenum; Value: ALint);
    procedure alSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alSourceiv(Sid: ALuint; Param: ALenum; const Values: PALint);

    procedure alGetSourcef(Sid: ALuint; Param: ALenum; Value: PALfloat);
    procedure alGetSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetSourcefv(Sid: ALuint; Param: ALenum; Values: PALfloat);
    procedure alGetSourcei(Sid: ALuint; Param: ALenum; Value: PALint);
    procedure alGetSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetSourceiv(Sid: ALuint; Param: ALenum; Values: PALint);

    procedure alSourcePlayv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourceStopv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourceRewindv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourcePausev(Ns: ALsizei; const Sids: PALuint);

    procedure alSourcePlay(Sid: ALuint);
    procedure alSourceStop(Sid: ALuint);
    procedure alSourceRewind(Sid: ALuint);
    procedure alSourcePause(Sid: ALuint);

    procedure alSourceQueueBuffers(Sid: ALuint; NumEntries: ALsizei; const Bids: PALuint);
    procedure alSourceUnqueueBuffers(Sid: ALuint; NumEntries: ALsizei; Bids: PALuint);

    // Buffer
    procedure alGenBuffers(N: ALsizei; Buffers: PALuint);
    procedure alDeleteBuffers(N: ALsizei; const Buffers: PALuint);
    function alIsBuffer(Bid: ALuint): ALboolean;

    procedure alBufferData(Bid: ALuint; Format: ALenum; const Data: Pointer; Size: ALsizei; Freq: ALsizei);

    procedure alBufferf(Bid: ALuint; Param: ALenum; Value: ALfloat);
    procedure alBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alBufferfv(Bid: ALuint; Param: ALenum; const Values: PALfloat);
    procedure alBufferi(Bid: ALuint; Param: ALenum; Value: ALint);
    procedure alBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alBufferiv(Bid: ALuint; Param: ALenum; const Values: PALint);

    procedure alGetBufferf(Bid: ALuint; Param: ALenum; Value: PALfloat);
    procedure alGetBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetBufferfv(Bid: ALuint; Param: ALenum; Values: PALfloat);
    procedure alGetBufferi(Bid: ALuint; Param: ALenum; Value: PALint);
    procedure alGetBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetBufferiv(Bid: ALuint; Param: ALenum; Values: PALint);

    // Global
    procedure alDopplerFactor(Value: ALfloat);
    procedure alDopplerVelocity(Value: ALfloat);
    procedure alSpeedOfSound(Value: ALfloat);
    procedure alDistanceModel(DistanceModel: ALenum);

    function getContext: IOpenALContext;
  end;


function getOpenAL: IOpenAL;

implementation

uses
  dynlibs;

const
  {$IFDEF WINDOWS}
    DefaultLib = 'soft_oal.dll';
  {$ELSEIF defined(LINUX)}
    DefaultLib = 'libopenal.so.1';
  {$ELSEIF defined(DARWIN)}
    DefaultLib = 'libopenal.1.dylib';
  {$ELSE}
  DefaultLib = 'openal';
  {$ENDIF}

type
  { TOpenAL }

  TOpenAL = class(TInterfacedObject, IOpenAL)
  private
    FLibHandle: TLibHandle;
    FEnable: TalEnable;
    FDisable: TalDisable;
    FIsEnabled: TalIsEnabled;
    FGetString: TalGetString;
    FGetBooleanv: TalGetBooleanv;
    FGetIntegerv: TalGetIntegerv;
    FGetFloatv: TalGetFloatv;
    FGetDoublev: TalGetDoublev;
    FGetBoolean: TalGetBoolean;
    FGetInteger: TalGetInteger;
    FGetFloat: TalGetFloat;
    FGetDouble: TalGetDouble;
    FGetError: TalGetError;
    FIsExtensionPresent: TalIsExtensionPresent;
    FGetProcAddress: TalGetProcAddress;
    FGetEnumValue: TalGetEnumValue;

    FListenerf: TalListenerf;
    FListener3f: TalListener3f;
    FListenerfv: TalListenerfv;
    FListeneri: TalListeneri;
    FListener3i: TalListener3i;
    FListeneriv: TalListeneriv;
    FGetListenerf: TalGetListenerf;
    FGetListener3f: TalGetListener3f;
    FGetListenerfv: TalGetListenerfv;
    FGetListeneri: TalGetListeneri;
    FGetListener3i: TalGetListener3i;
    FGetListeneriv: TalGetListeneriv;

    FGenSources: TalGenSources;
    FDeleteSources: TalDeleteSources;
    FIsSource: TalIsSource;
    FSourcef: TalSourcef;
    FSource3f: TalSource3f;
    FSourcefv: TalSourcefv;
    FSourcei: TalSourcei;
    FSource3i: TalSource3i;
    FSourceiv: TalSourceiv;
    FGetSourcef: TalGetSourcef;
    FGetSource3f: TalGetSource3f;
    FGetSourcefv: TalGetSourcefv;
    FGetSourcei: TalGetSourcei;
    FGetSource3i: TalGetSource3i;
    FGetSourceiv: TalGetSourceiv;

    FSourcePlayv: TalSourcePlayv;
    FSourceStopv: TalSourceStopv;
    FSourceRewindv: TalSourceRewindv;
    FSourcePausev: TalSourcePausev;

    FSourcePlay: TalSourcePlay;
    FSourceStop: TalSourceStop;
    FSourceRewind: TalSourceRewind;
    FSourcePause: TalSourcePause;

    FSourceQueueBuffers: TalSourceQueueBuffers;
    FSourceUnqueueBuffers: TalSourceUnqueueBuffers;

    FGenBuffers: TalGenBuffers;
    FDeleteBuffers: TalDeleteBuffers;
    FIsBuffer: TalIsBuffer;
    FBufferData: TalBufferData;

    FBufferf: TalBufferf;
    FBuffer3f: TalBuffer3f;
    FBufferfv: TalBufferfv;
    FBufferi: TalBufferi;
    FBuffer3i: TalBuffer3i;
    FBufferiv: TalBufferiv;

    FGetBufferf: TalGetBufferf;
    FGetBuffer3f: TalGetBuffer3f;
    FGetBufferfv: TalGetBufferfv;
    FGetBufferi: TalGetBufferi;
    FGetBuffer3i: TalGetBuffer3i;
    FGetBufferiv: TalGetBufferiv;

    FDopplerFactor: TalDopplerFactor;
    FDopplerVelocity: TalDopplerVelocity;
    FSpeedOfSound: TalSpeedOfSound;
    FDistanceModel: TalDistanceModel;

    procedure LoadLibrary;
    procedure LoadFunctions; virtual;
  public
    constructor Create();
    destructor Destroy; override;

    procedure alEnable(Capability: ALenum);
    procedure alDisable(Capability: ALenum);
    function alIsEnabled(Capability: ALenum): ALboolean;

    function alGetString(Param: ALenum): pansichar;
    procedure alGetBooleanv(Param: ALenum; Data: PALboolean);
    procedure alGetIntegerv(Param: ALenum; Data: PALint);
    procedure alGetFloatv(Param: ALenum; Data: PALfloat);
    procedure alGetDoublev(Param: ALenum; Data: PALdouble);

    function alGetBoolean(Param: ALenum): ALboolean;
    function alGetInteger(Param: ALenum): ALint;
    function alGetFloat(Param: ALenum): ALfloat;
    function alGetDouble(Param: ALenum): ALdouble;

    function alGetError: ALenum;

    function alIsExtensionPresent(const ExtName: pansichar): ALboolean;
    function alGetProcAddress(const FName: pansichar): Pointer;
    function alGetEnumValue(const EName: pansichar): ALenum;

    procedure alListenerf(Param: ALenum; Value: ALfloat);
    procedure alListener3f(Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alListenerfv(Param: ALenum; const Values: PALfloat);
    procedure alListeneri(Param: ALenum; Value: ALint);
    procedure alListener3i(Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alListeneriv(Param: ALenum; const Values: PALint);

    procedure alGetListenerf(Param: ALenum; Value: PALfloat);
    procedure alGetListener3f(Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetListenerfv(Param: ALenum; Values: PALfloat);
    procedure alGetListeneri(Param: ALenum; Value: PALint);
    procedure alGetListener3i(Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetListeneriv(Param: ALenum; Values: PALint);

    procedure alGenSources(N: ALsizei; Sources: PALuint);
    procedure alDeleteSources(N: ALsizei; const Sources: PALuint);
    function alIsSource(Sid: ALuint): ALboolean;

    procedure alSourcef(Sid: ALuint; Param: ALenum; Value: ALfloat);
    procedure alSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alSourcefv(Sid: ALuint; Param: ALenum; const Values: PALfloat);
    procedure alSourcei(Sid: ALuint; Param: ALenum; Value: ALint);
    procedure alSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alSourceiv(Sid: ALuint; Param: ALenum; const Values: PALint);

    procedure alGetSourcef(Sid: ALuint; Param: ALenum; Value: PALfloat);
    procedure alGetSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetSourcefv(Sid: ALuint; Param: ALenum; Values: PALfloat);
    procedure alGetSourcei(Sid: ALuint; Param: ALenum; Value: PALint);
    procedure alGetSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetSourceiv(Sid: ALuint; Param: ALenum; Values: PALint);

    procedure alSourcePlayv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourceStopv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourceRewindv(Ns: ALsizei; const Sids: PALuint);
    procedure alSourcePausev(Ns: ALsizei; const Sids: PALuint);

    procedure alSourcePlay(Sid: ALuint);
    procedure alSourceStop(Sid: ALuint);
    procedure alSourceRewind(Sid: ALuint);
    procedure alSourcePause(Sid: ALuint);

    procedure alSourceQueueBuffers(Sid: ALuint; NumEntries: ALsizei; const Bids: PALuint);
    procedure alSourceUnqueueBuffers(Sid: ALuint; NumEntries: ALsizei; Bids: PALuint);

    procedure alGenBuffers(N: ALsizei; Buffers: PALuint);
    procedure alDeleteBuffers(N: ALsizei; const Buffers: PALuint);
    function alIsBuffer(Bid: ALuint): ALboolean;

    procedure alBufferData(Bid: ALuint; Format: ALenum; const Data: Pointer; Size: ALsizei; Freq: ALsizei);

    procedure alBufferf(Bid: ALuint; Param: ALenum; Value: ALfloat);
    procedure alBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
    procedure alBufferfv(Bid: ALuint; Param: ALenum; const Values: PALfloat);
    procedure alBufferi(Bid: ALuint; Param: ALenum; Value: ALint);
    procedure alBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
    procedure alBufferiv(Bid: ALuint; Param: ALenum; const Values: PALint);

    procedure alGetBufferf(Bid: ALuint; Param: ALenum; Value: PALfloat);
    procedure alGetBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
    procedure alGetBufferfv(Bid: ALuint; Param: ALenum; Values: PALfloat);
    procedure alGetBufferi(Bid: ALuint; Param: ALenum; Value: PALint);
    procedure alGetBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
    procedure alGetBufferiv(Bid: ALuint; Param: ALenum; Values: PALint);

    procedure alDopplerFactor(Value: ALfloat);
    procedure alDopplerVelocity(Value: ALfloat);
    procedure alSpeedOfSound(Value: ALfloat);
    procedure alDistanceModel(DistanceModel: ALenum);

    function getContext: IOpenALContext; virtual;
  end;

type
  { TOpenALContext }

  TOpenALContext = class(TOpenAL, IOpenALContext)
  protected
    FALCCreateContext: TALCCreateContext;
    FALCMakeContextCurrent: TALCMakeContextCurrent;
    FALCProcessContext: TALCProcessContext;
    FALCSuspendContext: TALCSuspendContext;
    FALCDestroyContext: TALCDestroyContext;
    FALCGetCurrentContext: TALCGetCurrentContext;
    FALCGetContextsDevice: TALCGetContextsDevice;
    FALCOpenDevice: TALCOpenDevice;
    FALCCloseDevice: TALCCloseDevice;
    FALCGetError: TALCGetError;
    FALCIsExtensionPresent: TALCIsExtensionPresent;
    FALCGetProcAddress: TALCGetProcAddress;
    FALCGetEnumValue: TALCGetEnumValue;
    FALCGetString: TALCGetString;
    FALCGetIntegerv: TALCGetIntegerv;
    FALCCaptureOpenDevice: TALCCaptureOpenDevice;
    FALCCaptureCloseDevice: TALCCaptureCloseDevice;
    FALCCaptureStart: TALCCaptureStart;
    FALCCaptureStop: TALCCaptureStop;
    FALCCaptureSamples: TALCCaptureSamples;
  protected
    procedure LoadFunctions; override;
  public
    destructor Destroy; override;
    function alcCreateContext(var device: ALCdevice; attrlist: PALCint): PALCcontext;
    function alcMakeContextCurrent(context: PALCcontext): ALCboolean;
    procedure alcProcessContext(var context: ALCcontext);
    procedure alcSuspendContext(var context: ALCcontext);
    procedure alcDestroyContext(var context: ALCcontext);
    function alcGetCurrentContext: PALCcontext;
    function alcGetContextsDevice(var context: ALCcontext): PALCdevice;

    function alcOpenDevice(devicename: PALCchar): PALCdevice;
    function alcCloseDevice(var device: ALCdevice): ALCboolean;

    function alcGetError(var device: ALCdevice): ALCenum;

    function alcIsExtensionPresent(var device: ALCdevice; extname: PALCchar): ALCboolean;
    function alcGetProcAddress(var device: ALCdevice; funcname: PALCchar): Pointer;
    function alcGetEnumValue(var device: ALCdevice; enumname: PALCchar): ALCenum;

    function alcGetString(device: PALCdevice; param: ALCenum): PALCchar;
    procedure alcGetIntegerv(var device: ALCdevice; param: ALCenum; size: ALCsizei; values: PALCint);

    function alcCaptureOpenDevice(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice;
    function alcCaptureCloseDevice(var device: ALCdevice): ALCboolean;
    procedure alcCaptureStart(var device: ALCdevice);
    procedure alcCaptureStop(var device: ALCdevice);
    procedure alcCaptureSamples(var device: ALCdevice; buffer: PALCvoid; samples: ALCsizei);

    function getContext: IOpenALContext; override;

  end;

var
  singleton: IOpenAL = nil;

function getOpenAL: IOpenAL;
begin
  if singleton = nil then
  begin
    singleton := TOpenALContext.Create;
    TOpenALContext(singleton).LoadFunctions;
  end;
  Result := singleton;
end;

{ TOpenALContext }

procedure TOpenALContext.LoadFunctions;
begin
  inherited LoadFunctions;
  Pointer(FALCCreateContext) := GetProcAddress(FLibHandle, 'alcCreateContext');
  Pointer(FALCMakeContextCurrent) := GetProcAddress(FLibHandle, 'alcMakeContextCurrent');
  Pointer(FALCProcessContext) := GetProcAddress(FLibHandle, 'alcProcessContext');
  Pointer(FALCSuspendContext) := GetProcAddress(FLibHandle, 'alcSuspendContext');
  Pointer(FALCDestroyContext) := GetProcAddress(FLibHandle, 'alcDestroyContext');
  Pointer(FALCGetCurrentContext) := GetProcAddress(FLibHandle, 'alcGetCurrentContext');
  Pointer(FALCGetContextsDevice) := GetProcAddress(FLibHandle, 'alcGetContextsDevice');

  Pointer(FALCOpenDevice) := GetProcAddress(FLibHandle, 'alcOpenDevice');
  Pointer(FALCCloseDevice) := GetProcAddress(FLibHandle, 'alcCloseDevice');
  Pointer(FALCGetError) := GetProcAddress(FLibHandle, 'alcGetError');

  Pointer(FALCIsExtensionPresent) := GetProcAddress(FLibHandle, 'alcIsExtensionPresent');
  Pointer(FALCGetProcAddress) := GetProcAddress(FLibHandle, 'alcGetProcAddress');
  Pointer(FALCGetEnumValue) := GetProcAddress(FLibHandle, 'alcGetEnumValue');
  Pointer(FALCGetString) := GetProcAddress(FLibHandle, 'alcGetString');
  Pointer(FALCGetIntegerv) := GetProcAddress(FLibHandle, 'alcGetIntegerv');

  Pointer(FALCCaptureOpenDevice) := GetProcAddress(FLibHandle, 'alcCaptureOpenDevice');
  Pointer(FALCCaptureCloseDevice) := GetProcAddress(FLibHandle, 'alcCaptureCloseDevice');
  Pointer(FALCCaptureStart) := GetProcAddress(FLibHandle, 'alcCaptureStart');
  Pointer(FALCCaptureStop) := GetProcAddress(FLibHandle, 'alcCaptureStop');
  Pointer(FALCCaptureSamples) := GetProcAddress(FLibHandle, 'alcCaptureSamples');
end;

destructor TOpenALContext.Destroy;
begin
  dynlibs.UnloadLibrary(FLibHandle);
  inherited Destroy;
end;

function TOpenALContext.alcCreateContext(var device: ALCdevice; attrlist: PALCint): PALCcontext;
begin
  if Assigned(FALCCreateContext) then
    Result := FALCCreateContext(@device, attrlist)
  else
    Result := nil;
end;

function TOpenALContext.alcMakeContextCurrent(context: PALCcontext): ALCboolean;
begin
  if Assigned(FALCMakeContextCurrent) then
    Result := FALCMakeContextCurrent(context)
  else
    Result := ALC_FALSE;
end;

procedure TOpenALContext.alcProcessContext(var context: ALCcontext);
begin
  if Assigned(FALCProcessContext) then
    FALCProcessContext(@context);
end;

procedure TOpenALContext.alcSuspendContext(var context: ALCcontext);
begin
  if Assigned(FALCSuspendContext) then
    FALCSuspendContext(@context);
end;

procedure TOpenALContext.alcDestroyContext(var context: ALCcontext);
begin
  if Assigned(FALCDestroyContext) then
    FALCDestroyContext(@context);
end;

function TOpenALContext.alcGetCurrentContext: PALCcontext;
begin
  if Assigned(FALCGetCurrentContext) then
    Result := FALCGetCurrentContext()
  else
    Result := nil;
end;

function TOpenALContext.alcGetContextsDevice(var context: ALCcontext): PALCdevice;
begin
  if Assigned(FALCGetContextsDevice) then
    Result := FALCGetContextsDevice(@context)
  else
    Result := nil;
end;

function TOpenALContext.alcOpenDevice(devicename: PALCchar): PALCdevice;
begin
  if Assigned(FALCOpenDevice) then
    Result := FALCOpenDevice(devicename)
  else
    Result := nil;
end;

function TOpenALContext.alcCloseDevice(var device: ALCdevice): ALCboolean;
begin
  if Assigned(FALCCloseDevice) then
    Result := FALCCloseDevice(@device)
  else
    Result := ALC_FALSE;
end;

function TOpenALContext.alcGetError(var device: ALCdevice): ALCenum;
begin
  if Assigned(FALCGetError) then
    Result := FALCGetError(@device)
  else
    Result := -1;
end;

function TOpenALContext.alcIsExtensionPresent(var device: ALCdevice; extname: PALCchar): ALCboolean;
begin
  if Assigned(FALCIsExtensionPresent) then
    Result := FALCIsExtensionPresent(@device, extname)
  else
    Result := ALC_FALSE;
end;

function TOpenALContext.alcGetProcAddress(var device: ALCdevice; funcname: PALCchar): Pointer;
begin
  if Assigned(FALCGetProcAddress) then
    Result := FALCGetProcAddress(@device, funcname)
  else
    Result := nil;
end;

function TOpenALContext.alcGetEnumValue(var device: ALCdevice; enumname: PALCchar): ALCenum;
begin
  if Assigned(FALCGetEnumValue) then
    Result := FALCGetEnumValue(@device, enumname)
  else
    Result := 0;
end;

function TOpenALContext.alcGetString(device: PALCdevice; param: ALCenum): PALCchar;
begin
  if Assigned(FALCGetString) then
    Result := FALCGetString(device, param)
  else
    Result := nil;
end;

procedure TOpenALContext.alcGetIntegerv(var device: ALCdevice; param: ALCenum; size: ALCsizei; values: PALCint);
begin
  if Assigned(FALCGetIntegerv) then
    FALCGetIntegerv(@device, param, size, values);
end;

function TOpenALContext.alcCaptureOpenDevice(const devicename: PALCchar; frequency: ALCuint; format: ALCenum; buffersize: ALCsizei): PALCdevice;
begin
  if Assigned(FALCCaptureOpenDevice) then
    Result := FALCCaptureOpenDevice(devicename, frequency, format, buffersize)
  else
    Result := nil;
end;

function TOpenALContext.alcCaptureCloseDevice(var device: ALCdevice): ALCboolean;
begin
  if Assigned(FALCCaptureCloseDevice) then
    Result := FALCCaptureCloseDevice(@device)
  else
    Result := ALC_FALSE;
end;

procedure TOpenALContext.alcCaptureStart(var device: ALCdevice);
begin
  if Assigned(FALCCaptureStart) then
    FALCCaptureStart(@device);
end;

procedure TOpenALContext.alcCaptureStop(var device: ALCdevice);
begin
  if Assigned(FALCCaptureStop) then
    FALCCaptureStop(@device);
end;

procedure TOpenALContext.alcCaptureSamples(var device: ALCdevice; buffer: PALCvoid; samples: ALCsizei);
begin
  if Assigned(FALCCaptureSamples) then
    FALCCaptureSamples(@device, buffer, samples);
end;

function TOpenALContext.getContext: IOpenALContext;
begin
  Result := self as IOpenALContext;
end;


{ TOpenAL }

constructor TOpenAL.Create;
var
  LibName: string;
begin
  inherited Create;
  LoadFunctions;
end;

destructor TOpenAL.Destroy;
begin
  if FLibHandle <> NilHandle then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle := NilHandle;
  end;
  inherited Destroy;
end;

procedure TOpenAL.LoadLibrary;
begin
  FLibHandle := dynlibs.LoadLibrary(DefaultLib);
  if FLibHandle = NilHandle then
    raise Exception.Create('Impossibile caricare OpenAL: ' + DefaultLib);
end;

procedure TOpenAL.LoadFunctions;
begin
  Pointer(FEnable) := dynlibs.GetProcAddress(FLibHandle, 'alEnable');
  Pointer(FDisable) := dynlibs.GetProcAddress(FLibHandle, 'alDisable');
  Pointer(FIsEnabled) := dynlibs.GetProcAddress(FLibHandle, 'alIsEnabled');
  Pointer(FGetString) := dynlibs.GetProcAddress(FLibHandle, 'alGetString');
  Pointer(FGetBooleanv) := dynlibs.GetProcAddress(FLibHandle, 'alGetBooleanv');
  Pointer(FGetIntegerv) := dynlibs.GetProcAddress(FLibHandle, 'alGetIntegerv');
  Pointer(FGetFloatv) := dynlibs.GetProcAddress(FLibHandle, 'alGetFloatv');
  Pointer(FGetDoublev) := dynlibs.GetProcAddress(FLibHandle, 'alGetDoublev');
  Pointer(FGetBoolean) := dynlibs.GetProcAddress(FLibHandle, 'alGetBoolean');
  Pointer(FGetInteger) := dynlibs.GetProcAddress(FLibHandle, 'alGetInteger');
  Pointer(FGetFloat) := dynlibs.GetProcAddress(FLibHandle, 'alGetFloat');
  Pointer(FGetDouble) := dynlibs.GetProcAddress(FLibHandle, 'alGetDouble');
  Pointer(FGetError) := dynlibs.GetProcAddress(FLibHandle, 'alGetError');
  Pointer(FIsExtensionPresent) := dynlibs.GetProcAddress(FLibHandle, 'alIsExtensionPresent');
  Pointer(FGetProcAddress) := dynlibs.GetProcAddress(FLibHandle, 'alGetProcAddress');
  Pointer(FGetEnumValue) := dynlibs.GetProcAddress(FLibHandle, 'alGetEnumValue');

  Pointer(FListenerf) := dynlibs.GetProcAddress(FLibHandle, 'alListenerf');
  Pointer(FListener3f) := dynlibs.GetProcAddress(FLibHandle, 'alListener3f');
  Pointer(FListenerfv) := dynlibs.GetProcAddress(FLibHandle, 'alListenerfv');
  Pointer(FListeneri) := dynlibs.GetProcAddress(FLibHandle, 'alListeneri');
  Pointer(FListener3i) := dynlibs.GetProcAddress(FLibHandle, 'alListener3i');
  Pointer(FListeneriv) := dynlibs.GetProcAddress(FLibHandle, 'alListeneriv');
  Pointer(FGetListenerf) := dynlibs.GetProcAddress(FLibHandle, 'alGetListenerf');
  Pointer(FGetListener3f) := dynlibs.GetProcAddress(FLibHandle, 'alGetListener3f');
  Pointer(FGetListenerfv) := dynlibs.GetProcAddress(FLibHandle, 'alGetListenerfv');
  Pointer(FGetListeneri) := dynlibs.GetProcAddress(FLibHandle, 'alGetListeneri');
  Pointer(FGetListener3i) := dynlibs.GetProcAddress(FLibHandle, 'alGetListener3i');
  Pointer(FGetListeneriv) := dynlibs.GetProcAddress(FLibHandle, 'alGetListeneriv');

  Pointer(FGenSources) := dynlibs.GetProcAddress(FLibHandle, 'alGenSources');
  Pointer(FDeleteSources) := dynlibs.GetProcAddress(FLibHandle, 'alDeleteSources');
  Pointer(FIsSource) := dynlibs.GetProcAddress(FLibHandle, 'alIsSource');
  Pointer(FSourcef) := dynlibs.GetProcAddress(FLibHandle, 'alSourcef');
  Pointer(FSource3f) := dynlibs.GetProcAddress(FLibHandle, 'alSource3f');
  Pointer(FSourcefv) := dynlibs.GetProcAddress(FLibHandle, 'alSourcefv');
  Pointer(FSourcei) := dynlibs.GetProcAddress(FLibHandle, 'alSourcei');
  Pointer(FSource3i) := dynlibs.GetProcAddress(FLibHandle, 'alSource3i');
  Pointer(FSourceiv) := dynlibs.GetProcAddress(FLibHandle, 'alSourceiv');
  Pointer(FGetSourcef) := dynlibs.GetProcAddress(FLibHandle, 'alGetSourcef');
  Pointer(FGetSource3f) := dynlibs.GetProcAddress(FLibHandle, 'alGetSource3f');
  Pointer(FGetSourcefv) := dynlibs.GetProcAddress(FLibHandle, 'alGetSourcefv');
  Pointer(FGetSourcei) := dynlibs.GetProcAddress(FLibHandle, 'alGetSourcei');
  Pointer(FGetSource3i) := dynlibs.GetProcAddress(FLibHandle, 'alGetSource3i');
  Pointer(FGetSourceiv) := dynlibs.GetProcAddress(FLibHandle, 'alGetSourceiv');

  Pointer(FSourcePlayv) := dynlibs.GetProcAddress(FLibHandle, 'alSourcePlayv');
  Pointer(FSourceStopv) := dynlibs.GetProcAddress(FLibHandle, 'alSourceStopv');
  Pointer(FSourceRewindv) := dynlibs.GetProcAddress(FLibHandle, 'alSourceRewindv');
  Pointer(FSourcePausev) := dynlibs.GetProcAddress(FLibHandle, 'alSourcePausev');

  Pointer(FSourcePlay) := dynlibs.GetProcAddress(FLibHandle, 'alSourcePlay');
  Pointer(FSourceStop) := dynlibs.GetProcAddress(FLibHandle, 'alSourceStop');
  Pointer(FSourceRewind) := dynlibs.GetProcAddress(FLibHandle, 'alSourceRewind');
  Pointer(FSourcePause) := dynlibs.GetProcAddress(FLibHandle, 'alSourcePause');

  Pointer(FSourceQueueBuffers) := dynlibs.GetProcAddress(FLibHandle, 'alSourceQueueBuffers');
  Pointer(FSourceUnqueueBuffers) := dynlibs.GetProcAddress(FLibHandle, 'alSourceUnqueueBuffers');

  Pointer(FGenBuffers) := dynlibs.GetProcAddress(FLibHandle, 'alGenBuffers');
  Pointer(FDeleteBuffers) := dynlibs.GetProcAddress(FLibHandle, 'alDeleteBuffers');
  Pointer(FIsBuffer) := dynlibs.GetProcAddress(FLibHandle, 'alIsBuffer');
  Pointer(FBufferData) := dynlibs.GetProcAddress(FLibHandle, 'alBufferData');

  Pointer(FBufferf) := dynlibs.GetProcAddress(FLibHandle, 'alBufferf');
  Pointer(FBuffer3f) := dynlibs.GetProcAddress(FLibHandle, 'alBuffer3f');
  Pointer(FBufferfv) := dynlibs.GetProcAddress(FLibHandle, 'alBufferfv');
  Pointer(FBufferi) := dynlibs.GetProcAddress(FLibHandle, 'alBufferi');
  Pointer(FBuffer3i) := dynlibs.GetProcAddress(FLibHandle, 'alBuffer3i');
  Pointer(FBufferiv) := dynlibs.GetProcAddress(FLibHandle, 'alBufferiv');

  Pointer(FGetBufferf) := dynlibs.GetProcAddress(FLibHandle, 'alGetBufferf');
  Pointer(FGetBuffer3f) := dynlibs.GetProcAddress(FLibHandle, 'alGetBuffer3f');
  Pointer(FGetBufferfv) := dynlibs.GetProcAddress(FLibHandle, 'alGetBufferfv');
  Pointer(FGetBufferi) := dynlibs.GetProcAddress(FLibHandle, 'alGetBufferi');
  Pointer(FGetBuffer3i) := dynlibs.GetProcAddress(FLibHandle, 'alGetBuffer3i');
  Pointer(FGetBufferiv) := dynlibs.GetProcAddress(FLibHandle, 'alGetBufferiv');

  Pointer(FDopplerFactor) := dynlibs.GetProcAddress(FLibHandle, 'alDopplerFactor');
  Pointer(FDopplerVelocity) := dynlibs.GetProcAddress(FLibHandle, 'alDopplerVelocity');
  Pointer(FSpeedOfSound) := dynlibs.GetProcAddress(FLibHandle, 'alSpeedOfSound');
  Pointer(FDistanceModel) := dynlibs.GetProcAddress(FLibHandle, 'alDistanceModel');
end;

procedure TOpenAL.alEnable(Capability: ALenum);
begin
  if Assigned(FEnable) then FEnable(Capability);
end;

procedure TOpenAL.alDisable(Capability: ALenum);
begin
  if Assigned(FDisable) then FDisable(Capability);
end;

function TOpenAL.alIsEnabled(Capability: ALenum): ALboolean;
begin
  if Assigned(FIsEnabled) then
    Result := FIsEnabled(Capability)
  else
    Result := AL_FALSE;
end;

function TOpenAL.alGetString(Param: ALenum): pansichar;
begin
  if Assigned(FGetString) then
    Result := FGetString(Param)
  else
    Result := nil;
end;

procedure TOpenAL.alGetBooleanv(Param: ALenum; Data: PALboolean);
begin
  if Assigned(FGetBooleanv) then FGetBooleanv(Param, Data);
end;

procedure TOpenAL.alGetIntegerv(Param: ALenum; Data: PALint);
begin
  if Assigned(FGetIntegerv) then FGetIntegerv(Param, Data);
end;

procedure TOpenAL.alGetFloatv(Param: ALenum; Data: PALfloat);
begin
  if Assigned(FGetFloatv) then FGetFloatv(Param, Data);
end;

procedure TOpenAL.alGetDoublev(Param: ALenum; Data: PALdouble);
begin
  if Assigned(FGetDoublev) then FGetDoublev(Param, Data);
end;

function TOpenAL.alGetBoolean(Param: ALenum): ALboolean;
begin
  if Assigned(FGetBoolean) then
    Result := FGetBoolean(Param)
  else
    Result := AL_FALSE;
end;

function TOpenAL.alGetInteger(Param: ALenum): ALint;
begin
  if Assigned(FGetInteger) then
    Result := FGetInteger(Param)
  else
    Result := 0;
end;

function TOpenAL.alGetFloat(Param: ALenum): ALfloat;
begin
  if Assigned(FGetFloat) then
    Result := FGetFloat(Param)
  else
    Result := 0.0;
end;

function TOpenAL.alGetDouble(Param: ALenum): ALdouble;
begin
  if Assigned(FGetDouble) then
    Result := FGetDouble(Param)
  else
    Result := 0.0;
end;

function TOpenAL.alGetError: ALenum;
begin
  if Assigned(FGetError) then
    Result := FGetError()
  else
    Result := AL_INVALID_OPERATION;
end;

function TOpenAL.alIsExtensionPresent(const ExtName: pansichar): ALboolean;
begin
  if Assigned(FIsExtensionPresent) then
    Result := FIsExtensionPresent(ExtName)
  else
    Result := AL_FALSE;
end;

function TOpenAL.alGetProcAddress(const FName: pansichar): Pointer;
begin
  if Assigned(FGetProcAddress) then
    Result := FGetProcAddress(FName)
  else
    Result := nil;
end;

function TOpenAL.alGetEnumValue(const EName: pansichar): ALenum;
begin
  if Assigned(FGetEnumValue) then
    Result := FGetEnumValue(EName)
  else
    Result := 0;
end;

procedure TOpenAL.alListenerf(Param: ALenum; Value: ALfloat);
begin
  if Assigned(FListenerf) then FListenerf(Param, Value);
end;

procedure TOpenAL.alListener3f(Param: ALenum; Value1, Value2, Value3: ALfloat);
begin
  if Assigned(FListener3f) then FListener3f(Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alListenerfv(Param: ALenum; const Values: PALfloat);
begin
  if Assigned(FListenerfv) then FListenerfv(Param, Values);
end;

procedure TOpenAL.alListeneri(Param: ALenum; Value: ALint);
begin
  if Assigned(FListeneri) then FListeneri(Param, Value);
end;

procedure TOpenAL.alListener3i(Param: ALenum; Value1, Value2, Value3: ALint);
begin
  if Assigned(FListener3i) then FListener3i(Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alListeneriv(Param: ALenum; const Values: PALint);
begin
  if Assigned(FListeneriv) then FListeneriv(Param, Values);
end;

procedure TOpenAL.alGetListenerf(Param: ALenum; Value: PALfloat);
begin
  if Assigned(FGetListenerf) then FGetListenerf(Param, Value);
end;

procedure TOpenAL.alGetListener3f(Param: ALenum; Value1, Value2, Value3: PALfloat);
begin
  if Assigned(FGetListener3f) then FGetListener3f(Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetListenerfv(Param: ALenum; Values: PALfloat);
begin
  if Assigned(FGetListenerfv) then FGetListenerfv(Param, Values);
end;

procedure TOpenAL.alGetListeneri(Param: ALenum; Value: PALint);
begin
  if Assigned(FGetListeneri) then FGetListeneri(Param, Value);
end;

procedure TOpenAL.alGetListener3i(Param: ALenum; Value1, Value2, Value3: PALint);
begin
  if Assigned(FGetListener3i) then FGetListener3i(Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetListeneriv(Param: ALenum; Values: PALint);
begin
  if Assigned(FGetListeneriv) then FGetListeneriv(Param, Values);
end;

procedure TOpenAL.alGenSources(N: ALsizei; Sources: PALuint);
begin
  if Assigned(FGenSources) then FGenSources(N, Sources);
end;

procedure TOpenAL.alDeleteSources(N: ALsizei; const Sources: PALuint);
begin
  if Assigned(FDeleteSources) then FDeleteSources(N, Sources);
end;

function TOpenAL.alIsSource(Sid: ALuint): ALboolean;
begin
  if Assigned(FIsSource) then
    Result := FIsSource(Sid)
  else
    Result := AL_FALSE;
end;

procedure TOpenAL.alSourcef(Sid: ALuint; Param: ALenum; Value: ALfloat);
begin
  if Assigned(FSourcef) then FSourcef(Sid, Param, Value);
end;

procedure TOpenAL.alSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
begin
  if Assigned(FSource3f) then FSource3f(Sid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alSourcefv(Sid: ALuint; Param: ALenum; const Values: PALfloat);
begin
  if Assigned(FSourcefv) then FSourcefv(Sid, Param, Values);
end;

procedure TOpenAL.alSourcei(Sid: ALuint; Param: ALenum; Value: ALint);
begin
  if Assigned(FSourcei) then FSourcei(Sid, Param, Value);
end;

procedure TOpenAL.alSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
begin
  if Assigned(FSource3i) then FSource3i(Sid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alSourceiv(Sid: ALuint; Param: ALenum; const Values: PALint);
begin
  if Assigned(FSourceiv) then FSourceiv(Sid, Param, Values);
end;

procedure TOpenAL.alGetSourcef(Sid: ALuint; Param: ALenum; Value: PALfloat);
begin
  if Assigned(FGetSourcef) then FGetSourcef(Sid, Param, Value);
end;

procedure TOpenAL.alGetSource3f(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
begin
  if Assigned(FGetSource3f) then FGetSource3f(Sid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetSourcefv(Sid: ALuint; Param: ALenum; Values: PALfloat);
begin
  if Assigned(FGetSourcefv) then FGetSourcefv(Sid, Param, Values);
end;

procedure TOpenAL.alGetSourcei(Sid: ALuint; Param: ALenum; Value: PALint);
begin
  if Assigned(FGetSourcei) then FGetSourcei(Sid, Param, Value);
end;

procedure TOpenAL.alGetSource3i(Sid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
begin
  if Assigned(FGetSource3i) then FGetSource3i(Sid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetSourceiv(Sid: ALuint; Param: ALenum; Values: PALint);
begin
  if Assigned(FGetSourceiv) then FGetSourceiv(Sid, Param, Values);
end;

procedure TOpenAL.alSourcePlayv(Ns: ALsizei; const Sids: PALuint);
begin
  if Assigned(FSourcePlayv) then FSourcePlayv(Ns, Sids);
end;

procedure TOpenAL.alSourceStopv(Ns: ALsizei; const Sids: PALuint);
begin
  if Assigned(FSourceStopv) then FSourceStopv(Ns, Sids);
end;

procedure TOpenAL.alSourceRewindv(Ns: ALsizei; const Sids: PALuint);
begin
  if Assigned(FSourceRewindv) then FSourceRewindv(Ns, Sids);
end;

procedure TOpenAL.alSourcePausev(Ns: ALsizei; const Sids: PALuint);
begin
  if Assigned(FSourcePausev) then FSourcePausev(Ns, Sids);
end;

procedure TOpenAL.alSourcePlay(Sid: ALuint);
begin
  if Assigned(FSourcePlay) then FSourcePlay(Sid);
end;

procedure TOpenAL.alSourceStop(Sid: ALuint);
begin
  if Assigned(FSourceStop) then FSourceStop(Sid);
end;

procedure TOpenAL.alSourceRewind(Sid: ALuint);
begin
  if Assigned(FSourceRewind) then FSourceRewind(Sid);
end;

procedure TOpenAL.alSourcePause(Sid: ALuint);
begin
  if Assigned(FSourcePause) then FSourcePause(Sid);
end;

procedure TOpenAL.alSourceQueueBuffers(Sid: ALuint; NumEntries: ALsizei; const Bids: PALuint);
begin
  if Assigned(FSourceQueueBuffers) then FSourceQueueBuffers(Sid, NumEntries, Bids);
end;

procedure TOpenAL.alSourceUnqueueBuffers(Sid: ALuint; NumEntries: ALsizei; Bids: PALuint);
begin
  if Assigned(FSourceUnqueueBuffers) then FSourceUnqueueBuffers(Sid, NumEntries, Bids);
end;

procedure TOpenAL.alGenBuffers(N: ALsizei; Buffers: PALuint);
begin
  if Assigned(FGenBuffers) then FGenBuffers(N, Buffers);
end;

procedure TOpenAL.alDeleteBuffers(N: ALsizei; const Buffers: PALuint);
begin
  if Assigned(FDeleteBuffers) then FDeleteBuffers(N, Buffers);
end;

function TOpenAL.alIsBuffer(Bid: ALuint): ALboolean;
begin
  if Assigned(FIsBuffer) then
    Result := FIsBuffer(Bid)
  else
    Result := AL_FALSE;
end;

procedure TOpenAL.alBufferData(Bid: ALuint; Format: ALenum; const Data: Pointer; Size: ALsizei; Freq: ALsizei);
begin
  if Assigned(FBufferData) then FBufferData(Bid, Format, Data, Size, Freq);
end;

procedure TOpenAL.alBufferf(Bid: ALuint; Param: ALenum; Value: ALfloat);
begin
  if Assigned(FBufferf) then FBufferf(Bid, Param, Value);
end;

procedure TOpenAL.alBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALfloat);
begin
  if Assigned(FBuffer3f) then FBuffer3f(Bid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alBufferfv(Bid: ALuint; Param: ALenum; const Values: PALfloat);
begin
  if Assigned(FBufferfv) then FBufferfv(Bid, Param, Values);
end;

procedure TOpenAL.alBufferi(Bid: ALuint; Param: ALenum; Value: ALint);
begin
  if Assigned(FBufferi) then FBufferi(Bid, Param, Value);
end;

procedure TOpenAL.alBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: ALint);
begin
  if Assigned(FBuffer3i) then FBuffer3i(Bid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alBufferiv(Bid: ALuint; Param: ALenum; const Values: PALint);
begin
  if Assigned(FBufferiv) then FBufferiv(Bid, Param, Values);
end;

procedure TOpenAL.alGetBufferf(Bid: ALuint; Param: ALenum; Value: PALfloat);
begin
  if Assigned(FGetBufferf) then FGetBufferf(Bid, Param, Value);
end;

procedure TOpenAL.alGetBuffer3f(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALfloat);
begin
  if Assigned(FGetBuffer3f) then FGetBuffer3f(Bid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetBufferfv(Bid: ALuint; Param: ALenum; Values: PALfloat);
begin
  if Assigned(FGetBufferfv) then FGetBufferfv(Bid, Param, Values);
end;

procedure TOpenAL.alGetBufferi(Bid: ALuint; Param: ALenum; Value: PALint);
begin
  if Assigned(FGetBufferi) then FGetBufferi(Bid, Param, Value);
end;

procedure TOpenAL.alGetBuffer3i(Bid: ALuint; Param: ALenum; Value1, Value2, Value3: PALint);
begin
  if Assigned(FGetBuffer3i) then FGetBuffer3i(Bid, Param, Value1, Value2, Value3);
end;

procedure TOpenAL.alGetBufferiv(Bid: ALuint; Param: ALenum; Values: PALint);
begin
  if Assigned(FGetBufferiv) then FGetBufferiv(Bid, Param, Values);
end;

procedure TOpenAL.alDopplerFactor(Value: ALfloat);
begin
  if Assigned(FDopplerFactor) then FDopplerFactor(Value);
end;

procedure TOpenAL.alDopplerVelocity(Value: ALfloat);
begin
  if Assigned(FDopplerVelocity) then FDopplerVelocity(Value);
end;

procedure TOpenAL.alSpeedOfSound(Value: ALfloat);
begin
  if Assigned(FSpeedOfSound) then FSpeedOfSound(Value);
end;

procedure TOpenAL.alDistanceModel(DistanceModel: ALenum);
begin
  if Assigned(FDistanceModel) then FDistanceModel(DistanceModel);
end;

function TOpenAL.getContext: IOpenALContext;
begin
  raise EIntfCastError.Create('No IOpenALContext implementations');
end;

end.
