unit vulkan.tutorial.application;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, pax.glfw, pax.vulkan, fgl;

type
  THints = specialize TFPGMap<integer, integer>;

  { TVulkanTutorialApplication - Base astratta }

  TVulkanTutorialApplication = class
  private
    FCaption: string;
    FHeight: uint16;
    FWidth: uint16;
    FWindow: PGLFWwindow;
    FWindowHint: THints;
    procedure SetCaption(AValue: string);
    procedure SetHeight(AValue: uint16);
    procedure SetWidth(AValue: uint16);
  protected
    procedure initWindow; virtual;
    procedure initVulkan; virtual;
    procedure mainLoop; virtual;
    procedure cleanup; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure run;
  published
    property Width: uint16 read FWidth write SetWidth;
    property Height: uint16 read FHeight write SetHeight;
    property Caption: string read FCaption write SetCaption;
  end;

  TVulkanTutorialApplication00 = class(TVulkanTutorialApplication)
    constructor Create; override;
  end;

  TVulkanTutorialApplication01 = class(TVulkanTutorialApplication)
  protected
    FInstance: VkInstance;
    function createAppInfo: VkApplicationInfo; virtual;
    function createInstanceInfo(appInfo: VkApplicationInfo): VkInstanceCreateInfo; virtual;
    procedure CreateInstance; virtual;
    procedure initVulkan; override;
    procedure cleanup; override;
  public
    constructor Create; override;
  end;

  { TVulkanTutorialApplication02 }

  TVulkanTutorialApplication02 = class(TVulkanTutorialApplication01)
  protected
    FEnableValidationLayers: boolean;
    FDebugMessenger: VkDebugUtilsMessengerEXT;
    FDebugUtils: IVulkanDebugUtilsExtension;
  protected
    function GetRequiredExtensions: TStringArray; virtual;
    procedure SetupDebugMessenger; virtual;
    procedure SetEnableValidationLayers(AValue: boolean);
    function populateDebugMessengerCreateInfo: VkDebugUtilsMessengerCreateInfoEXT; virtual;
    procedure CreateInstance; override;
    procedure initVulkan; override;
    procedure cleanup; override;
  public
    constructor Create; override;
    property EnableValidationLayers: boolean read FEnableValidationLayers write SetEnableValidationLayers;
  end;

  TVulkanTutorialApplication03 = class(TVulkanTutorialApplication02)
  public
  type

    { TQueueFamilyIndices }

    TQueueFamilyIndices = record
      GraphicsFamily: integer;  // -1 se non trovato
      function IsComplete: boolean;
    end;
  private
    FPhysicalDevice: VkPhysicalDevice;
    function CheckValidationLayerSupport: boolean;
    function GetRequiredExtensions: TStringArray; override;
    procedure PickPhysicalDevice;
    function IsDeviceSuitable(Device: VkPhysicalDevice): boolean;
    function FindQueueFamilies(Device: VkPhysicalDevice): TQueueFamilyIndices;
  protected
    procedure CreateInstance; override;
    procedure initVulkan; override;
  public
    constructor Create; override;
  end;

  TVulkanTutorialApplication04 = class(TVulkanTutorialApplication03)
  private
    FDevice: VkDevice;
    FGraphicsQueue: VkQueue;
    procedure CreateLogicalDevice;
  protected
    procedure initVulkan; override;
    procedure cleanup; override;
  public
    constructor Create; override;
  end;

implementation

const
  VALIDATION_LAYER_NAME = 'VK_LAYER_KHRONOS_validation';

  { Debug callback globale }

function debugCallback(messageSeverity: VkDebugUtilsMessageSeverityFlagBitsEXT; messageTypes: VkDebugUtilsMessageTypeFlagsEXT; pCallbackData: PVkDebugUtilsMessengerCallbackDataEXT; pUserData: Pointer): VkBool32; cdecl;
begin
  if Assigned(pCallbackData) then
    WriteLn(ErrOutput, 'validation layer: ', pCallbackData^.pMessage)
  else
    WriteLn(ErrOutput, 'validation layer: (callback data missing)');
  Result := VK_FALSE;
end;

{ TVulkanTutorialApplication }

procedure TVulkanTutorialApplication.SetCaption(AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
end;

procedure TVulkanTutorialApplication.SetHeight(AValue: uint16);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
end;

procedure TVulkanTutorialApplication.SetWidth(AValue: uint16);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
end;

procedure TVulkanTutorialApplication.initWindow;
var
  idx, key, Value: integer;
begin
  with getGLFW do
  begin
    glfwInit;
    for idx := 0 to FWindowHint.Count - 1 do
    begin
      key := FWindowHint.Keys[idx];
      Value := FWindowHint.KeyData[key];
      glfwWindowHint(key, Value);
    end;
    FWindow := glfwCreateWindow(Width, Height, PChar(Caption), nil, nil);
    if FWindow = nil then
      raise Exception.Create('Failed to create GLFW window');
  end;
end;

procedure TVulkanTutorialApplication.initVulkan;
begin
  // Da sovrascrivere nelle classi derivate
end;

constructor TVulkanTutorialApplication.Create;
begin
  FWindow := nil;
  FWindowHint := THints.Create;
end;

destructor TVulkanTutorialApplication.Destroy;
begin
  FreeAndNil(FWindowHint);
  inherited Destroy;
end;

procedure TVulkanTutorialApplication.run;
begin
  initWindow;
  initVulkan;
  mainLoop;
  cleanup;
end;

procedure TVulkanTutorialApplication.mainLoop;
begin
  with getGLFW do
    while not glfwWindowShouldClose(FWindow) do
      glfwPollEvents;
end;

procedure TVulkanTutorialApplication.cleanup;
begin
  with getGLFW do
  begin
    if FWindow <> nil then
      glfwDestroyWindow(FWindow);
    glfwTerminate;
  end;
end;

{ TVulkanTutorialApplication00 }

constructor TVulkanTutorialApplication00.Create;
begin
  inherited Create;
  Caption := 'Tutorial 00 - Base Code';
end;

{ TVulkanTutorialApplication01 }

constructor TVulkanTutorialApplication01.Create;
begin
  inherited Create;
  Caption := 'Tutorial 01 - Instance Create';
  FWindowHint.AddOrSetData(GLFW_CLIENT_API, GLFW_NO_API);
  FWindowHint.AddOrSetData(GLFW_RESIZABLE, GLFW_FALSE);
end;

function TVulkanTutorialApplication01.createAppInfo: VkApplicationInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  Result.pApplicationName := 'Hello Triangle';
  Result.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
  Result.pEngineName := 'No Engine';
  Result.engineVersion := VK_MAKE_VERSION(1, 0, 0);
  Result.apiVersion := VK_API_VERSION_1_0;
end;

function TVulkanTutorialApplication01.createInstanceInfo(appInfo: VkApplicationInfo): VkInstanceCreateInfo;
var
  glfwExtensionCount: uint32 = 0;
  glfwExtensions: PPChar;
begin
  glfwExtensions := getGLFW.glfwGetRequiredInstanceExtensions(glfwExtensionCount);

  FillChar(Result, SizeOf(Result), 0);
  Result.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  Result.pApplicationInfo := @appInfo;
  Result.enabledExtensionCount := glfwExtensionCount;
  Result.ppEnabledExtensionNames := glfwExtensions;
  Result.enabledLayerCount := 0;
end;

procedure TVulkanTutorialApplication01.CreateInstance;
var
  appInfo: VkApplicationInfo;
  createInfo: VkInstanceCreateInfo;
begin
  appInfo := createAppInfo;
  createInfo := createInstanceInfo(appInfo);

  if getVulkan.vkCreateInstance(createInfo, nil, FInstance) <> VK_SUCCESS then
    raise Exception.Create('Failed to create Vulkan instance');
end;

procedure TVulkanTutorialApplication01.initVulkan;
begin
  inherited initVulkan;
  CreateInstance;
end;

procedure TVulkanTutorialApplication01.cleanup;
begin
  if FInstance <> VK_NULL_HANDLE then
    getVulkan.vkDestroyInstance(FInstance, nil);
  inherited cleanup;
end;

{ TVulkanTutorialApplication02 }

constructor TVulkanTutorialApplication02.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 02 - Validation Layers';
  FEnableValidationLayers := True;
end;

procedure TVulkanTutorialApplication02.SetEnableValidationLayers(AValue: boolean);
begin
  if FEnableValidationLayers = AValue then Exit;
  FEnableValidationLayers := AValue;
end;

function TVulkanTutorialApplication02.populateDebugMessengerCreateInfo: VkDebugUtilsMessengerCreateInfoEXT;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.sType := VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
  Result.messageSeverity :=
    VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
  Result.messageType :=
    VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
  Result.pfnUserCallback := @debugCallback;
end;

function TVulkanTutorialApplication02.GetRequiredExtensions: TStringArray;
var
  Count: uint32;
  exts: PPChar;
  i: integer;
begin
  exts := getGLFW.glfwGetRequiredInstanceExtensions(Count);
  SetLength(Result, Count + Ord(EnableValidationLayers));

  for i := 0 to Count - 1 do
    Result[i] := StrPas(exts[i]);

  if EnableValidationLayers then
    Result[Count] := VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
end;

procedure TVulkanTutorialApplication02.SetupDebugMessenger;
var
  createInfo: VkDebugUtilsMessengerCreateInfoEXT;
  res: VkResult;
begin
  if not FEnableValidationLayers then Exit;

  FDebugUtils := getVulkan.GetDebugUtilsExtension(FInstance);
  if FDebugUtils = nil then
    raise Exception.Create('Debug utils extension not available');

  createInfo := populateDebugMessengerCreateInfo;

  res := FDebugUtils.vkCreateDebugUtilsMessengerEXT(@createInfo, nil, @FDebugMessenger);

  if res <> VK_SUCCESS then
    raise Exception.Create('Failed to create debug messenger');
end;

procedure TVulkanTutorialApplication02.CreateInstance;
var
  appInfo: VkApplicationInfo;
  createInfo: VkInstanceCreateInfo;
  extensions: TStringArray;
  i: integer;
begin
  appInfo := createAppInfo;
  createInfo := inherited createInstanceInfo(appInfo);

  extensions := GetRequiredExtensions;
  createInfo.enabledExtensionCount := Length(extensions);
  if Length(extensions) > 0 then
    createInfo.ppEnabledExtensionNames := @extensions[0];

  if FEnableValidationLayers then
  begin
    createInfo.enabledLayerCount := 1;
    createInfo.ppEnabledLayerNames := @[VALIDATION_LAYER_NAME];
  end;

  if getVulkan.vkCreateInstance(createInfo, nil, FInstance) <> VK_SUCCESS then
    raise Exception.Create('Failed to create instance');
end;

procedure TVulkanTutorialApplication02.initVulkan;
begin
  inherited initVulkan;
  SetupDebugMessenger;
end;

procedure TVulkanTutorialApplication02.cleanup;
begin
  if FEnableValidationLayers and (FDebugUtils <> nil) then
  begin
    FDebugUtils.vkDestroyDebugUtilsMessengerEXT(FDebugMessenger, nil);
    FillChar(FDebugMessenger, SizeOf(FDebugMessenger), 0);
  end;

  inherited cleanup;
end;

{ TVulkanTutorialApplication03 }

constructor TVulkanTutorialApplication03.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 03 - Physical Device';
  FPhysicalDevice := VK_NULL_HANDLE;
end;

function TVulkanTutorialApplication03.CheckValidationLayerSupport: boolean;
var
  layerCount: uint32;
  layers: array of VkLayerProperties;
  i: integer;
begin
  with getVulkan do
  begin
    vkEnumerateInstanceLayerProperties(@layerCount, nil);
    SetLength(layers, layerCount);
    vkEnumerateInstanceLayerProperties(@layerCount, @layers[0]);

    for i := 0 to High(layers) do
      if StrPas(layers[i].layerName) = VALIDATION_LAYER_NAME then
        Exit(True);
    Result := False;
  end;
end;

function TVulkanTutorialApplication03.GetRequiredExtensions: TStringArray;
var
  Count: uint32;
  exts: PPChar;
  i: integer;
begin
  exts := getGLFW.glfwGetRequiredInstanceExtensions(Count);
  SetLength(Result, Count + Ord(EnableValidationLayers));

  for i := 0 to Count - 1 do
    Result[i] := StrPas(exts[i]);

  if EnableValidationLayers then
    Result[Count] := VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
end;

procedure TVulkanTutorialApplication03.PickPhysicalDevice;
var
  Count: uint32;
  devices: array of VkPhysicalDevice;
  i: integer;
begin
  getVulkan.vkEnumeratePhysicalDevices(FInstance, @Count, nil);
  if Count = 0 then
    raise Exception.Create('No Vulkan-capable devices found');

  SetLength(devices, Count);
  getVulkan.vkEnumeratePhysicalDevices(FInstance, @Count, @devices[0]);

  for i := 0 to High(devices) do
    if IsDeviceSuitable(devices[i]) then
    begin
      FPhysicalDevice := devices[i];
      Break;
    end;

  if FPhysicalDevice = VK_NULL_HANDLE then
    raise Exception.Create('No suitable physical device found');
end;

function TVulkanTutorialApplication03.IsDeviceSuitable(Device: VkPhysicalDevice): boolean;
var
  indices: TQueueFamilyIndices;
begin
  indices := FindQueueFamilies(Device);
  Result := indices.IsComplete;
end;

function TVulkanTutorialApplication03.FindQueueFamilies(Device: VkPhysicalDevice): TQueueFamilyIndices;
var
  Count: uint32;
  families: array of VkQueueFamilyProperties;
  i: integer;
begin
  Result.GraphicsFamily := -1;

  getVulkan.vkGetPhysicalDeviceQueueFamilyProperties(Device, @Count, nil);
  SetLength(families, Count);
  getVulkan.vkGetPhysicalDeviceQueueFamilyProperties(Device, @Count, @families[0]);

  for i := 0 to High(families) do
    if (families[i].queueFlags and VK_QUEUE_GRAPHICS_BIT) <> 0 then
    begin
      Result.GraphicsFamily := i;
      Break;
    end;
end;

procedure TVulkanTutorialApplication03.CreateInstance;
begin
  inherited CreateInstance;
end;

procedure TVulkanTutorialApplication03.initVulkan;
begin
  inherited initVulkan;
  PickPhysicalDevice;
end;

{ TVulkanTutorialApplication03.TQueueFamilyIndices }

function TVulkanTutorialApplication03.TQueueFamilyIndices.IsComplete: boolean;
begin

end;

{ TVulkanTutorialApplication04 }

constructor TVulkanTutorialApplication04.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 04 - Logical Device';
  FDevice := VK_NULL_HANDLE;
  fillByte(FGraphicsQueue, SizeOf(FGraphicsQueue), 0);
end;

procedure TVulkanTutorialApplication04.CreateLogicalDevice;
var
  indices: TQueueFamilyIndices;
  queueInfo: VkDeviceQueueCreateInfo;
  features: VkPhysicalDeviceFeatures;
  deviceInfo: VkDeviceCreateInfo;
  priority: single = 1.0;
begin
  indices := FindQueueFamilies(FPhysicalDevice);

  FillChar(queueInfo, SizeOf(queueInfo), 0);
  queueInfo.sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  queueInfo.queueFamilyIndex := indices.GraphicsFamily;
  queueInfo.queueCount := 1;
  queueInfo.pQueuePriorities := @priority;

  FillChar(features, SizeOf(features), 0);

  FillChar(deviceInfo, SizeOf(deviceInfo), 0);
  deviceInfo.sType := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  deviceInfo.queueCreateInfoCount := 1;
  deviceInfo.pQueueCreateInfos := @queueInfo;
  deviceInfo.pEnabledFeatures := @features;

  if getVulkan.vkCreateDevice(FPhysicalDevice, @deviceInfo, nil, FDevice) <> VK_SUCCESS then
    raise Exception.Create('Failed to create logical device');

  getVulkan.vkGetDeviceQueue(FDevice, indices.GraphicsFamily, 0, @FGraphicsQueue);
end;

procedure TVulkanTutorialApplication04.initVulkan;
begin
  inherited initVulkan;
  CreateLogicalDevice;
end;

procedure TVulkanTutorialApplication04.cleanup;
begin
  if FDevice <> VK_NULL_HANDLE then
  begin
    getVulkan.vkDeviceWaitIdle(FDevice);
    getVulkan.vkDestroyDevice(FDevice, nil);
    FDevice := VK_NULL_HANDLE;
  end;

  inherited cleanup;
end;

end.
