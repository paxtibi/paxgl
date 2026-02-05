unit vulkan.tutorial.application02;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application01,
  pax.glfw;

const
  VALIDATION_LAYER_NAME = 'VK_LAYER_KHRONOS_validation';

type
  TQueueFamilyIndices = record
    graphicsFamily: int32;
    presentFamily: int32;
    function isComplete(): boolean;
  end;

  { TVulkanApplicationTutorial02 }

  TVulkanApplicationTutorial02 = class(TVulkanApplicationTutorial01, IVulkanTutorial)
  protected
    FDebugMessenger: VkDebugUtilsMessengerEXT;
    FDebugUtils: IVulkanDebugUtilsExtension;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    function CheckValidationLayerSupport: boolean;
    procedure SetupDebugMessenger;
  public
    constructor Create; override;
  end;

implementation

function VulkanDebugCallback(messageSeverity: VkDebugUtilsMessageSeverityFlagBitsEXT; messageTypes: VkDebugUtilsMessageTypeFlagsEXT; pCallbackData: PVkDebugUtilsMessengerCallbackDataEXT; pUserData: Pointer): VkBool32; cdecl;
begin
  WriteLn(ErrOutput, 'validation layer: ', pCallbackData^.pMessage);
  Result := VK_FALSE;
end;

{ TQueueFamilyIndices }

function TQueueFamilyIndices.isComplete: boolean;
begin
  Result := graphicsFamily > -1;
end;

{ TVulkanApplicationTutorial02 }

constructor TVulkanApplicationTutorial02.Create;
begin
  inherited Create;
  FDebugMessenger := nil;
  Caption := 'Vulkan Tutorial 02 - Validation Layers';
  FInstanceExtensions.Add(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);
end;

function TVulkanApplicationTutorial02.CheckValidationLayerSupport: boolean;
var
  LayerCount: uint32;
  Layers: array of VkLayerProperties;
  i: integer;
begin
  getLogger.Enter('TVulkanApplicationTutorial02', 'CheckValidationLayerSupport');
  with getVulkan do
  begin
    vkEnumerateInstanceLayerProperties(@LayerCount, nil);
    SetLength(Layers, LayerCount);
    vkEnumerateInstanceLayerProperties(@LayerCount, @Layers[0]);

    for i := 0 to High(Layers) do
      if StrPas(Layers[i].layerName) = VALIDATION_LAYER_NAME then
        Exit(True);
  end;
  Result := False;
  getLogger.Leave('TVulkanApplicationTutorial02', 'CheckValidationLayerSupport');
end;

procedure TVulkanApplicationTutorial02.SetupDebugMessenger;
var
  createInfo: VkDebugUtilsMessengerCreateInfoEXT;
begin
  getLogger.Enter('TVulkanApplicationTutorial02', 'SetupDebugMessenger');
  if not EnableValidationLayers then Exit;

  FDebugUtils := getVulkan.GetDebugUtilsExtension(FInstance);
  if FDebugUtils = nil then
    raise Exception.Create('Estensione VK_EXT_debug_utils non disponibile');

  createInfo.zero;
  createInfo.sType := VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
  createInfo.messageSeverity := VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
  createInfo.messageType := VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT or VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
  createInfo.pfnUserCallback := @VulkanDebugCallback;

  if FDebugUtils.vkCreateDebugUtilsMessengerEXT(createInfo, nil, @FDebugMessenger) <> VK_SUCCESS then
    raise Exception.Create('Impossibile creare il debug messenger');
  getLogger.Leave('TVulkanApplicationTutorial02', 'SetupDebugMessenger');
end;

procedure TVulkanApplicationTutorial02.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial02', 'InitVulkan');
  inherited InitVulkan;
  if EnableValidationLayers and not CheckValidationLayerSupport then
    raise Exception.Create('Validation layers richiesti ma non disponibili');
  SetupDebugMessenger;
  getLogger.Leave('TVulkanApplicationTutorial02', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial02.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial02', 'Cleanup');
  if EnableValidationLayers and (FDebugMessenger <> 0) then
    FDebugUtils.vkDestroyDebugUtilsMessengerEXT(FDebugMessenger, nil);
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial02', 'Cleanup');
end;

end.
