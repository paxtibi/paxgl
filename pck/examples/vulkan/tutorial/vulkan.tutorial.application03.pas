unit vulkan.tutorial.application03;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial.application02;

type
  { TVulkanApplicationTutorial03 }

  TVulkanApplicationTutorial03 = class(TVulkanApplicationTutorial02)
  protected
    FPhysicalDevice: VkPhysicalDevice;
  protected
    function findQueueFamilies(device: VkPhysicalDevice): TQueueFamilyIndices;
    function isDeviceSuitable(device: VkPhysicalDevice): boolean;
    procedure InitVulkan; override;
    procedure PickPhysicalDevice; virtual;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial03 }

constructor TVulkanApplicationTutorial03.Create;
begin
  getLogger.Enter('TVulkanApplicationTutorial03', 'Create');
  inherited Create;
  FPhysicalDevice := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 03 - Physical Devices and Queue Families';
  getLogger.Leave('TVulkanApplicationTutorial03', 'Create');
end;

function TVulkanApplicationTutorial03.findQueueFamilies(device: VkPhysicalDevice): TQueueFamilyIndices;
var
  indices: TQueueFamilyIndices;
  queueFamilyCount: uint32 = 0;
  queueFamilies: array of VkQueueFamilyProperties;
  i: integer = 0;
begin
  getLogger.Enter('TVulkanApplicationTutorial03', 'findQueueFamilies');
  indices.graphicsFamily := -1;

  with getVulkan do
  begin
    vkGetPhysicalDeviceQueueFamilyProperties(device, @queueFamilyCount, nil);
    SetLength(queueFamilies, queueFamilyCount);
    vkGetPhysicalDeviceQueueFamilyProperties(device, @queueFamilyCount, @queueFamilies[0]);

    for i := 0 to High(queueFamilies) do
    begin
      if (queueFamilies[i].queueFlags and VK_QUEUE_GRAPHICS_BIT) <> 0 then
        indices.graphicsFamily := i;
      if indices.isComplete then Break;
    end;
  end;
  Result := indices;
  getLogger.Leave('TVulkanApplicationTutorial03', 'findQueueFamilies');
end;

function TVulkanApplicationTutorial03.isDeviceSuitable(device: VkPhysicalDevice): boolean;
var
  indices: TQueueFamilyIndices;
begin
  getLogger.Enter('TVulkanApplicationTutorial03', 'isDeviceSuitable');
  indices := findQueueFamilies(device);
  Result := indices.isComplete;
  getLogger.Leave('TVulkanApplicationTutorial03', 'isDeviceSuitable');
end;

procedure TVulkanApplicationTutorial03.PickPhysicalDevice;
var
  DeviceCount: uint32;
  Devices: array of VkPhysicalDevice;
  i: integer;
begin
  getLogger.Enter('TVulkanApplicationTutorial03', 'PickPhysicalDevice');
  with getVulkan do
  begin
    vkEnumeratePhysicalDevices(FInstance, @DeviceCount, nil);
    if DeviceCount = 0 then
      raise Exception.Create('Nessun dispositivo Vulkan trovato');

    SetLength(Devices, DeviceCount);
    vkEnumeratePhysicalDevices(FInstance, @DeviceCount, @Devices[0]);

    for i := 0 to High(Devices) do
      if IsDeviceSuitable(Devices[i]) then
      begin
        FPhysicalDevice := Devices[i];
        getLogger.Leave('TVulkanApplicationTutorial03', 'PickPhysicalDevice');
        Exit;
      end;
  end;
  getLogger.Leave('TVulkanApplicationTutorial03', 'PickPhysicalDevice exit in exception');
  raise Exception.Create('Nessun dispositivo Vulkan adatto trovato');
end;

procedure TVulkanApplicationTutorial03.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial03', 'InitVulkan');
  inherited InitVulkan;
  PickPhysicalDevice;
  getLogger.Leave('TVulkanApplicationTutorial03', 'InitVulkan');
end;

end.
