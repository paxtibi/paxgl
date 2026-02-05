unit vulkan.tutorial.application04;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application03,
  pax.vulkan,
  pax.vulkan.helpers;

type
  { TVulkanApplicationTutorial04 }

  TVulkanApplicationTutorial04 = class(TVulkanApplicationTutorial03)
  protected
    FDevice: VkDevice;
    FGraphicsQueue: VkQueue;
    FQueuePriority: single;
    FFeatures: VkPhysicalDeviceFeatures;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateLogicalDevice;
  public
    constructor Create; override;
  end;

implementation

uses
  vulkan.tutorial.application02;

  { TVulkanApplicationTutorial04 }

constructor TVulkanApplicationTutorial04.Create;
begin
  getLogger.Enter('TVulkanApplicationTutorial04', 'Create');
  inherited Create;
  FDevice := VK_NULL_HANDLE;
  FGraphicsQueue := VK_NULL_HANDLE;
  FQueuePriority := 1.0;
  Caption := 'Vulkan Tutorial 04 - Logical Device and Queues';
  getLogger.Leave('TVulkanApplicationTutorial04', 'Create');
end;

procedure TVulkanApplicationTutorial04.CreateLogicalDevice;
var
  indices: TQueueFamilyIndices;
  queueCreateInfo: VkDeviceQueueCreateInfo;
  createInfo: VkDeviceCreateInfo;
begin
  getLogger.Enter('TVulkanApplicationTutorial04', 'CreateLogicalDevice');
  indices := findQueueFamilies(FPhysicalDevice);
  queueCreateInfo.zero;
  queueCreateInfo.sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  queueCreateInfo.queueFamilyIndex := indices.graphicsFamily;
  queueCreateInfo.queueCount := 1;
  queueCreateInfo.pQueuePriorities := @FQueuePriority;

  FFeatures.zero;

  createInfo.zero;
  createInfo.sType := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  createInfo.pQueueCreateInfos := @queueCreateInfo;
  createInfo.queueCreateInfoCount := 1;
  createInfo.pEnabledFeatures := @FFeatures;
  createInfo.enabledExtensionCount := 0;
  createInfo.enabledLayerCount := 0;

  with getVulkan do
  begin
    if vkCreateDevice(FPhysicalDevice, @createInfo, nil, FDevice) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare logical device');

    vkGetDeviceQueue(FDevice, indices.graphicsFamily, 0, @FGraphicsQueue);
  end;
  getLogger.Leave('TVulkanApplicationTutorial04', 'CreateLogicalDevice');
end;

procedure TVulkanApplicationTutorial04.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial04', 'InitVulkan');
  inherited InitVulkan;
  CreateLogicalDevice;
  getLogger.Leave('TVulkanApplicationTutorial04', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial04.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial04', 'Cleanup');
  with getVulkan do
  begin
    if FDevice <> VK_NULL_HANDLE then
      vkDestroyDevice(FDevice, nil);
  end;
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial04', 'Cleanup');
end;

end.
