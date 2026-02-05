unit vulkan.tutorial.application01;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial,
  vulkan.tutorial.application00,
  pax.glfw, pax.vulkan, pax.vulkan.helpers;

type

  { TVulkanApplicationTutorial01 }

  TVulkanApplicationTutorial01 = class(TVulkanApplicationTutorial00, IVulkanTutorial)
  protected
    FInstance: VkInstance;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateInstance; virtual;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial01 }

constructor TVulkanApplicationTutorial01.Create;
begin
  inherited Create;
  FInstance := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 01 - Instance';
end;

procedure TVulkanApplicationTutorial01.CreateInstance;
var
  appInfo: VkApplicationInfo;
  createInfo: VkInstanceCreateInfo;
  glfwExtCount: uint32;
  glfwExtensions: PPChar;
begin
  getLogger.Enter('TVulkanApplicationTutorial01', 'CreateInstance');

  with getVulkan do
  begin
    appInfo.zero;
    appInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
    appInfo.pApplicationName := PChar(Caption);
    appInfo.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
    appInfo.pEngineName := 'No Engine';
    appInfo.engineVersion := VK_MAKE_VERSION(1, 0, 0);
    appInfo.apiVersion := VK_API_VERSION_1_0;

    createInfo.zero;
    createInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    createInfo.pApplicationInfo := @appInfo;

    glfwExtensions := getGLFW.glfwGetRequiredInstanceExtensions(glfwExtCount);
    createInfo.enabledExtensionCount := glfwExtCount;
    createInfo.ppEnabledExtensionNames := glfwExtensions;

    createInfo.enabledLayerCount := 0;

    if vkCreateInstance(createInfo, nil, FInstance) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare l''istanza Vulkan');
  end;
  getLogger.Leave('TVulkanApplicationTutorial01', 'CreateInstance');
end;

procedure TVulkanApplicationTutorial01.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial01', 'InitVulkan');
  inherited InitVulkan;
  CreateInstance;
  getLogger.Leave('TVulkanApplicationTutorial01', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial01.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial01', 'Cleanup');
  with getVulkan do
  begin
    if FInstance <> VK_NULL_HANDLE then
      vkDestroyInstance(FInstance, nil);
  end;
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial01', 'Cleanup');
end;

end.
